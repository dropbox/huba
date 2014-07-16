{-# LANGUAGE OverloadedStrings #-}
module SQL.Parser (sqlQuery) where

import Shared.Thrift.Types 
import qualified Shared.Thrift.Types as T
  
import Control.Lens
import Data.Functor
import qualified Data.Text.Lazy as Text
import qualified Data.HashSet as S
import qualified Data.Vector as V
import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)
import Data.List

import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.Maybe

-- SELECT column, column, ...,
--   aggregate(column), aggregate(column), ...
-- FROM table
-- WHERE time >= min-timestamp
--   AND time <= max-timestamp
--  [AND condition ...]
-- GROUP BY column, column, ...
-- ORDER BY aggregate(column)
-- LIMIT number

type TableName = Text.Text

aggregationFns :: [(String, AggregationFunction)]
aggregationFns = [ ("CONSTANT", CONSTANT)
                 , ("COUNT", COUNT)
                 , ("MIN", MIN)
                 , ("MAX", MAX)
                 , ("SUM", SUM)
                 , ("AVG", AVERAGE)
                 , ("SUM_PER_MINUTE", SUM_PER_MINUTE)
                 , ("HIST", HISTOGRAM)
                 ]

comprisonFns :: [(String, ComparisonFunction)]
comprisonFns = [ ("==", T.EQ)
               , ("!=", T.NEQ)
               , ("~=", T.REGEXP_EQ)
               , (">", T.GT)
               , ("<", T.LT)
               , (">=", T.GTE)
               , ("<=", T.LTE)
               ]

sqlQuery :: String -> Either ParseError Query
sqlQuery = parse parseSqlQuery ""

parseSqlQuery :: Parser Query
parseSqlQuery = do
    aggregations <- V.fromList <$> parseAggregations
    tableName <- parseTable
    conditions <- V.fromList <$> parseConditions
    let res = extractTime conditions
    unless (isJust res) $ fail "No start and end time specified"
    let Just (timeStart, timeEnd, conditions') = extractTime conditions
    groupBys <- optionMaybe $ V.fromList <$> parseGroupBys
    orderBy <- optionMaybe parseOrderBy
    limit <- parseLimit
    return $ Query aggregations
                   tableName
                   timeStart
                   timeEnd
                   (Just conditions')
                   groupBys
                   (orderBy >>= \o -> V.findIndex (== o) aggregations)
                   limit
  where extractTime cs = let (ts, other) = V.partition (^. cColumn . to (== "time")) cs
                             startTime   = getIntValue =<< view cValue <$> V.find (^. cComparisonFunction . to (== T.GTE)) ts
                             endTime     = getIntValue =<< view cValue <$> V.find (^. cComparisonFunction . to (== T.LTE)) ts in
                               (,,) <$> startTime <*> endTime <*> Just other
        getIntValue (IntValue n) = Just n
        getIntValue _            = Nothing


parseAggregations :: Parser [ColumnExpression]
parseAggregations = reserved "SELECT" >> commaSep1 parseAggregation

parseTable :: Parser TableName
parseTable = reserved "FROM" >> parseTableName
  where parseTableName = Text.pack <$> identifier <?> "table name"

parseConditions :: Parser [Condition]
parseConditions = reserved "WHERE" >> sepBy1 parseCondition (reserved "AND")
    where parseCondition = Condition <$> parseColumnName <*> parseComparisonFn <*> parseValue
          parseComparisonFn = choice (map (\(name, comp) -> comp <$ reservedOp name) $ comprisonFns)
          parseValue = choice [ StringValue . Text.pack <$> stringLiteral
                              , IntValue . fromInteger <$> integer
                              , StringSet . S.fromList . map Text.pack <$> braces (commaSep stringLiteral)
                              , StringVector . V.fromList . map Text.pack  <$> brackets (commaSep stringLiteral)
                              ]


parseGroupBys :: Parser [ColumnName]
parseGroupBys = reserved "GROUP" >> reserved "BY" >> commaSep1 parseColumnName

parseOrderBy :: Parser ColumnExpression
parseOrderBy = reserved "ORDER" >> reserved "BY" >> parseAggregation

parseLimit :: Parser Int
parseLimit = reserved "LIMIT" >> fromInteger <$> integer

parseAggregation :: Parser ColumnExpression
parseAggregation =  flip ColumnExpression CONSTANT <$> parseColumnName
                <|> flip ColumnExpression <$> parseAggFn <*> parens parseColumnName

parseAggFn :: Parser AggregationFunction
parseAggFn = choice (map (\(name, agg) -> agg <$ reserved name) aggregationFns)

parseColumnName :: Parser ColumnName
parseColumnName = Text.pack <$> identifier <?> "column name"

---------------
lexer = P.makeTokenParser emptyDef
  { P.commentStart = "/*"
  , P.commentEnd = "*/"
  , P.commentLine = "#"
  , P.identStart = letter
  , P.identLetter = alphaNum <|> oneOf "_-"
  , P.reservedNames = ["SELECT", "FROM", "WHERE", "AND", "GROUP", "ORDER", "BY", "LIMIT"] ++ map fst aggregationFns
  , P.opStart = oneOf $ map head $ map fst comprisonFns
  , P.opLetter = oneOf $ nub $ concatMap tail $ map fst comprisonFns
  , P.reservedOpNames = map fst comprisonFns
  , P.nestedComments = True
  , P.caseSensitive = False
  }

identifier    = P.identifier lexer
reserved      = P.reserved lexer
reservedOp    = P.reservedOp lexer
stringLiteral = P.stringLiteral lexer
integer       = P.integer lexer
decimal       = P.decimal lexer
whiteSpace    = P.whiteSpace lexer
parens        = P.parens lexer
brackets      = P.brackets lexer
braces        = P.braces lexer
commaSep      = P.commaSep lexer
commaSep1     = P.commaSep1 lexer

