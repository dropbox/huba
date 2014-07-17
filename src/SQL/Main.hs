module Main where

import Shared.Thrift.Types
import Shared.Thrift.ClientInterface
import System.Environment
import Data.Maybe
import Data.List
import qualified Data.Vector as V
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text.Lazy as Text

import System.Console.Haskeline
import System.Exit


import SQL.Parser

main :: IO ()
main = do
    [serverName, portStr] <- getArgs
    let server = Server serverName (fromInteger $ read portStr)
    runInputT defaultSettings { historyFile = Just ".sqlhistory" } $ forever $ do
       mInput <- getInputLine "% "
       case mInput of
         Nothing -> liftIO exitSuccess
         (Just "") -> return ()
         (Just input) -> case sqlQuery input of
           Left err    -> outputStrLn $ show err
           Right query -> liftIO (runQuery server query) >>= outputStrLn


runQuery :: Server -> Query -> IO String
runQuery server query = do
    resp <- sendRootQuery server query
    return $ pShowResponse resp


pShowResponse :: Maybe QueryResponse -> String
pShowResponse Nothing = "No response."
pShowResponse (Just (QueryResponse code mMessage mRows)) = "Response code " ++ show code
                                                         ++ (fromMaybe "" $ (" | " ++) . Text.unpack <$> mMessage)
                                                         ++ ":"
                                                         ++ pShowRows mRows
  where pShowRows Nothing = ""
        pShowRows (Just rows) = ("\n" ++) $ intercalate "\n" $ map (("\t" ++) . pShowRow) $ V.toList rows
        pShowRow rs = show $ V.toList rs
