{-# LANGUAGE OverloadedStrings #-}
module Main where

import Shared.Thrift.ClientInterface
import Shared.Thrift.Types
import Text.CSV
import Text.Read
import qualified Data.Vector as V
import qualified Data.HashMap.Lazy as M
import qualified Data.Text.Lazy as T

import Control.Applicative
import Control.Monad
import Data.Maybe


main :: IO ()
main = do
    Right csvList <- parseCSVFromFile "data/data.csv"
    let cols = take 2000 $ tail csvList
        colMsgs = V.fromList $ concatMap mkMessages cols
    -- print cols
    forM_ (chop 100 colMsgs) $ \msgs -> print =<< sendIngestorLog (Server "localhost" 8000) msgs

-- Series Name,"Series Code","Country Name","Country Code","2000 ","2001 ","2002 ","2003 ","2004 ","2005 ","2006 ","2007 ","2008 ","2009 ","2010 ","2011 ","2012 "


mkMessages :: [String] -> [LogMessage]
mkMessages (series: seriesCode: countryName: countryCode: years) = catMaybes $ map mkLog yearData
  where yearData = zip [2000..] $ map ((fmap $ fromIntegral . round . (* 10)) . (readMaybe :: String -> Maybe Double)) years
        mkLog (time, Just dat) = Just $ LogMessage time "wb" $ M.fromList [ ("co2", IntValue dat)
                                                              , ("seriesCode", StringValue $ T.pack seriesCode)
                                                              , ("series", StringValue $ T.pack series)
                                                              , ("countryName", StringValue $ T.pack countryName)
                                                              , ("countryCode", StringValue $ T.pack countryCode)
                                                              ]
        mkLog _  = Nothing


chop :: Int -> V.Vector a -> [V.Vector a]
chop _ xs | V.null xs = []
chop 0 _              = []
chop n xs             = slice : chop (n - 1) rest
  where (slice, rest) = V.splitAt sliceSize xs
        sliceSize = ceiling (fromIntegral (V.length xs) / fromIntegral n :: Double)

