{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module LsTask (lsTask) where

import ClassyPrelude
import Control.Monad
import Network.HTTP.Client
import RtmApi

screenWidth = 80 :: Int

lsTask :: RtmConfig -> Manager -> Text -> [Text] -> RtmM ()
lsTask rc mgr cmd args = do
  lists <- getListList rc mgr
  putStrLn . formatLists $ lists

formatLists :: [RtmList] -> Text
formatLists ls =
  let ns = sort $ fmap rtmListName ls
      w = maxLength ns + 2
      cols = screenWidth `div` w
      widens = fmap (\t -> take w (t `mappend` replicate w ' ')) ns
      pieces = splitIntoPieces ((length ns `div` cols) + 1) widens
  in intercalate "\n" $ joinAcross pieces


splitIntoPieces :: Int -> [a] -> [[a]]
splitIntoPieces _ [] = []
splitIntoPieces n xs = let (t, ts) = splitAt n xs in
                        t : splitIntoPieces n ts

joinAcross :: (Show a, Monoid a) => [[a]] -> [a]
joinAcross [] = []
joinAcross nss = let pairs = mapMaybe uncons nss
                     (ts, rests) = unzip pairs
                 in concat ts : joinAcross rests

maxLength :: [Text] -> Int
maxLength = foldr (\e acc -> max (length e) acc) 0
