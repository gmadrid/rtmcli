{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module LsTask (lsTask) where

import ClassyPrelude
import Data.List (transpose)
import Network.HTTP.Client
import RtmApi
import RtmMonad

padding = 2 :: Int
screenWidth = 80 :: Int

lsTask :: Text -> [Text] -> RtmM ()
lsTask cmd args = do
  lists <- getListList
  putStrLn . formatLists $ lists

formatLists :: [RtmList] -> Text
formatLists ls =
  let names = sort $ fmap rtmListName ls
      colWidth = maxLength names + padding
      numCols = screenWidth `div` colWidth
      numPerCol = length names `funkyDiv` numCols
      columns = splitIntoPieces numPerCol names
      rows = map (concatMap (leftJ colWidth)) (transpose columns)
  in intercalate "\n" rows


funkyDiv :: Integral a => a -> a -> a
funkyDiv n d = (n `div` d) + if n `mod` d == 0 then 0 else 1


leftJ :: Int -> Text -> Text
leftJ n t = take n (t ++ replicate n ' ')


splitIntoPieces :: Int -> [a] -> [[a]]
splitIntoPieces _ [] = []
splitIntoPieces n xs = let (t, ts) = splitAt n xs in
                        t : splitIntoPieces n ts

maxLength :: [Text] -> Int
maxLength = foldr (\e acc -> max (length e) acc) 0
