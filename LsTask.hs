{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module LsTask (lsTask) where

import ClassyPrelude
import Data.List (transpose)
import Network.HTTP.Client
import RtmApi
import Rtm.Types

padding = 2 :: Int
screenWidth = 80 :: Int

lsTask :: Text -> [Text] -> RtmM ()
lsTask cmd args = do
  lists <- getListList
  case args of
       "-l":_ -> putStrLn . longFormatLists $ lists
       _      -> putStrLn . formatLists $ lists


longFormatLists :: [RtmList] -> Text
longFormatLists ls = unlines outLines
  where (names, ids, flgs) =
          unzip3 $ map tupl ls
        tupl l = (rtmListName l, flags l, rtmListId l)
        flags l = choice (rtmListSmart l) "s" " " ++
                  choice (rtmListDeleted l) "d" " " ++
                  choice (rtmListArchived l) "a" " " ++
                  choice (rtmListLocked l) "l" " "
        outLines = zipWith3 (\n i s -> n ++ "  " ++ i ++ "  " ++ s)
                   (sameLen names) (sameLen ids) flgs
        sameLen xs = flushLeft (maxLength xs) xs


flushLeft :: Int -> [Text] -> [Text]
flushLeft n xs = [ take n (x ++ replicate n ' ') | x <- xs ]


formatLists :: [RtmList] -> Text
formatLists ls =
  let names = sort $ fmap rtmListName ls
      colWidth = maxLength names + padding
      numCols = screenWidth `div` colWidth
      numPerCol = length names `funkyDiv` numCols
      columns = splitIntoPieces numPerCol names
      rows = map (concatMap (leftJ colWidth)) (transpose columns)
  in intercalate "\n" rows


choice :: Bool -> a -> a -> a
choice b y n = if b then y else n


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
