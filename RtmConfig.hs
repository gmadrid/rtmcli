{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module RtmConfig (readConfig
                  ) where

import ClassyPrelude
import Control.Monad.Except (throwError)
import Data.Char
import Prelude (Read(..), read)
import RtmApi
import System.Directory

import qualified Prelude


readConfig :: RtmM RtmConfig
readConfig = do
  fn <- getFilename
  checkFile fn
  c <- getContents fn
  parseFile c


parseFile :: String -> RtmM RtmConfig
parseFile s = do
  let ps = span (/= '=') <$> lines s
  let cleanPs = fmap clean ps
  RtmConfig <$> 
    lookupConfig cleanPs "apiKey" <*>
    lookupConfig cleanPs "secret" <*>
    lookupConfig cleanPs "token"


clean :: (String, String) -> (String, String)
clean (a, '=':bs) = (trim a, trim bs)
                    where trim = f . f
                          f = reverse . dropWhile isSpace
-- Throw an error here.
clean (a, _) = (a, "MISSING")

lookupConfig :: [(String, String)] -> String -> RtmM ByteString
lookupConfig ps k =
  -- TODO: improve this error message
  maybe (throwError $ "Missing config parameter: " ++ fromString k)
    (return . fromString)
    (Prelude.lookup k ps)


getFilename :: RtmM String
getFilename = do
  hd <- liftIO getHomeDirectory
  return $ hd </> ".rtmcli2"
  

checkFile :: String -> RtmM ()
checkFile fn = return ()


getContents :: String -> RtmM String
getContents = readFile

