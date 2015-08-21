{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module RtmConfig (readConfig,
                  writeConfig
                  ) where

import ClassyPrelude
import Control.Monad.Except (MonadError, throwError)
import Data.Char
import Prelude (Read(..), read)
import RtmApi
import Rtm.Types
import System.Directory
import System.IO (withFile, IOMode(WriteMode))

import qualified Prelude


readConfig :: RtmM RtmConfig
readConfig = do
  fn <- getFilename
  checkFile fn
  c <- getContents fn
  parseFile c


writeConfig :: RtmConfig -> RtmM ()
writeConfig rc = do
  fn <- getFilename
  liftIO $ withFile fn WriteMode (\h -> do
                                     hPutStrLn h ("apiKey = " ++ apiKey rc)
                                     hPutStrLn h ("token = " ++ token rc)
                                     hPutStrLn h ("secret = " ++ secret rc))


parseFile :: (Monad m, MonadError LByteString m) => String -> m RtmConfig
parseFile s = do
  let ps = span (/= '=') <$> lines s
  cleanPs <- mapM clean ps
  RtmConfig <$>
    lookupConfig cleanPs "apiKey" <*>
    lookupConfig cleanPs "secret" <*>
    lookupConfig cleanPs "token"


clean :: (IsString e, MonadError e m) => (String, String) -> m (String, String)
clean (a, '=':bs) = return (trim a, trim bs)
                    where trim = f . f
                          f = reverse . dropWhile isSpace
clean (a, _) = throwError $ fromString ("Missing config value for '" ++ a ++ "'")

lookupConfig :: (MonadError LByteString m) => [(String, String)] -> String -> m ByteString
lookupConfig ps k =
  -- TODO: improve this error message
  maybe (throwError $ "Missing config parameter: " ++ fromString k)
    (return . fromString)
    (Prelude.lookup k ps)


getFilename :: MonadIO m => m String
getFilename = do
  hd <- liftIO getHomeDirectory
  return $ hd </> ".rtmcli"


checkFile :: MonadIO m => String -> m ()
checkFile fn = return ()


getContents :: MonadIO m => String -> m String
getContents = readFile
