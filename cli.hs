{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import ClassyPrelude
import Control.Monad.Except (runExceptT)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Prelude (Read(..), read)
import RtmApi
import System.Directory
import System.Exit
import System.FilePath
import System.Process

import qualified Data.ByteString.Char8 as C8

-- TODO: use this Monad
--type ProcessM = ExceptT LByteString IO

runCli :: RtmConfig -> Manager -> IO (Either ByteString ())
runCli r m = do
  return $ Right ()
  
workOrDie :: IO a -> IO a
workOrDie f = do
  e <- tryIOError f
  case e of
    Left e -> do
      hPutStrLn stderr $ tshow e
      exitFailure
    Right r ->
      return r

readConfig :: IO RtmConfig
readConfig = do
  hd <- getHomeDirectory
  let fn = hd </> ".rtmcli"
  c <- readFile fn
  return . read $ c

ensureToken :: RtmConfig -> Manager -> IO RtmConfig
ensureToken rc mgr = do
  if token rc == mempty
    then acquireAndSaveToken rc mgr
    else return rc

acquireAndSaveToken :: RtmConfig -> Manager -> IO RtmConfig
acquireAndSaveToken rc mgr = do
  -- TODO: put y/n in a function of its own.
  hPutStrLn stderr $ asText "You have not authenticated to Remember The Milk."
  hPutStrLn stderr $ asText "If you wish to proceed, a browser window will open."
  hPutStrLn stderr $ asText "When you have followed the instructions in the browser,"
  hPutStrLn stderr $ asText "return here and hit the <Return> key."
  hPutStrLn stderr $ asText "Do you wish to proceed? (Type 'y'.):"
  y <- hGetLine stdin
  case y of
   ('y':_) -> return ()
   _ -> ioError $ userError "No token. User stopped authentication sequence."

  -- TODO: get rid of this pattern match
  (Right frob) <- runExceptT $ getFrob rc mgr

  let au = authUrl rc frob
  spawnCommand . C8.unpack $ "open '" ++ au ++ "'"
  hPutStrLn stderr $ asText "Hit <Return> when done with the browser."
  getLine :: IO String

  -- TODO: get rid of this pattern match
  (Right token) <- runExceptT $ getToken rc mgr frob
  let rc' = rc { token = token }

  hd <- getHomeDirectory
  let fn = hd </> ".rtmcli"
  
  writeFile fn (show rc')
  return rc'
  

-- setup needs to do several things:
-- 1. read in the config file.
-- 2. if the token is missing, do the auth step and save it out.
-- 3. if we have a token, then return the config.
setup :: Manager -> IO RtmConfig
setup mgr = do
  config <- workOrDie readConfig
  workOrDie $ ensureToken config mgr

main = do
  mgr <- newManager tlsManagerSettings

  config <- setup mgr
  putStrLn $ tshow config
  
--  runCli config mgr

  return ()
  
  
  
