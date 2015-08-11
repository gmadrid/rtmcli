{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import ClassyPrelude
import Control.Monad.Except (catchError, runExceptT, throwError)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Prelude (Read(..), read)
import RtmApi
import System.Directory
import System.Exit
import System.FilePath
import System.Process

import qualified Data.ByteString.Char8 as C8

workOrDie :: RtmM a -> RtmM a
workOrDie f = f `catchError` (\e -> do
                                 hPutStrLn stderr $ tshow e
                                 liftIO exitFailure)
  
readConfig :: RtmM RtmConfig
readConfig = do
  hd <- liftIO getHomeDirectory
  let fn = hd </> ".rtmcli"
  c <- readFile fn
  return . read $ c

ensureToken :: RtmConfig -> Manager -> RtmM RtmConfig
ensureToken rc mgr = do
  if token rc == mempty
    then acquireAndSaveToken rc mgr
    else return rc

acquireAndSaveToken :: RtmConfig -> Manager -> RtmM RtmConfig
acquireAndSaveToken rc mgr = do
  let msg = asText
            "\nYou have not authenticated to Remember The Milk.\n\
            \To proceed, a browser window will open. Follow the instructions in the\n\
            \browser, come back here, and hit the <Return> key.\n\n\
            \Type 'y' and hit <Return> to proceed."
  hPutStrLn stderr msg

  -- TODO: put y/n in a function of its own.
  y <- hGetLine stdin
  case y of
   ('y':_) -> return ()
   _ -> throwError "No token. User stopped authentication sequence."

  frob <- getFrob rc mgr

  let au = authUrl rc frob
  liftIO $ spawnCommand . C8.unpack $ "open '" ++ au ++ "'"
  hPutStrLn stderr $ asText "Hit <Return> when done with the browser."
  liftIO (getLine :: IO String)

  token <- getToken rc mgr frob
  let rc' = rc { token = token }

  hd <- liftIO getHomeDirectory
  let fn = hd </> ".rtmcli"
  
  writeFile fn (show rc')
  return rc'
  

-- setup needs to do several things:
-- 1. read in the config file.
-- 2. if the token is missing, do the auth step and save it out.
-- 3. if we have a token, then return the config.
setup :: Manager -> RtmM RtmConfig
setup mgr = do
  rc <- workOrDie readConfig
  workOrDie $ ensureToken rc mgr


showLists :: RtmConfig -> Manager -> RtmM ()
showLists rc m = do
  getListList rc m
  return ()


runEverything :: Manager -> RtmM ()
runEverything mgr = do
  rc <- setup mgr
  workOrDie $ showLists rc mgr


main = do
  mgr <- newManager tlsManagerSettings
  runExceptT $ runEverything mgr

  
