{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import ClassyPrelude
import Control.Monad.Except (catchError, runExceptT, throwError)
import LsTask
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Prelude (Read(..), read)
import RtmApi
import RtmArgs
import RtmConfig
import System.Console.Readline (addHistory, readline)
import System.Directory
import System.Exit
import System.FilePath
import System.Process

import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as LC8

workOrDie :: RtmM a -> RtmM a
workOrDie f = f `catchError` (\e -> do
                                 liftIO $ LC8.hPutStrLn stderr e
                                 liftIO exitFailure)
  
ensureToken :: RtmConfig -> Manager -> RtmM RtmConfig
ensureToken rc mgr = if token rc == mempty
                     then acquireAndSaveToken rc mgr
                     else return rc

askYorN :: IO () -> LByteString -> RtmM ()
askYorN f errMsg = do
  liftIO f
  y <- getLine
  case y of
   ('y':_) -> return ()
   _       -> throwError errMsg

acquireAndSaveToken :: RtmConfig -> Manager -> RtmM RtmConfig
acquireAndSaveToken rc mgr = do
  let msg = asText
            "\nYou have not authenticated to Remember The Milk.\n\
            \To proceed, a browser window will open. Follow the instructions in the\n\
            \browser, come back here, and hit the <Return> key.\n\n\
            \Type 'y' and hit <Return> to proceed."

  q <- askYorN (hPutStrLn stderr msg) "No token. User stopped authentication sequence."
  frob <- getFrob rc mgr

  let au = authUrl rc frob
  liftIO $ spawnCommand . C8.unpack $ "open '" ++ au ++ "'"
  hPutStrLn stderr $ asText "Hit <Return> when done with the browser."
  liftIO (getLine :: IO String)

  token <- getToken rc mgr frob
  let rc' = rc { token = token }

  writeConfig rc'
  return rc'
  

-- setup needs to do several things:
-- 1. read in the config file.
-- 2. if the token is missing, do the auth step and save it out.
-- 3. if we have a token, then return the config.
setup :: Options -> Manager -> RtmM RtmConfig
setup opts mgr = do
  rc <- workOrDie readConfig
  tokenOk <- checkToken rc
  let rc' = if not tokenOk || optRefreshToken opts
            then rc { token = mempty }
            else rc
  workOrDie $ ensureToken rc' mgr


checkToken :: RtmConfig -> RtmM Bool
checkToken rc = return True


processLine :: RtmConfig -> Manager -> Text -> RtmM ()
processLine rc mgr l = case words l of
                        c@"ls" : args -> lsTask rc mgr c args
                        _             -> putStrLn "FOO"


loop :: RtmConfig -> Manager -> RtmM ()
loop rc mgr = do
  maybeLine <- liftIO $ readline "rtm % "
  case maybeLine of
   Nothing     -> return () -- EOF / control-d
   Just "exit" -> return ()
   Just line   -> do liftIO $ addHistory line
                     processLine rc mgr . fromString $ line
                     loop rc mgr



processCommands :: RtmConfig -> Manager -> [Text] -> RtmM ()
processCommands rc mgr cmds = mapM_ (processLine rc mgr) cmds 


runEverything :: Options -> Manager -> RtmM ()
runEverything opts mgr = do
  rc <- setup opts mgr
  if null $ optCommands opts
    then loop rc mgr
    else processCommands rc mgr $ optCommands opts


main = do
  (opts, _) <- parseOpts
  mgr <- newManager tlsManagerSettings
  runExceptT $ runEverything opts mgr

  
