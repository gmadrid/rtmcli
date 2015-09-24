{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import ClassyPrelude
import Control.Monad.Except (catchError, runExceptT, throwError)
import Control.Monad.Reader (local, runReaderT, reader)
import Data.Default
import LsTask
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Prelude (Read(..), read)
import RtmApi
import RtmArgs
import RtmConfig
import Rtm.Types
import System.Console.Readline (addHistory, readline)
import System.Directory
import System.Exit
import System.FilePath
import System.Process

import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as LC8

workOrDie :: RtmM a -> RtmM a
workOrDie f = f `catchError` (\e -> do
<<<<<<< HEAD
                                 liftIO $ LC8.hPutStrLn stderr ("Error: " `mappend` e)
=======
                                 liftIO $ LC8.hPutStrLn stderr e
>>>>>>> 6043b708f77afb24322ea3b66ea305d57c2b000c
                                 liftIO exitFailure)

ensureToken :: RtmM RtmConfig
ensureToken = do
  rc <- reader envConfig
  if token rc == mempty
  then acquireAndSaveToken
  else return rc

askYorN :: IO () -> LByteString -> RtmM ()
askYorN f errMsg = do
  liftIO f
  y <- getLine
  case y of
   ('y':_) -> return ()
   _       -> throwError errMsg

acquireAndSaveToken :: RtmM RtmConfig
acquireAndSaveToken = do
  mgr <- reader envMgr
  rc <- reader envConfig
  let msg = asText
            "\nYou have not authenticated to Remember The Milk.\n\
            \To proceed, a browser window will open. Follow the instructions in the\n\
            \browser, come back here, and hit the <Return> key.\n\n\
            \Type 'y' and hit <Return> to proceed."

  q <- askYorN (hPutStrLn stderr msg) "No token. User stopped authentication sequence."
  frob <- getFrob

  let au = authUrl rc frob
  liftIO $ spawnCommand . C8.unpack $ "open '" ++ au ++ "'"
  hPutStrLn stderr $ asText "Hit <Return> when done with the browser."
  liftIO (getLine :: IO String)

  token <- getToken frob
  let rc' = rc { token = token }

  writeConfig rc'
  return rc'


-- setup needs to do several things:
-- 1. read in the config file.
-- 2. if the token is missing, do the auth step and save it out.
-- 3. if we have a token, then return the config.
setup :: RtmM RtmConfig
setup = do
  opts <- reader envOpts
  rc <- workOrDie readConfig
  tokenOk <- checkToken
  let rc' = if not tokenOk || optRefreshToken opts
            then rc { token = mempty }
            else rc
  local (\r -> r { envConfig = rc' })
    (workOrDie ensureToken)


checkToken :: RtmM Bool
checkToken = return True


processLine :: Text -> RtmM ()
processLine l = do
  rc <- reader envConfig
  mgr <- reader envMgr
  case words l of
   c@"ls" : args -> lsTask c args
   _             -> putStrLn "FOO"


loop :: RtmM ()
loop = do
  rc <- reader envConfig
  mgr <- reader envMgr
  maybeLine <- liftIO $ readline "rtm % "
  case maybeLine of
   Nothing     -> return () -- EOF / control-d
   Just "exit" -> return ()
   Just line   -> do liftIO $ addHistory line
                     processLine . fromString $ line
                     loop



processCommands :: [Text] -> RtmM ()
processCommands = mapM_ processLine


setupAndRun :: RtmM ()
setupAndRun = do
  rc <- setup
  local
    (\r -> r { envConfig = rc })
    runCommands


runCommands :: RtmM ()
runCommands = do
  cmds <- reader (optCommands . envOpts)
  if null cmds
    then loop
    else processCommands cmds


main = do
  (opts, _) <- parseOpts
  mgr <- newManager tlsManagerSettings
  let startEnv = RtmEnv { envConfig = def,
                          envMgr = mgr,
                          envOpts = opts }
  runExceptT (runReaderT setupAndRun startEnv)
