{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module RtmMonad (RtmConfig(..),
                 RtmM,
                 RtmEnv(..)
                )
       where

import ClassyPrelude
import Control.Monad.Except (ExceptT(..))
import Control.Monad.Reader (ReaderT(..))
import Data.Default
import Network.HTTP.Client (Manager)
import RtmArgs (Options)

data RtmConfig = RtmConfig {
  apiKey :: ByteString,
  secret :: ByteString,
  token  :: ByteString
  } deriving (Read, Show)

instance Default RtmConfig where
  def = RtmConfig "" "" ""


data RtmEnv = RtmEnv {
  envMgr :: Manager,
  envOpts :: Options,
  envConfig :: RtmConfig
  }


type RtmM = ReaderT RtmEnv (ExceptT LByteString IO)
