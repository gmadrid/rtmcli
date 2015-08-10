{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module RtmApi (RtmConfig(..),
               RtmM,
               authUrl,
               getFrob,
               getToken,
              ) where

import ClassyPrelude
import Control.Monad.Except (ExceptT(..))
import Crypto.Hash.MD5
import Data.Aeson
import Network.HTTP.Client
import Network.HTTP.Types
import Numeric

import qualified Data.ByteString.Char8 as C8

type RtmM   = ExceptT LByteString IO
type Param  = (ByteString, ByteString)
type Params = [ Param ]

data RtmConfig = RtmConfig {
  apiKey :: ByteString,
  secret :: ByteString,
  token  :: ByteString
  } deriving (Read, Show)

authBaseUrl = "https://www.rememberthemilk.com/services/auth/"
baseUrl     = "https://api.rememberthemilk.com/services/rest/"

methodGetFrob  = "rtm.auth.getFrob"
methodGetToken = "rtm.auth.getToken"

apiKeyParam      = "api_key"
apiSigParam      = "api_sig"
formatParam      = "format"
formatJsonValue  = "json"
frobParam        = "frob"
methodParam      = "method"
permsParam       = "perms"
permsDeleteValue = "delete"

authJsonKey  = "auth"
frobJsonKey  = "frob"
permsJsonKey = "perms"
rspJsonKey   = "rsp"
tokenJsonKey = "token"


methodUrl :: RtmConfig -> ByteString -> Params -> ByteString
methodUrl r m ps = buildUrl r baseUrl $ [ (methodParam, m),
                                          (formatParam, formatJsonValue) ] ++ ps


authUrl :: RtmConfig -> ByteString -> ByteString
authUrl r f = buildUrl r authBaseUrl [ (permsParam, permsDeleteValue),
                                       (frobParam, f) ]


buildUrl :: RtmConfig -> ByteString -> Params -> ByteString
buildUrl r b ps = b ++ q
  where ps' = (apiKeyParam, apiKey r) : ps
        sig = signParams (secret r) ps'
        q = renderSimpleQuery True $ (apiSigParam, sig) : ps'


callMethod :: FromJSON r => RtmConfig -> Manager -> ByteString -> Params -> RtmM r
callMethod rc mgr m ps = do
  let u = methodUrl rc m ps
  req <- parseUrl . C8.unpack $ u
  ExceptT $ withResponse req mgr readResponse
    where readResponse rsp = do
            c <- brRead $ responseBody rsp
            let mj = decode $ fromStrict c
            return $ maybe
              (Left "Decode from JSON failed.")
              (\j -> Right j) mj


getFrob :: RtmConfig -> Manager -> RtmM ByteString
getFrob rc mgr = do
  fr <- callMethod rc mgr methodGetFrob []
  return . fromString . frob $ fr


getToken :: RtmConfig -> Manager -> ByteString -> RtmM ByteString
getToken rc mgr f = do
  tk <- callMethod rc mgr methodGetToken [ (frobParam, f) ]
  return . fromString . tokenToken $ tk


signParams :: ByteString -> Params -> ByteString
signParams secret ps = fromString dsp
  where cps = concat . (secret:) . map (\(a,b) -> a ++ b) $ sort ps
        hsh = Crypto.Hash.MD5.hash cps
        dsp = foldr (\w acc -> (twoDigitHex w) ++ acc) mempty hsh
        twoDigitHex w = drop ((length s) - 2) s
          where s = "00" ++ (showHex w mempty)
        


data Frob = Frob { frob :: String }
            deriving (Show)

instance FromJSON Frob where
  parseJSON (Object v) = Frob <$> (rsp >>= (.: frobJsonKey))
    where rsp = (v .: rspJsonKey)
  parseJSON _ = mzero
  

data TokenResp = TokenResp {
  tokenToken :: String,
  tokenPerms :: String
  } deriving Show

instance FromJSON TokenResp where
  parseJSON (Object v) = TokenResp <$>
                         (auth >>= (.: tokenJsonKey)) <*>
                         (auth >>= (.: permsJsonKey))
    where rsp = (v .: rspJsonKey)
          auth = (rsp >>= (.: authJsonKey))
  parseJSON _ = mzero
