{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module RtmApi (RtmConfig(..),
               authUrl,
               getFrob,
               getToken,
              ) where

import ClassyPrelude
import Crypto.Hash.MD5
import Data.Aeson
import Network.HTTP.Client
import Network.HTTP.Types
import Numeric

import qualified Data.ByteString.Char8 as C8

data RtmConfig = RtmConfig {
  apiKey :: ByteString,
  secret :: ByteString,
  token  :: ByteString
  } deriving (Read, Show)

authBaseUrl = "https://www.rememberthemilk.com/services/auth/"
baseUrl = "https://api.rememberthemilk.com/services/rest/" :: ByteString

methodUrl :: RtmConfig -> ByteString -> [ (ByteString, ByteString) ] -> ByteString
methodUrl r m ps = buildUrl r baseUrl $ [ ("method", m),
                                          ("format", "json") ] ++ ps

authUrl :: RtmConfig -> ByteString -> ByteString
authUrl r f = buildUrl r authBaseUrl [ ("perms", "delete"),
                                       ("frob", f) ]

buildUrl :: RtmConfig -> ByteString -> [ (ByteString, ByteString) ] -> ByteString
buildUrl r b ps = b ++ q
  where ps' = ("api_key", apiKey r) : ps
        sig = signParams (secret r) ps'
        q = renderSimpleQuery True $ ("api_sig", sig) : ps'

callMethod :: FromJSON r => RtmConfig -> Manager -> ByteString -> [ (ByteString, ByteString) ] ->
              IO (Either ByteString r)
callMethod rc mgr m ps = do
  let u = methodUrl rc m ps
  req <- parseUrl $ C8.unpack u
  withResponse req mgr readResponse
    where readResponse rsp = do
            c <- brRead $ responseBody rsp
            let mj = decode $ fromStrict c
            return $ maybe (Left "Decode from JSON failed.")
              (\j -> Right j) mj


getFrob :: RtmConfig -> Manager -> IO (Either ByteString ByteString)
getFrob rc mgr = do
  efr <- callMethod rc mgr "rtm.auth.getFrob" []
  case efr of
   Right fr -> return . Right . fromString $ frob fr
   Left e -> return $ Left e


getFrob' :: RtmConfig -> Manager -> IO (Either ByteString ByteString)
getFrob' r mgr = do
  let u = methodUrl r "rtm.auth.getFrob" []
  req <- parseUrl $ C8.unpack u
  withResponse req mgr readResponse
    where readResponse rsp = do
            c <- brRead . responseBody $ rsp
            let mf = decode $ fromStrict c
            return $ maybe (Left "Decode failed in getFrob")
              (\f -> Right $ fromString $ frob f) mf

getToken :: RtmConfig -> Manager -> ByteString -> IO (Either ByteString ByteString)
getToken rc mgr f = do
  etk <- callMethod rc mgr "rtm.auth.getToken" [ ("frob", f) ]
  case etk of
   Right tk -> return . Right . fromString $ tokenToken tk
   Left e -> return $ Left e


getToken' :: RtmConfig -> Manager -> ByteString -> IO (Either ByteString ByteString)
getToken' r mgr f = do
  let u = methodUrl r "rtm.auth.getToken" [ ("frob", f) ]
  req <- parseUrl $ C8.unpack u
  withResponse req mgr readResponse
    where readResponse rsp = do
            c <- brRead . responseBody $ rsp
            let mt = decode $ fromStrict c
            return $ maybe (Left "Decode failed in getToken")
              (\t -> Right $ fromString $ tokenToken t) mt


twoDigitHex :: Word8 -> String
twoDigitHex w = drop ((length s) - 2) s
  where s = "00" ++ (showHex w "")

signParams :: ByteString -> [ ( ByteString, ByteString ) ] -> ByteString
signParams secret ps = fromString dsp
  where cps = concat . (secret:) . map (\(a,b) -> a ++ b) $ sort ps
        hsh = Crypto.Hash.MD5.hash cps
        dsp = foldr (\w acc -> (twoDigitHex w) ++ acc) "" hsh


data Frob = Frob { frob :: String }
            deriving (Show)

instance FromJSON Frob where
  parseJSON (Object v) = Frob <$> (rsp >>= (.: "frob"))
    where rsp = (v .: "rsp")
  parseJSON _ = mzero
  
data TokenResp = TokenResp {
  tokenToken :: String,
  tokenPerms :: String
  } deriving Show

instance FromJSON TokenResp where
  parseJSON (Object v) = TokenResp <$>
                         (auth >>= (.: "token")) <*>
                         (auth >>= (.: "perms"))
    where rsp = (v .: "rsp")
          auth = (rsp >>= (.: "auth"))
  parseJSON _ = mzero
