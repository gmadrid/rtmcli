{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module RtmApi (RtmConfig(..),
               RtmM,
               authUrl,
               getFrob,
               getListList,
               getToken,
              ) where

import ClassyPrelude
import Control.Monad.Except (ExceptT(..))
import Crypto.Hash.MD5
import Data.Aeson
import Data.Aeson.Types (parseEither)
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

methodGetFrob      = "rtm.auth.getFrob"
methodGetToken     = "rtm.auth.getToken"
methodListsGetList = "rtm.lists.getList"

apiKeyParam      = "api_key"
apiSigParam      = "api_sig"
authTokenParam   = "auth_token"
formatParam      = "format"
formatJsonValue  = "json"
frobParam        = "frob"
methodParam      = "method"
permsParam       = "perms"
permsDeleteValue = "delete"

authJsonKey  = "auth"
frobJsonKey  = "frob"
idJsonKey    = "id"
listJsonKey  = "list"
listsJsonKey = "lists"
nameJsonKey  = "name"
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


callAuthMethod :: FromJSON r => RtmConfig -> Manager -> ByteString -> Params -> RtmM r
callAuthMethod rc mgr m ps = callMethod rc mgr m ((authTokenParam, token rc) : ps)


callMethod :: FromJSON r => RtmConfig -> Manager -> ByteString -> Params -> RtmM r
callMethod rc mgr m ps = do
  let u = methodUrl rc m ps
  req <- parseUrl . C8.unpack $ u
  ExceptT $ withResponse req mgr readResponse
    where readResponse rsp = do
            -- {"rsp":{"stat":"ok","frob":"a5594c8ca48a0e33d44cd8edf8c56ec55728f4b0"}}
            c <- brRead $ responseBody rsp
            -- TODO: see if decodeStrictEither will get you a better err msg.
            let ejo = liftString $ eitherDecodeStrict c
            let ersp = checkJo ejo

            -- either checks for an err in the rsp.
            return $ either
              Left
              (liftString . parseEither parseJSON)
              ersp


liftString :: Either String b -> Either LByteString b
liftString (Left s)    = Left . fromString $ s
liftString (Right b) = Right b


-- Check the status of the response, return Right rsp if status is "ok".
-- Otherwise, return Left errmsg.
checkJo :: Either LByteString Value -> Either LByteString Value
checkJo (Right (Object o)) = do
  rspv <- maybeAsEither "'rsp' is missing from JSON result" (lookup "rsp" o)
  rsp <- grabObjectVal  "'rsp' is not Object in JSON result" rspv
  stv <- maybeAsEither "'stat' is missing from JSON result" (lookup "stat" rsp)
  st <- grabStringVal "'stat' is not Object in JSON result" stv
  if st == "ok"
    then Right rspv
    else responseErr rsp
checkJo (Left errmsg) = Left errmsg
checkJo _ = Left "Invalid JSON result is not Object"


-- {"rsp":{"stat":"fail","err":{"code":"100","msg":"Invalid API Key"}}}
responseErr :: Object -> Either LByteString Value
responseErr rsp = do
  errv <- maybeAsEither "'err' object is missing in error response" (lookup "err" rsp)
  err <- grabObjectVal "'err' is not Object in error response" errv
  let vals = mapMaybe (getStringVal err) ["code", "msg"]
  -- TODO: improve the way that this reports errors in absence of some piece.
  Left $ case vals of
   []   -> "Badly formed 'err' Object in error response"
   [v] -> fromChunks [ encodeUtf8 . mconcat $ ["Partially formed 'err' Object: ", v] ]
   vs   -> fromChunks [ encodeUtf8 . mconcat . intersperse ": " $ vs ]


maybeAsEither :: a -> Maybe b -> Either a b
maybeAsEither msg = maybe (Left msg) Right


grabObjectVal :: LByteString -> Value -> Either LByteString Object
grabObjectVal _ (Object o) = Right o
grabObjectVal msg _        = Left msg


grabStringVal :: LByteString -> Value -> Either LByteString Text
grabStringVal _ (String s) = Right s
grabStringVal msg _        = Left msg


getStringVal :: Object -> Text -> Maybe Text
getStringVal h k = do
  val <- lookup k h
  case val of
   (String t) -> Just t
   _          -> Nothing


getFrob :: RtmConfig -> Manager -> RtmM ByteString
getFrob rc mgr = do
  fr <- callMethod rc mgr methodGetFrob []
  return . fromString . rtmFrob $ fr


getToken :: RtmConfig -> Manager -> ByteString -> RtmM ByteString
getToken rc mgr f = do
  tk <- callMethod rc mgr methodGetToken [ (frobParam, f) ]
  return . fromString . trToken $ tk


getListList :: RtmConfig -> Manager -> RtmM [RtmList]
getListList rc mgr = do
  ll <- callAuthMethod rc mgr methodListsGetList []
  putStrLn $ tshow ll
  return . rtmListList $ ll


signParams :: ByteString -> Params -> ByteString
signParams secret ps = fromString dsp
  where cps = concat . (secret:) . map (uncurry (++)) $ sort ps
        hsh = Crypto.Hash.MD5.hash cps
        dsp = foldr (\w acc -> twoDigitHex w ++ acc) mempty hsh
        twoDigitHex w = drop (length s - 2) s
          where s = "00" ++ showHex w mempty



data RtmFrob = RtmFrob { rtmFrob :: String }
            deriving (Show)

instance FromJSON RtmFrob where
  parseJSON (Object rsp) = RtmFrob <$> (rsp .: frobJsonKey)
  parseJSON _ = mzero


data TokenResponse = TokenResponse {
  trToken :: String,
  trPerms :: String
  } deriving Show

instance FromJSON TokenResponse where
  parseJSON (Object rsp) = TokenResponse <$>
                           (auth >>= (.: tokenJsonKey)) <*>
                           (auth >>= (.: permsJsonKey))
    where auth = rsp .: authJsonKey
  parseJSON _ = mzero

data RtmListList = RtmListList {
  rtmListList :: [RtmList]
  } deriving Show

instance FromJSON RtmListList where
  parseJSON (Object rsp) = RtmListList <$>
                           (lsts >>= (.: listJsonKey))
    where lsts = rsp .: listsJsonKey
  parseJSON _ = mzero

data RtmList = RtmList {
  rtmListName :: Text,
  rtmListId :: Text
  } deriving Show

instance FromJSON RtmList where
  parseJSON (Object v) = RtmList <$>
                         (v .: nameJsonKey) <*>
                         (v .: idJsonKey)
  parseJSON _ = mzero
