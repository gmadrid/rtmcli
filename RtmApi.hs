{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module RtmApi (authUrl,

               getFrob,
               getListList,
               getToken,

               RtmList,
               rtmListArchived,
               rtmListDeleted,
               rtmListId,
               rtmListLocked,
               rtmListName,
               rtmListSmart
              ) where

import ClassyPrelude
import Control.Monad.Except (ExceptT(..))
import Control.Monad.Reader (reader)
import Crypto.Hash.MD5
import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import Network.HTTP.Client
import Network.HTTP.Types
import Numeric
import RtmArgs
import Rtm.Types

import qualified Data.ByteString.Char8 as C8

type Param  = (ByteString, ByteString)
type Params = [ Param ]

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

archivedJsonKey = "archived"
authJsonKey     = "auth"
deletedJsonKey  = "deleted"
frobJsonKey     = "frob"
idJsonKey       = "id"
listJsonKey     = "list"
listsJsonKey    = "lists"
lockedJsonKey   = "locked"
nameJsonKey     = "name"
permsJsonKey    = "perms"
rspJsonKey      = "rsp"
smartJsonKey    = "smart"
tokenJsonKey    = "token"


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


callAuthMethod :: FromJSON r => ByteString -> Params -> RtmM r
callAuthMethod m ps = do
  rc <- reader envConfig
  callMethod m ((authTokenParam, token rc) : ps)


callMethod :: FromJSON r => ByteString -> Params -> RtmM r
callMethod m ps = do
  rc <- reader envConfig
  mgr <- reader envMgr
  dump <- reader (optDumpJson . envOpts)
  let u = methodUrl rc m ps
  req <- parseUrl . C8.unpack $ u
  ReaderT (\r -> ExceptT $ withResponse req mgr (readResponse dump))
    where readResponse dump rsp = do
            -- {"rsp":{"stat":"ok","frob":"a5594c8ca48a0e33d44cd8edf8c56ec55728f4b0"}}
            c <- brRead $ responseBody rsp
            if dump then C8.hPutStr stderr c else return ()
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
   v:[] -> fromChunks [ encodeUtf8 . mconcat $ ["Partially formed 'err' Object: ", v] ]
   vs   -> fromChunks [ encodeUtf8 . mconcat . intersperse " - " $ vs ]


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


getFrob :: RtmM ByteString
getFrob = do
  fr <- callMethod methodGetFrob []
  return . fromString . rtmFrob $ fr


getToken :: ByteString -> RtmM ByteString
getToken f = do
  tk <- callMethod methodGetToken [ (frobParam, f) ]
  return . fromString . trToken $ tk


getListList :: RtmM [RtmList]
getListList = do
  ll <- callAuthMethod methodListsGetList []
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
  rtmListId :: Text,
  rtmListArchived :: Bool,
  rtmListDeleted :: Bool,
  rtmListLocked :: Bool,
  rtmListSmart :: Bool
  } deriving Show

instance FromJSON RtmList where
  parseJSON (Object v) = RtmList <$>
                         (v .: nameJsonKey) <*>
                         (v .: idJsonKey) <*>
                         (withStringBool False v archivedJsonKey) <*>
                         (withStringBool False v deletedJsonKey) <*>
                         (withStringBool False v lockedJsonKey) <*>
                         (withStringBool False v smartJsonKey)
  parseJSON _ = mzero

withStringBool :: Bool -> Object -> Text -> Parser Bool
withStringBool d o k = pure $ maybe False (s2b d) (lookup k o)
  where s2b d s
          | s == "0"  = False
          | s == "1"  = True
          | otherwise = d
