{-# LANGUAGE DeriveGeneric #-}

module ZeroBin (
  Expiration(..),
  pasteEc,
  share
) where

import Data.ByteString (ByteString)
import Data.ByteString.Base64 (encode)
import Data.Maybe (fromJust)
import GHC.Generics (Generic)
import ZeroBin.SJCL (encrypt, Content)
import ZeroBin.Utils (makePassword)
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP.Conduit as HTTP

pasteEc :: String
pasteEc = "https://paste.ec"

data Response = Response {
    status  :: String
  , message :: Maybe String
  , paste   :: Maybe String
  } deriving (Generic, Show)
instance JSON.FromJSON Response

data Expiration
  = Once 
  | Day
  | Week
  | Month
  | Never

instance Show Expiration where
  show Once  = "burn_after_reading"
  show Day   = "1_day"
  show Week  = "1_week"
  show Month = "1_month"
  show Never = "never"

post :: Expiration -> Content -> IO Response
post ex ct = do
  req' <- HTTP.parseUrl $ pasteEc ++ "/paste/create"
  let req = HTTP.urlEncodedBody
            [ (C.pack "expiration" , C.pack     $ show ex)
            , (C.pack "content"    , L.toStrict $ JSON.encode ct)
            ] req'
  manager <- HTTP.newManager HTTP.tlsManagerSettings
  response <- HTTP.httpLbs req manager
  return . fromJust . JSON.decode $ HTTP.responseBody response

share :: Expiration -> ByteString -> IO (Either String String)
share ex txt = do 
  pwd  <- makePassword 33
  c    <- encrypt pwd (encode txt)
  resp <- post ex c
  case status resp of
    "ok" -> return . Right $
            pasteEc ++ "/paste/" ++ (fromJust . paste) resp ++ "#" ++ pwd
    _    -> return . Left $
            (fromJust . message) resp

