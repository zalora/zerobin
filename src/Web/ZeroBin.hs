{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Web.ZeroBin (
  Expiration(..),
  ZeroBinError(..),
  share
) where

import Control.Exception (Exception)
import Control.Exception.Base (throwIO)
import Data.ByteString (ByteString)
import Data.ByteString.Base64 (encode)
import Data.Maybe (fromJust)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Web.ZeroBin.SJCL (encrypt, Content)
import Web.ZeroBin.Utils (makePassword)
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP.Conduit as HTTP

data Response = Response {
    status  :: String
  , message :: Maybe String
  , paste   :: Maybe String
  } deriving (Generic, Show)
instance JSON.FromJSON Response

data ZeroBinError = ZeroBinError String
  deriving (Show, Typeable)
instance Exception ZeroBinError

data Expiration
  = Once 
  | Day
  | Week
  | Month
  | Never

form :: Expiration -> String
form Once  = "burn_after_reading"
form Day   = "1_day"
form Week  = "1_week"
form Month = "1_month"
form Never = "never"

post :: String -> Expiration -> Content -> IO String
post bin ex ct = do
  req' <- HTTP.parseUrl $ bin ++ "/paste/create"
  let req = HTTP.urlEncodedBody
            [ (C.pack "expiration" , C.pack     $ form ex)
            , (C.pack "content"    , L.toStrict $ JSON.encode ct)
            ] req'
  manager <- HTTP.newManager HTTP.tlsManagerSettings
  response <- HTTP.httpLbs req manager
  let resp = fromJust . JSON.decode $ HTTP.responseBody response
  case status resp of
    "ok" -> return $ bin ++ "/paste/" ++ (fromJust . paste) resp
    _    -> throwIO . ZeroBinError $ (fromJust . message) resp

share :: String -> Expiration -> ByteString -> IO String
share bin ex txt = do
  pwd  <- makePassword 33
  cnt  <- encrypt pwd (encode txt)
  uri  <- post bin ex cnt
  return $ uri ++ "#" ++ pwd

