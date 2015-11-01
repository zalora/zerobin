{-# LANGUAGE DeriveGeneric #-}

module ZeroBin (
  Expiration(..),
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

post :: String -> Expiration -> Content -> IO (Either String String)
post bin ex ct = do
  req' <- HTTP.parseUrl $ bin ++ "/paste/create"
  let req = HTTP.urlEncodedBody
            [ (C.pack "expiration" , C.pack     $ show ex)
            , (C.pack "content"    , L.toStrict $ JSON.encode ct)
            ] req'
  manager <- HTTP.newManager HTTP.tlsManagerSettings
  response <- HTTP.httpLbs req manager
  let resp = fromJust . JSON.decode $ HTTP.responseBody response
  case status resp of
    "ok" -> return . Right $
            bin ++ "/paste/" ++ (fromJust . paste) resp
    _    -> return . Left $
            (fromJust . message) resp

share :: String -> Expiration -> ByteString -> IO (Either String String)
share bin ex txt = do
  pwd  <- makePassword 33
  c    <- encrypt pwd (encode txt)
  append pwd `fmap` post bin ex c
  where
    append _ (Left e) = Left e
    append p (Right u) = Right $ u ++ "#" ++ p

