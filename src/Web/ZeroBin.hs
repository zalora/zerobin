{-|
High-level functions for posting to 0bin services like
<http://0bin.net> or <http://paste.ec>.

 >>> import Web.ZeroBin
 >>> import Data.ByteString.Char8
 >>> share "http://0bin.net" Day (pack "hello")
"http://0bin.net/paste/ZH6VyKXjDHAiPT8J#C6LLidGyHO7xt3xuDtsNHjZ77luualukEuJ25S6w/K1m"

-}

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

-- | 0bin error message
data ZeroBinError = ZeroBinError String
  deriving (Show, Typeable)
instance Exception ZeroBinError

-- | Expiration of a paste.
--   "Burn after reading" really means "burn after two readings",
--   because we do not redirect to the paste like a browser does.
--   You can verify your paste before sharing the link.
--   Original <http://0bin.net> does not support 'Week'.
data Expiration
  = Once      -- ^ burn after reading
  | Day       -- ^ keep for 24 hours
  | Week      -- ^ for 7 days
  | Month     -- ^ for 30 days
  | Never     -- ^ for 100 years

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


-- | Encrypts the plain data with a random password,
--   post to 0bin and return the URI of a new paste.
--   Can throw 'ZeroBinError' or 'Network.HTTP.Conduit.HttpException'.
share :: String      -- ^ the address of 0bin, e. g. <http://0bin.net> or <https://paste.ec>
      -> Expiration
      -> ByteString  -- ^ the plain data to encrypt and paste
      -> IO String   -- ^ the URI of paste
share bin ex txt = do
  pwd  <- makePassword 33
  cnt  <- encrypt pwd (encode txt)
  uri  <- post bin ex cnt
  return $ uri ++ "#" ++ pwd

