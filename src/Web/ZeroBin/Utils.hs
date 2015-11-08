{-|
Various utility functions
-}

module Web.ZeroBin.Utils (
  toWeb
, makePassword
) where

import Crypto.Random.Entropy (getEntropy)
import Data.ByteString (ByteString)
import Data.ByteString.Base64 (encode)
import Data.ByteString.Char8 (unpack)

-- | Encodes to base64 and drops padding '='.
toWeb :: ByteString -- ^ the data to encode
      -> String     -- ^ base64 string without padding
toWeb = takeWhile (/= '=') . unpack . encode

-- | Makes a random password
makePassword :: Int       -- ^ the number of bytes of entropy
             -> IO String -- ^ random byte-string encoded by 'toWeb'
makePassword n = toWeb `fmap` getEntropy n

