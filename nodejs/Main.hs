{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import System.Process (callProcess)
import Web.ZeroBin.SJCL (encrypt)
import Web.ZeroBin.Utils (makePassword)
import qualified Data.Aeson as JSON
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L

-- nodejs is a Debian's thing, others may have simple "node"

getText :: IO BS.ByteString
getText = do
  args <- map C.pack `fmap` getArgs
  if null args
    then return "heinrich hertz"
    else return . BS.intercalate " " $ args

main :: IO ()
main = do
  password <- makePassword 32
  text     <- getText
  cont     <- encrypt password text
  callProcess "nodejs" [ "nodejs/decrypt.js"
      , password
      , C.unpack . L.toStrict $ JSON.encode cont
    ]

