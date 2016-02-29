{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.Version (showVersion)
import Paths_zerobin (version) -- from cabal
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (stderr, hPutStrLn)
import Text.RawString.QQ (r)
import Web.ZeroBin (share, Expiration(..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified System.Console.Docopt.NoTH as O

usage :: String
usage =  "zerobin " ++ showVersion version
  ++ " pastes to 0bin services" ++ [r|
zerobin prints URI to be shared or error message
See http://0bin.net and https://paste.ec

Usage:
  zerobin [options] TEXT

Options:
  -b, --bin=BIN   0bin service [default: https://paste.ec]
  -f, --file      Paste the content of file TEXT ("-" for stdin)
  -e, --expire=E  Set expiration of paste: once, day, week, month [default: day]

  -h, --help      Show this message

Examples:
  zerobin hello                      paste "hello" for a day
  zerobin -f /etc/fstab              paste file /etc/fstab for a day
  cat /etc/fstab | zerobin -f -      likewise
  zerobin -e once hello              paste "hello", it will burn after reading
  zerobin -b http://0bin.net hello   paste to 0bin.net
|]


getExpiration :: String -> Maybe Expiration
getExpiration e =
  case e of
    "once"  -> Just Once
    "day"   -> Just Day
    "week"  -> Just Week
    "month" -> Just Month
    _       -> Nothing

die :: String -> IO ()
die msg = do
  hPutStrLn stderr $ "zerobin: " ++ msg
  exitFailure

getContent :: Bool -> String -> IO BS.ByteString
getContent fromFile text
  | fromFile && (text == "-") = BS.getContents
  | fromFile = BS.readFile text
  | otherwise = return $ C.pack text

main :: IO ()
main = do
  doco <- O.parseUsageOrExit usage
  args <- O.parseArgsOrExit doco =<< getArgs
  if args `O.isPresent` O.longOption "help"
  then putStrLn $ O.usage doco
  else do
    let get = O.getArgOrExitWith doco
    bin    <- args `get` O.longOption "bin"
    expire <- args `get` O.longOption "expire"
    text   <- args `get` O.argument "TEXT"
    cnt    <- getContent (args `O.isPresent` O.longOption "file") text
    case getExpiration expire of
      Nothing -> die "invalid value for expiration"
      Just e  -> share bin e cnt >>= putStrLn

