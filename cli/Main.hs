{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.Version (showVersion)
import Paths_zerobin (version) -- from cabal
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (stderr, hPutStrLn)
import Text.RawString.QQ (r)
import ZeroBin (share, Expiration(..), pasteEc)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified System.Console.Docopt.NoTH as O

usage :: String
usage =  "zerobin " ++ showVersion version
  ++ " pastes to " ++ pasteEc ++ [r|
zerobin prints URI to be shared or error message

Usage:
  zerobin [options] TEXT

Options:
  -e, --expire=E  Set expiration of paste: once, day, week, month [default: week]
  -f, --file      Paste the content of file TEXT instead of plain TEXT

  -h, --help      Show this message

Examples:
  zerobin hello           paste "hello" for a week
  zerobin -f /etc/fstab   paste file /etc/fstab for a week
  zerobin -e once hello   paste "hello", it will burn after reading
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
getContent asFile text =
  if not asFile
    then return $ C.pack text
    else BS.readFile text


main :: IO ()
main = do
  doco <- O.parseUsageOrExit usage
  args  <- O.parseArgsOrExit doco =<< getArgs
  if args `O.isPresent` O.longOption "help"
  then putStrLn $ O.usage doco
  else do
    let get = O.getArgOrExitWith doco
    expire <- args `get` O.longOption "expire"
    text   <- args `get` O.argument "TEXT"
    cnt    <- getContent (args `O.isPresent` O.longOption "file") text
    case getExpiration expire of
      Nothing  -> die "invalid value for expiration"
      Just e   -> do
        rc <- share e cnt
        case rc of
          Left err -> die err
          Right uri -> putStrLn uri

