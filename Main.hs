-- | Deeplink's main module.
--
-- Wraps the DeepLink with optparse based option parsing and invokes
-- the given command.
{-# LANGUAGE CPP #-}
module Main (main) where

import           Control.Monad (liftM, when)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified DeepLink
import           Options.Applicative
import           System.FilePath.ByteString (FilePath)
import qualified System.Posix.ByteString as Posix
import           System.Process (callProcess)

import           Prelude.Compat hiding (FilePath)

data Opts = Opts
  { _ldCommand :: String
  , _oPaths :: [FilePath]
  , _verbose :: Bool
  } deriving Show

#ifdef OPTPARSE_OLD_VERSION
bytestr :: Monad m => String -> m ByteString
bytestr = liftM BS8.pack . str
#else
bytestr :: ReadM ByteString
bytestr = liftM BS8.pack str
#endif

desc :: String
desc =
    unlines
    [ "Recursively scans for dependencies in .o files (specified via "
    , "DEEPLINK__ADD_* macros in a C compilation) from a specified root set "
    , "of .o files.  Gives the result to a specified command (e.g: \"ld\" or "
    , "\"echo\")."
    ]

getOpts :: IO Opts
getOpts =
  execParser $
  info (helper <*> parser) $
  fullDesc
  <> progDesc desc
  <> header "deeplink - deeply link a target"
  where
    parser =
      Opts
      <$> strOption
          (long "ld" <> help "ld command to use" <>
           metavar "ld-command")
      <*> some (argument bytestr (metavar "opaths" <> help "At least one root .o path"))
      <*> switch (long "verbose" <> short 'v' <> help "Verbose mode")

main :: IO ()
main = do
  -- setNumCapabilities . (*2) =<< getNumProcessors -- To get full reasonable buildsome parallelism
  Opts ldCommand oPaths verbose <- getOpts
  cwd <- Posix.getWorkingDirectory
  fullList <- DeepLink.deepLink cwd oPaths
  let cmd@(cmdExec:cmdArgs) = words ldCommand ++ map BS8.unpack fullList
  when verbose $ putStrLn $ unwords cmd
  callProcess cmdExec cmdArgs
  return ()
