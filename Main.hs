-- | Deeplink's main module.
--
-- Wraps the DeepLink with optparse based option parsing and invokes
-- the given command.
{-# LANGUAGE CPP #-}
module Main (main) where

import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import           DeepLink
import           FilePath (FilePath)
import           Options.Applicative
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
bytestr = liftM BS8.pack $ str
#endif

getOpts :: IO Opts
getOpts =
  execParser $
  info (helper <*> parser) $
  fullDesc
  <> progDesc "Deeply link a target."
  <> header "deeplink - deeply link a target"
  where
    parser =
      Opts
      <$> strOption
          (long "ld" <> help "ld command to use" <>
           metavar "ld-command")
      <*> many (argument bytestr (metavar "opaths"))
      <*> switch (long "verbose" <> short 'v' <> help "Verbose mode")

main :: IO ()
main = do
  -- setNumCapabilities . (*2) =<< getNumProcessors -- To get full reasonable buildsome parallelism
  Opts ldCommand oPaths verbose <- getOpts
  cwd <- Posix.getWorkingDirectory
  fullList <- deepLink cwd oPaths
  let cmd@(cmdExec:cmdArgs) = words ldCommand ++ map BS8.unpack fullList
  when verbose $ putStrLn $ unwords cmd
  callProcess cmdExec cmdArgs
  return ()
