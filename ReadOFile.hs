{-# LANGUAGE OverloadedStrings #-}
-- | Read a single .o files deeplink's sections

module Main (main) where

import           Control.Monad (forM_)
import qualified Data.ByteString.Char8 as BS8
import           Data.Monoid ((<>))
import           DeepLink (readDeps, readPrunes)
import           System.Environment (getArgs)

import           Prelude.Compat

main :: IO ()
main =
    do
        oPaths <- map BS8.pack <$> getArgs
        forM_ oPaths $ \oPath ->
            do
                BS8.putStrLn $ oPath <> ":"
                readPrunes oPath >>= mapM_ (BS8.putStrLn . ("  prune: " <>))
                readDeps oPath >>= mapM_ (BS8.putStrLn . ("  dep: " <>))
