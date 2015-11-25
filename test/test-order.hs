-- | Test that the order of linkage instructions is preserved all the
-- way to the final command
{-# LANGUAGE CPP, OverloadedStrings #-}
module Main (main) where

import           Control.Exception (bracket)
import           Control.Monad (unless)
import qualified Data.ByteString.Char8 as BS8
import           Data.List ((\\))
import           Data.Monoid ((<>))
import           DeepLink (deepLink)
import           System.FilePath (takeDirectory, (</>))
import           System.IO (Handle, openTempFile, hClose)
import           System.Process

import           Prelude.Compat

myDir :: FilePath
myDir = takeDirectory __FILE__

withTempFile :: String -> ((FilePath, Handle) -> IO a) -> IO a
withTempFile template =
    bracket (openTempFile "/tmp" template) (hClose . snd)

order :: [Int]
order = [0,5,1,2,3,8,4,9,10,11]

prunes :: [Int]
prunes = [8, 10]

prog :: String
prog =
    unlines $
    "#include <deeplink.h>" :
    "" :
    [ "DEEPLINK__ADD_LIB(\"" ++ show i ++ "\")"
    | i <- order
    ] ++
    [ "DEEPLINK__PRUNE_LIB(\"" ++ show i ++ "\")"
    | i <- prunes
    ]

main :: IO ()
main =
    withTempFile "file.o" $ \(oPath, _) ->
    do
        _ <- readProcess "gcc" ["-o", oPath, "-c", "-xc", "-I", myDir </> "../include", "-"] prog
        fileList <- deepLink "." [BS8.pack oPath]
        let expected = map BS8.pack $ oPath : ["-l" <> show i | i <- order \\ prunes]
        let unlinesStr = unlines . map BS8.unpack
        unless (expected == fileList) $
            fail $
            "Got: \n" <> unlinesStr fileList <>
            "Expected: \n" <> unlinesStr expected
