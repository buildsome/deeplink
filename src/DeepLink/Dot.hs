{-# LANGUAGE OverloadedStrings #-}
module DeepLink.Dot
       ( genDot )
       where

import           Control.Monad (forM_)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import           System.FilePath.ByteString (FilePath)
import           System.IO (withFile, IOMode(..))

import qualified DeepLink

import           Prelude.Compat hiding (FilePath)

quote :: ByteString -> ByteString
quote = BS8.pack . show


genDot :: Map DeepLink.Dependent (Set FilePath) -> FilePath -> IO ()
genDot dependencies dotFilePath =
    withFile (BS8.unpack dotFilePath) WriteMode $ \handle -> do
      BS8.hPutStrLn handle "digraph G {"
      forM_ (Map.toList dependencies) $ \(dependent, reqObjFiles) ->
        forM_ reqObjFiles $ \objFile ->
          BS8.hPutStrLn handle $ mconcat
          [ "\t"
          , case dependent of
                  DeepLink.CmdLine -> "commandline"
                  DeepLink.ObjFile o -> quote o
          , " -> "
          , quote objFile
          , ";" ]
      BS8.hPutStrLn handle "}"
