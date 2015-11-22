{-# LANGUAGE OverloadedStrings #-}
module DeepLink
    ( deepLink
    ) where

import           Control.Concurrent.MVar
import qualified Control.Exception as E
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import           Data.Elf (ElfSection(..), Elf(..), parseElf)
import           Data.List (sortBy)
import           Data.Ord (comparing)
import           Data.Typeable (Typeable)
import qualified OrderedSet as OSet
import           System.FilePath.ByteString (FilePath, (</>))
import qualified System.FilePath.ByteString as FilePath
import           System.IO.Error
import           System.IO.Posix.MMap (unsafeMMapFile)

import           Prelude.Compat hiding (FilePath)

data MissingElfFile = MissingElfFile FilePath
    deriving (Typeable)
instance E.Exception MissingElfFile
instance Show MissingElfFile where
    show (MissingElfFile path)
        | "/" `BS8.isPrefixOf` path =
          "Missing *absolute* .o path " ++ show path ++ ". " ++
          "Are you running a kernel older than end of 2014? If so, upgrade!" ++
          "Otherwise, is your build system configured to build this path?"
        | otherwise =
          "Failed to open file: " ++ show path ++ ". " ++
          "Are you missing a build rule to build it?"

readElfSection :: String -> FilePath -> IO ByteString
readElfSection sectionName path =
    do
        fileData <-
            unsafeMMapFile (BS8.unpack path)
            `E.catch` \e ->
            if isDoesNotExistErrorType (ioeGetErrorType e)
            then E.throwIO $ MissingElfFile path
            else E.throwIO   e
        let elf = parseElf fileData
        let namedSections =
                [ (elfSectionName section, section)
                | section <- elfSections elf ]
        return $ maybe BS8.empty elfSectionData $ lookup sectionName namedSections

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs [_] = error "Uneven number split into pairs?"
pairs (x:y:xs) = (x, y) : pairs xs

getOFilesNeeded :: FilePath -> IO [FilePath]
getOFilesNeeded oPath =
    do
        deepLinkSectionStr <- readElfSection "deeplink" oPath
        let dependencies = pairs $ filter (not . BS8.null) $ BS8.split '\0' deepLinkSectionStr
        return $ map f dependencies
    where
        f (hPath, depOPath)
            | "-l" `BS8.isPrefixOf` depOPath = depOPath
            | otherwise = FilePath.takeDirectory hPath </> depOPath

data Order = File | Lib | SystemLib deriving (Eq, Ord)
getOrder :: FilePath -> Order
getOrder filePath
    | "-l" `BS8.isPrefixOf` filePath = SystemLib
    | ".a" `BS8.isSuffixOf` filePath = Lib
    | otherwise = File

deepLink :: FilePath -> [FilePath] -> IO [FilePath]
deepLink cwd opaths =
    do
        alreadyAdded <- newMVar OSet.empty
        let addMany = void . mapM addRecursively
            addRecursively oPathRaw
                | File /= getOrder oPathRaw =
                  modifyMVar_ alreadyAdded $ return . OSet.tryAppend oPathRaw
                | otherwise =
                  do
                      let oPathRelative = FilePath.canonicalizePathAsRelative cwd oPathRaw
                      let add oldSet =
                              case OSet.maybeAppend oPathRelative oldSet of
                              Nothing -> (oldSet, True)
                              Just newSet -> (newSet, False)
                      alreadyMember <- modifyMVar alreadyAdded $ return . add
                      unless alreadyMember $ addMany =<< getOFilesNeeded oPathRelative
        addMany opaths
        sortOn getOrder . OSet.toList <$> readMVar alreadyAdded

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn = sortBy . comparing
