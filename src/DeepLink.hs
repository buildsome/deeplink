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
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Typeable (Typeable)
import qualified OrderedSet as OSet
import           System.FilePath.ByteString (FilePath, (</>))
import qualified System.FilePath.ByteString as FilePath
import           System.IO.Error
import           System.IO.Posix.MMap (unsafeMMapFile)

import           Prelude.Compat hiding (FilePath)

data DeepLinkError
    = MissingElfFile FilePath
    -- ^ The specified ELF (object) file does not exist
    | ConflictingPrune FilePath
    -- ^ Trying to prune an object file that was already added
    | RedundantPrunes (Set FilePath)
    -- ^ Requested to prune a file that was not asked for by any dependency.
    -- Such a prune cmd is meaningless and is likely a wrong-name error.
    deriving (Typeable)
instance E.Exception DeepLinkError
instance Show DeepLinkError where
    show (MissingElfFile path)
        | "/" `BS8.isPrefixOf` path =
          "Missing *absolute* .o path " ++ show path ++ ". " ++
          "Are you running a kernel older than end of 2014? If so, upgrade!" ++
          "Otherwise, is your build system configured to build this path?"
        | otherwise =
          "Failed to open file: " ++ show path ++ ". " ++
          "Are you missing a build rule to build it?"
    show (ConflictingPrune path) =
        "Cannot prune already-added file " ++ show path ++ ". " ++
        "Make sure your prune cmds are the first thing in the target."
    show (RedundantPrunes paths) =
        "Did not prune files " ++ show paths ++ " specified by prune cmds " ++
        "as they were not specified as a dependency by anyone." ++
        "Make sure the prune cmd uses the correct path/filename."

readElfSection :: String -> FilePath -> IO ByteString
readElfSection sectionName path =
    do
        fileData <-
            unsafeMMapFile (BS8.unpack path)
            `E.catch` \e ->
            if isDoesNotExistErrorType (ioeGetErrorType e)
            then E.throwIO $ MissingElfFile path
            else E.throwIO e
        let elf = parseElf fileData
        let namedSections =
                [ (elfSectionName section, section)
                | section <- elfSections elf ]
        return $ maybe BS8.empty elfSectionData $ lookup sectionName namedSections

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs [_] = error "Uneven number split into pairs?"
pairs (x:y:xs) = (x, y) : pairs xs

readDeepLinkSection :: String -> FilePath -> IO [FilePath]
readDeepLinkSection sectionName oPath =
    do
        deepLinkSectionStr <- readElfSection sectionName oPath
        let cmds = pairs $ filter (not . BS8.null) $ BS8.split '\0' deepLinkSectionStr
        return $ map f cmds
    where
        f (hPath, depOPath)
            | "-l" `BS8.isPrefixOf` depOPath = depOPath
            | otherwise = FilePath.takeDirectory hPath </> depOPath

onEachInSection :: (FilePath -> IO ()) -> String -> FilePath -> IO ()
onEachInSection func sectionName oPath = mapM_ func =<< readDeepLinkSection sectionName oPath

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
        pruned <- newMVar OSet.empty
        alreadyPruned <- newMVar OSet.empty
        let addPruneList oPathRaw =
                do
                    let oPathRelative = FilePath.canonicalizePathAsRelative cwd oPathRaw
                    -- if the mvar is used concurrenty, this is a race (they are not nested)
                    pruneConflict <- withMVar alreadyAdded $ return . OSet.isMember oPathRelative
                    when pruneConflict $ E.throwIO $ ConflictingPrune oPathRaw
                    modifyMVar_ pruned $ return . OSet.tryAppend oPathRelative
        let addLinkCmds oPathRelative =
                do
                    onEachInSection addPruneList "deeplink-prune" oPathRelative
                    onEachInSection addRecursively "deeplink-dep" oPathRelative
            addRecursively oPathRaw
                | File /= getOrder oPathRaw =
                  modifyMVar_ alreadyAdded $ return . OSet.tryAppend oPathRaw
                | otherwise =
                  do
                      let oPathRelative = FilePath.canonicalizePathAsRelative cwd oPathRaw
                      fileIsPruned <-
                          withMVar pruned $ return . OSet.isMember oPathRelative
                      if fileIsPruned
                          then modifyMVar_ alreadyPruned $ return . OSet.tryAppend oPathRelative
                          else do
                          let add oldSet =
                                  case OSet.maybeAppend oPathRelative oldSet of
                                  Nothing -> (oldSet, True)
                                  Just newSet -> (newSet, False)
                          alreadyMember <- modifyMVar alreadyAdded $ return . add
                          unless alreadyMember $ addLinkCmds oPathRelative
        mapM_ addRecursively opaths
        alreadyPrunedSet <- OSet.toSet <$> readMVar alreadyPruned
        prunedSet <- OSet.toSet <$> readMVar pruned
        let redundantPrunes = prunedSet `Set.difference` alreadyPrunedSet
        unless (Set.null redundantPrunes) $ E.throwIO (RedundantPrunes redundantPrunes)
        sortOn getOrder . OSet.toList <$> readMVar alreadyAdded

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn = sortBy . comparing
