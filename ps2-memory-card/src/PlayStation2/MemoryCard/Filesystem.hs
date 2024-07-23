{-# LANGUAGE PatternSynonyms #-}

-- | PlayStation 2 memory card filesystem.
module PlayStation2.MemoryCard.Filesystem
  ( Filesystem (fRawData, fSuperblock, fFatMatrix, fRootDirectoryEntry)
  , toSuperblockAndData
  , toSuperblockFatMatrixAndData
  , ReadFilesystemError (..)
  , readFilesystem

  , FilesystemReference (..)
  , fromEntry
  , buildDirectoryTree

  , DirectoryReference (DirectoryReference)
  , mkDirectoryReference
  , listDirectoryContents

  , FileReference (FileReference)
  , mkFileReference
  , readFile
  ) where

import Data.Bifunctor
  ( first )
import Data.Binary.Get
  ( ByteOffset )
import Data.ByteString
  ( ByteString )
import qualified Data.ByteString.Lazy as LBS
import Data.Tree
  ( Tree (..) )
import PlayStation2.MemoryCard.Entry
  ( Entry, isDirectory, isFile )
import PlayStation2.MemoryCard.Fat
  ( FindChildEntriesError
  , FindFileDataError
  , FindRootDirectoryEntryError (..)
  , MkMemoryCardFatMatrixError (..)
  , findChildEntries
  , findFileData
  , findRootDirectoryEntry
  , mkMemoryCardFatMatrix
  )
import PlayStation2.MemoryCard.Fat.Matrix
  ( FatMatrix )
import PlayStation2.MemoryCard.PartialFilesystem
  ( SuperblockAndData (..)
  , SuperblockFatMatrixAndData (..)
  , mkSuperblockAndData
  )
import PlayStation2.MemoryCard.Superblock
  ( Superblock )
import Prelude hiding
  ( readFile )

-- | PlayStation 2 memory card filesystem.
data Filesystem = Filesystem
  { -- | Raw memory card data in its entirety.
    fRawData :: !ByteString
  , -- | Memory card superblock.
    fSuperblock :: !Superblock
  , -- | Matrix of direct FAT clusters.
    fFatMatrix :: !FatMatrix
  , -- | Root directory entry.
    fRootDirectoryEntry :: !DirectoryReference
  } deriving stock (Show, Eq)

-- | Convert a 'Filesystem' to a 'SuperblockAndData'.
toSuperblockAndData :: Filesystem -> SuperblockAndData
toSuperblockAndData fs =
  SuperblockAndData
    { sadRawMemoryCardData = fRawData
    , sadSuperblock = fSuperblock
    }
  where
    Filesystem
      { fRawData
      , fSuperblock
      } = fs

-- | Convert a 'Filesystem' to a 'SuperblockFatMatrixAndData'.
toSuperblockFatMatrixAndData :: Filesystem -> SuperblockFatMatrixAndData
toSuperblockFatMatrixAndData fs =
  SuperblockFatMatrixAndData
    { sfmadRawMemoryCardData = fRawData
    , sfmadSuperblock = fSuperblock
    , sfmadFatMatrix = fFatMatrix
    }
  where
    Filesystem
      { fRawData
      , fSuperblock
      , fFatMatrix
      } = fs

-- | Error reading a 'Filesystem'.
data ReadFilesystemError
  = -- | Error reading the memory card's 'Superblock'.
    ReadFilesystemGetSuperblockError !(LBS.ByteString, ByteOffset, String)
  | -- | Error constructing the memory card's direct FAT matrix.
    ReadFilesystemMkMemoryCardFatMatrixError !MkMemoryCardFatMatrixError
  | -- | Error finding the root directory entry in a memory card.
    ReadFilesystemFindRootDirectoryEntryError !FindRootDirectoryEntryError
  | -- | Root directory is not actually a directory.
    ReadFilesystemInvalidRootDirectoryEntryError
      -- | Invalid root directory entry.
      !Entry
  deriving stock (Show, Eq)

-- | Read and construct a 'Filesystem' from raw memory card data.
readFilesystem :: ByteString -> Either ReadFilesystemError Filesystem
readFilesystem bs = do
  sad@SuperblockAndData
    { sadRawMemoryCardData
    , sadSuperblock
    } <- first ReadFilesystemGetSuperblockError (mkSuperblockAndData bs)
  fatMatrix <- first ReadFilesystemMkMemoryCardFatMatrixError (mkMemoryCardFatMatrix sad)
  rootDirectoryEntry <- first ReadFilesystemFindRootDirectoryEntryError (findRootDirectoryEntry sad)
  rootDirectoryRef <-
    case mkDirectoryReference rootDirectoryEntry of
      Nothing -> Left (ReadFilesystemInvalidRootDirectoryEntryError rootDirectoryEntry)
      Just r -> Right r
  Right Filesystem
    { fRawData = sadRawMemoryCardData
    , fSuperblock = sadSuperblock
    , fFatMatrix = fatMatrix
    , fRootDirectoryEntry = rootDirectoryRef
    }

-- | Reference to a directory.
newtype DirectoryReference = MkDirectoryReference
  { unDirectoryReference :: Entry }
  deriving stock (Show, Eq)

pattern DirectoryReference :: Entry -> DirectoryReference
pattern DirectoryReference e <- MkDirectoryReference e

{-# COMPLETE DirectoryReference #-}

mkDirectoryReference :: Entry -> Maybe DirectoryReference
mkDirectoryReference e
  | isDirectory e = Just (MkDirectoryReference e)
  | otherwise = Nothing

-- | Reference to a file.
newtype FileReference = MkFileReference
  { unFileReference :: Entry }
  deriving stock (Show, Eq)

pattern FileReference :: Entry -> FileReference
pattern FileReference e <- MkFileReference e

{-# COMPLETE FileReference #-}

mkFileReference :: Entry -> Maybe FileReference
mkFileReference e
  | isFile e = Just (MkFileReference e)
  | otherwise = Nothing

-- | Reference to a filesystem object (i.e. a directory or file).
data FilesystemReference
  = -- | Reference to a directory.
    FilesystemDirectoryReference !DirectoryReference
  | -- | Reference to a file.
    FilesystemFileReference !FileReference
  deriving stock (Show, Eq)

-- | Convert an 'Entry' to a 'FilesystemReference'.
--
-- If the provided directory entry does not correspond to a file or directory,
-- the result is 'Nothing' (this shouldn't be possible in a well-formatted
-- filesystem).
fromEntry :: Entry -> Maybe FilesystemReference
fromEntry e
  | isFile e = FilesystemFileReference <$> mkFileReference e
  | isDirectory e = FilesystemDirectoryReference <$> mkDirectoryReference e
  | otherwise = Nothing

-- | Error listing directory contents.
data ListDirectoryContentsError
  = -- | Error finding child entries.
    ListDirectoryContentsFindChildEntriesError !FindChildEntriesError
  | -- | Error converting an 'Entry' to a 'FilesystemReference'.
    ListDirectoryContentsConvertEntryToFilesystemReferenceError

-- | List the contents of a directory.
listDirectoryContents
  :: Filesystem
  -> DirectoryReference
  -> Either ListDirectoryContentsError [FilesystemReference]
listDirectoryContents fs (DirectoryReference e) = do
  children <-
    first ListDirectoryContentsFindChildEntriesError $
      findChildEntries (toSuperblockFatMatrixAndData fs) e
  case mapM fromEntry children of
    Nothing -> Left ListDirectoryContentsConvertEntryToFilesystemReferenceError
    Just x -> Right x

-- | Build a tree of directory contents.
buildDirectoryTree
  :: Filesystem
  -> DirectoryReference
  -> Either ListDirectoryContentsError (Tree FilesystemReference)
buildDirectoryTree fs startingDirRef = go True (Node startingFsDirRef []) []
  where
    Filesystem { fRootDirectoryEntry } = fs

    startingFsDirRef :: FilesystemReference
    startingFsDirRef = FilesystemDirectoryReference startingDirRef

    go
      :: Bool
      -- ^ Whether this is the first run.
      -> Tree FilesystemReference
      -- ^ Accumulated 'Tree'.
      -> [FilesystemReference]
      -- ^ 'FilesystemReference's from which to build sub-trees.
      -> Either ListDirectoryContentsError (Tree FilesystemReference)
    go True (Node r []) [] =
      -- This case represents the first run.
      case r of
        FilesystemFileReference _ -> error "impossible: starting directory reference is a file reference"
        FilesystemDirectoryReference dr -> do
          dirContents <- listDirectoryContents fs dr
          go False (Node r []) dirContents
    go True _ _ = error "impossible: the first run must be given a tree with no children and an empty list"
    go False acc [] = Right acc
    go False (Node r acc) (x : xs) =
      case x of
        FilesystemFileReference _ -> go False (Node r (acc ++ [Node x []])) xs
        FilesystemDirectoryReference dr
          | -- The root directory is a special case.
            --
            -- Unless this is the first run (i.e. the root directory is the
            -- starting directory from which we're building the tree), we
            -- should not attempt to build sub-trees based on the root
            -- directory as it will result in an infinite loop.
            dr == fRootDirectoryEntry ->
              go False (Node r (acc ++ [Node x []])) xs
          | otherwise -> do
              dirContents <- listDirectoryContents fs dr
              subTree <- go False (Node x []) dirContents
              go False (Node r (acc ++ [subTree])) xs

-- | Read the contents of a file as a 'ByteString'.
readFile :: Filesystem -> FileReference -> Either FindFileDataError ByteString
readFile fs (FileReference e) =
  findFileData (toSuperblockFatMatrixAndData fs) e
