module PlayStation2.MemoryCard.Superblock
  ( MagicString (..)
  , magicStringFromByteString
  , magicStringToByteString

  , Version (..)
  , versionFromByteString
  , versionToByteString

  , Metadata (..)
  , MkMetadataParams (..)
  , mkMetadata

  , Superblock (..)
  , superblockByteSize
  , getSuperblock
  , putSuperblock
  ) where

import Control.Monad
  ( replicateM, replicateM_ )
import Data.Binary.Get
  ( Get, getByteString, getWord16le, getWord32le, getWord8, skip )
import Data.Binary.Put
  ( Put, putByteString, putWord16le, putWord32le, putWord8 )
import Data.ByteString
  ( ByteString )
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word
  ( Word16, Word32, Word8 )
import PlayStation2.MemoryCard.Cluster.Index
  ( ClusterIndex, getClusterIndex, putClusterIndex )
import PlayStation2.MemoryCard.Superblock.IndirectFatClusterList
  ( IndirectFatClusterList
  , getIndirectFatClusterList
  , putIndirectFatClusterList
  )
import Prelude

------------------------------------------------------------------------------
-- Magic string
------------------------------------------------------------------------------

-- | Magic string for memory card format.
data MagicString = MagicString
  deriving stock (Show, Eq)

-- | Memory card format magic string ('sMagic').
magicString :: ByteString
magicString = "Sony PS2 Memory Card Format "

magicStringToByteString :: MagicString -> ByteString
magicStringToByteString _ = magicString

magicStringFromByteString :: ByteString -> Maybe MagicString
magicStringFromByteString bs
  | bs == magicString = Just MagicString
  | otherwise = Nothing

getMagicString :: Get MagicString
getMagicString = do
  bs <- getByteString (BS.length magicString)
  case magicStringFromByteString bs of
    Just _ -> pure MagicString
    Nothing -> fail $ "Invalid magic string: " <> T.unpack (TE.decodeUtf8 bs)

putMagicString :: MagicString -> Put
putMagicString _ = putByteString magicString

------------------------------------------------------------------------------
-- Version
------------------------------------------------------------------------------

-- | Memory card format version.
data Version
  = -- | Version 1.2.0.0.
    Version1_2_0_0
  deriving stock (Show, Eq)

versionToByteString :: Version -> ByteString
versionToByteString v = case v of
  Version1_2_0_0 -> "1.2.0.0\0\0\0\0\0"

versionFromByteString :: ByteString -> Maybe Version
versionFromByteString bs = case bs of
  "1.2.0.0\0\0\0\0\0" -> Just Version1_2_0_0
  _ -> Nothing

getVersion :: Get Version
getVersion = do
  bs <- getByteString 12
  case versionFromByteString bs of
    Just v -> pure v
    Nothing -> fail $ "Invalid version: " <> T.unpack (TE.decodeUtf8 bs)

putVersion :: Version -> Put
putVersion = putByteString . versionToByteString

------------------------------------------------------------------------------
-- Metadata
------------------------------------------------------------------------------

-- | Useful metadata calculated from values within the 'Superblock'.
data Metadata = Metadata
  { -- | Size (in bytes) of each page's spare area.
    mPageSpareAreaSize :: !Word16
  , -- | Size (in bytes) of a page along with its spare area.
    mPageWithSpareAreaSize :: !Word16
  , -- | Cluster size in bytes.
    mClusterSize :: !Word16
  , -- | Number of columns in a FAT cluster matrix.
    mFatClusterMatrixColumns :: !Word16
  } deriving stock (Show, Eq)

-- | Data required to construct 'Metadata'.
data MkMetadataParams = MkMetadataParams
  { mmpPageLen :: !Word16
  , mmpPagesPerCluster :: !Word16
  } deriving stock (Show, Eq)

-- | Construct 'Superblock' 'Metadata'.
mkMetadata :: MkMetadataParams -> Metadata
mkMetadata params =
  Metadata
    { mPageSpareAreaSize = pageSpareAreaSize
    , mPageWithSpareAreaSize = mmpPageLen + pageSpareAreaSize
    , mClusterSize = clusterSize
    , mFatClusterMatrixColumns = fst (clusterSize `divMod` 4)
    }
  where
    MkMetadataParams
      { mmpPageLen
      , mmpPagesPerCluster
      } = params

    pageSpareAreaSize :: Word16
    pageSpareAreaSize = (fst $ mmpPageLen `divMod` 128) * 4 -- TODO: use safe unsigned arithmetic

    clusterSize :: Word16
    clusterSize = mmpPageLen * mmpPagesPerCluster

------------------------------------------------------------------------------
-- Superblock
------------------------------------------------------------------------------

-- | PS2 memory card \"superblock\" data structure.
data Superblock = Superblock
  { -- | Magic string which identifies the memory card as being formatted.
    --
    -- See 'magicString'.
    sMagic :: !MagicString
  , -- | Format version number.
    sVersion :: !Version
  , -- | Size (in bytes) of a memory card page.
    sPageLen :: !Word16
  , -- | Number of pages per FAT cluster.
    sPagesPerCluster :: !Word16
  , -- | Number of pages per erase block.
    sPagesPerBlock :: !Word16
  , -- | Unknown field that appears to be unused.
    sUnknown :: !Word16
  , -- | Number of FAT clusters contained within this card.
    sClustersPerCard :: !Word32
  , sAllocOffset :: !Word32
  , sAllocEnd :: !Word32
  , -- | First cluster of the root directory, relative to 'sAllocOffset'.
    sRootDirCluster :: !ClusterIndex
  , sBackupBlock1 :: !Word32
  , sBackupBlock2 :: !Word32
  , -- | List of indirect FAT clusters.
    --
    -- Note that, on a standard 8M card, there's only one indirect FAT
    -- cluster.
    sIfcList :: !IndirectFatClusterList
  , sBadBlockList :: ![Word32]
  , sCardType :: !Word8
  , sCardFlags :: !Word8
  , -- | Useful metadata derived from values within the superblock.
    sMetadata :: !Metadata
  } deriving stock (Show, Eq)

-- | Size of a binary serialized 'Superblock' in bytes.
superblockByteSize :: Int
superblockByteSize = 340

getSuperblock :: Get Superblock
getSuperblock = do
  magic <- getMagicString
  version <- getVersion
  pageLen <- getWord16le
  pagesPerCluster <- getWord16le
  partialSuperblock <-
    Superblock magic version pageLen pagesPerCluster
      <$> getWord16le
      <*> getWord16le
      <*> getWord32le
      <*> getWord32le
      <*> getWord32le
      <*> getClusterIndex
      <*> getWord32le
      <*> getWord32le
  skip 8 -- padding
  fullSuperblock <-
    partialSuperblock
      <$> getIndirectFatClusterList
      <*> getBadBlockList
      <*> getWord8
      <*> getWord8
      <*> pure
        (mkMetadata
          MkMetadataParams
            { mmpPageLen = pageLen
            , mmpPagesPerCluster = pagesPerCluster
            }
        )
  skip 2 -- padding
  pure fullSuperblock
  where
    getBadBlockList :: Get [Word32]
    getBadBlockList = replicateM 32 getWord32le

putSuperblock :: Superblock -> Put
putSuperblock sb =
  putMagicString sMagic
    >> putVersion sVersion
    >> putWord16le sPageLen
    >> putWord16le sPagesPerCluster
    >> putWord16le sPagesPerBlock
    >> putWord16le sUnknown
    >> putWord32le sClustersPerCard
    >> putWord32le sAllocOffset
    >> putWord32le sAllocEnd
    >> putClusterIndex sRootDirCluster
    >> putWord32le sBackupBlock1
    >> putWord32le sBackupBlock2
    >> putPaddingBytes 8 -- padding
    >> putIndirectFatClusterList sIfcList
    >> putBadBlockList
    >> putWord8 sCardType
    >> putWord8 sCardFlags
    >> putPaddingBytes 2 -- padding
  where
    Superblock
      { sMagic
      , sVersion
      , sPageLen
      , sPagesPerCluster
      , sPagesPerBlock
      , sUnknown
      , sClustersPerCard
      , sAllocOffset
      , sAllocEnd
      , sRootDirCluster
      , sBackupBlock1
      , sBackupBlock2
      , sIfcList
      , sBadBlockList
      , sCardType
      , sCardFlags
      } = sb

    putBadBlockList :: Put
    putBadBlockList = mapM_ putWord32le sBadBlockList

    putPaddingBytes :: Int -> Put
    putPaddingBytes = flip replicateM_ (putWord8 0)
