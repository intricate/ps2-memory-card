-- | File allocation table (\"FAT\") operations.
module PlayStation2.MemoryCard.Fat
  ( -- * Finding indirect FAT clusters
    FindIndirectFatClustersError (..)
  , findIndirectFatClusters

    -- * Finding direct FAT clusters
  , FindDirectFatClustersError (..)
  , findDirectFatClusters

    -- * Building the memory card's direct FAT matrix
  , MkMemoryCardFatMatrixError (..)
  , mkMemoryCardFatMatrix

    -- * Finding the root directory entry
  , FindRootDirectoryEntryError (..)
  , findRootDirectoryEntry

    -- * Finding child entries of a directory entry
  , FindChildEntriesError (..)
  , findChildEntries

    -- * Finding file data for a file entry
  , FindFileDataError (..)
  , findFileData
  ) where

import Control.Monad
  ( foldM, replicateM )
import Data.Bifunctor
  ( bimap, first )
import Data.Binary.Get
  ( ByteOffset, getWord32le, runGetOrFail )
import Data.Bits
  ( finiteBitSize )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List.NonEmpty as NE
import Data.Maybe
  ( mapMaybe )
import Data.Tuple.Extra
  ( third )
import Data.Word
  ( Word32 )
import PlayStation2.MemoryCard.Cluster
  ( Cluster (..), FindClusterError (..), findCluster )
import PlayStation2.MemoryCard.Cluster.Index
  ( ClusterIndex (..) )
import qualified PlayStation2.MemoryCard.Cluster.Index as Cluster
import PlayStation2.MemoryCard.Entry
  ( Entry (..), entryByteSize, getEntry, isDirectory, isFile )
import PlayStation2.MemoryCard.Fat.Direct
  ( DirectFatCluster (..), DirectFatValue (..), directFatValueFromWord32 )
import PlayStation2.MemoryCard.Fat.Indirect
  ( IndirectFatCluster (..)
  , IndirectFatValue (..)
  , indirectFatValueFromWord32
  , indirectFatValueToClusterIndex
  )
import PlayStation2.MemoryCard.Fat.Matrix
  ( FatMatrix
  , FatMatrixError
  , FatValueChainLookupError
  , lookupFatValueChain
  , mkFatMatrix
  )
import PlayStation2.MemoryCard.PartialFilesystem
  ( SuperblockAndData (..)
  , SuperblockFatMatrixAndData (..)
  , toSuperblockAndData
  )
import PlayStation2.MemoryCard.Superblock
  ( Superblock (..) )
import PlayStation2.MemoryCard.Superblock.IndirectFatClusterList
  ( unIndirectFatClusterList )
import Prelude

------------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------------

getWord32ValuesFromFatCluster
  :: (Word32 -> a)
  -> Cluster
  -> Either
      (LBS.ByteString, ByteOffset, String)
      (LBS.ByteString, ByteOffset, [a])
getWord32ValuesFromFatCluster f (Cluster clusterData) =
  flip runGetOrFail (LBS.fromStrict clusterData) $ do
    let word32ByteSize :: Int
        word32ByteSize = fst $ finiteBitSize (0 :: Word32) `divMod` 8
    numWord32s <-
      case BS.length clusterData `divMod` word32ByteSize of
        (n, 0) -> pure n
        (_, _remainder) -> fail "FAT cluster is not 32-bit-aligned"
    replicateM numWord32s (f <$> getWord32le)

-- | Interpret a cluster of data as a list of 'Entry's.
getEntriesFromCluster
  :: Cluster
  -> Either
      (LBS.ByteString, ByteOffset, String)
      (LBS.ByteString, ByteOffset, [Entry])
getEntriesFromCluster (Cluster clusterData) = do
  let numEntries = fst $ BS.length clusterData `divMod` entryByteSize
  runGetOrFail
    (replicateM numEntries getEntry)
    (LBS.fromStrict clusterData)

------------------------------------------------------------------------------
-- Finding indirect FAT clusters
------------------------------------------------------------------------------

-- | Error finding a memory card's indirect FAT clusters.
data FindIndirectFatClustersError
  = -- | Error finding a cluster.
    FindIndirectFatClustersFindClusterError !FindClusterError
  | -- | Error interpreting a cluster of data as a list of
    -- 'IndirectFatValue's.
    FindIndirectFatClustersGetIndirectFatValuesError !(LBS.ByteString, ByteOffset, String)
  deriving stock (Show, Eq)

-- | Find a memory card's indirect FAT clusters.
findIndirectFatClusters :: SuperblockAndData -> Either FindIndirectFatClustersError [IndirectFatCluster]
findIndirectFatClusters sad = do
  -- Indirect FAT clusters represented as raw 'Cluster's of bytes.
  rawIndirectFatClusters <-
    first FindIndirectFatClustersFindClusterError $
      mapM (findCluster sad) indirectFatClusterIndexes

  case mapM (fmap (IndirectFatCluster . third) . getValuesFromFatCluster) rawIndirectFatClusters of
    Left err -> Left (FindIndirectFatClustersGetIndirectFatValuesError err)
    Right res -> pure res
  where
    SuperblockAndData
      { sadSuperblock =
          Superblock
            { sIfcList
            }
      } = sad

    -- List of indexes to indirect FAT clusters.
    --
    -- Note that the @0@ values have been filtered out.
    indirectFatClusterIndexes :: [ClusterIndex]
    indirectFatClusterIndexes = filter ((/= 0) . unClusterIndex) (unIndirectFatClusterList sIfcList)

    -- Interpret a cluster of data as a list of 'IndirectFatValue's.
    getValuesFromFatCluster
      :: Cluster
      -> Either
          (LBS.ByteString, ByteOffset, String)
          (LBS.ByteString, ByteOffset, [IndirectFatValue])
    getValuesFromFatCluster = getWord32ValuesFromFatCluster indirectFatValueFromWord32

------------------------------------------------------------------------------
-- Finding direct FAT clusters
------------------------------------------------------------------------------

-- | Error finding the direct FAT clusters referenced by an indirect FAT
-- cluster.
data FindDirectFatClustersError
  = -- | Error finding a cluster.
    FindDirectFatClustersFindClusterError !FindClusterError
  | -- | Error interpreting a cluster of data as a list of 'DirectFatValue's.
    FindDirectFatClustersGetDirectFatValuesError !(LBS.ByteString, ByteOffset, String)
  deriving stock (Show, Eq)

-- | Find the 'DirectFatCluster's referenced by an 'IndirectFatCluster'.
findDirectFatClusters :: SuperblockAndData -> IndirectFatCluster -> Either FindDirectFatClustersError [DirectFatCluster]
findDirectFatClusters sad (IndirectFatCluster vs) = do
    -- Direct FAT clusters represented as raw 'Cluster's of bytes.
  rawDirectFatClusters <-
    first FindDirectFatClustersFindClusterError $
      mapM (findCluster sad) directFatClusterIndexes

  case mapM (fmap (DirectFatCluster . third) . getValuesFromFatCluster) rawDirectFatClusters of
    Left err -> Left (FindDirectFatClustersGetDirectFatValuesError err)
    Right res ->
      -- TODO: Check that the number of columns is equivalent to 'mFatClusterMatrixColumns'.
      pure res
  where
    directFatClusterIndexes :: [ClusterIndex]
    directFatClusterIndexes = mapMaybe indirectFatValueToClusterIndex vs

    -- Interpret a cluster of data as a list of 'DirectFatValue's.
    getValuesFromFatCluster
      :: Cluster
      -> Either
          (LBS.ByteString, ByteOffset, String)
          (LBS.ByteString, ByteOffset, [DirectFatValue])
    getValuesFromFatCluster = getWord32ValuesFromFatCluster directFatValueFromWord32

------------------------------------------------------------------------------
-- Memory card direct FAT matrix
------------------------------------------------------------------------------

-- | Error constructing a memory card's direct FAT matrix.
data MkMemoryCardFatMatrixError
  = -- | Error finding the memory card's indirect FAT clusters.
    MkMemoryCardFatMatrixFindIndirectFatClustersError !FindIndirectFatClustersError
  | -- | Error finding the direct FAT clusters referenced by an indirect FAT
    -- cluster.
    MkMemoryCardFatMatrixFindDirectFatClustersError !FindDirectFatClustersError
  | -- | No direct FAT clusters could be found on the memory card.
    MkMemoryCardFatMatrixNoDirectFatClustersError
  | -- | Error constructing a FAT matrix.
    MkMemoryCardFatMatrixFatMatrixError !FatMatrixError
  deriving stock (Show, Eq)

-- | Construct a memory card's 'FatMatrix'.
mkMemoryCardFatMatrix :: SuperblockAndData -> Either MkMemoryCardFatMatrixError FatMatrix
mkMemoryCardFatMatrix sad = do
  ifcs <- first MkMemoryCardFatMatrixFindIndirectFatClustersError (findIndirectFatClusters sad)
  dfcs <- concat <$> foldM findAndAccumDirectFatClusters [] ifcs
  nonEmptyDfcs <-
    case NE.nonEmpty dfcs of
      Just x -> Right x
      Nothing -> Left MkMemoryCardFatMatrixNoDirectFatClustersError
  first MkMemoryCardFatMatrixFatMatrixError (mkFatMatrix nonEmptyDfcs)
  where
    findAndAccumDirectFatClusters
      :: [[DirectFatCluster]]
      -> IndirectFatCluster
      -> Either MkMemoryCardFatMatrixError [[DirectFatCluster]]
    findAndAccumDirectFatClusters acc ifc =
      case findDirectFatClusters sad ifc of
        Left err -> Left (MkMemoryCardFatMatrixFindDirectFatClustersError err)
        Right dfcs -> Right (acc ++ [dfcs])

------------------------------------------------------------------------------
-- Finding the root directory entry
------------------------------------------------------------------------------

-- | Error finding the root directory entry in a memory card.
data FindRootDirectoryEntryError
  = -- | Integer overflow error was encountered when attempting to add an
    -- offset to a 'ClusterIndex'.
    FindRootDirectoryEntryClusterIndexOverflowError
      -- | Cluster index.
      !ClusterIndex
      -- | Offset
      !Word32
  | -- | Error finding a cluster.
    FindRootDirectoryEntryFindClusterError !FindClusterError
  | -- | Error interpreting a cluster of data as an 'Entry'.
    FindRootDirectoryEntryGetEntryError !(LBS.ByteString, ByteOffset, String)
  deriving stock (Show, Eq)

-- | Find the root directory entry of a memory card.
findRootDirectoryEntry :: SuperblockAndData -> Either FindRootDirectoryEntryError Entry
findRootDirectoryEntry sad = do
  -- TODO: Check sAllocEnd
  rootDirClusterIndex <- case Cluster.addOffset sRootDirCluster sAllocOffset of
    Nothing -> Left (FindRootDirectoryEntryClusterIndexOverflowError sRootDirCluster sAllocOffset)
    Just c -> Right c

  rootDirCluster <- first FindRootDirectoryEntryFindClusterError (findCluster sad rootDirClusterIndex)

  bimap FindRootDirectoryEntryGetEntryError third (getRootDirEntry rootDirCluster)
  where
    SuperblockAndData
      { sadSuperblock =
          Superblock
            { sAllocOffset
            , sRootDirCluster
            }
      } = sad

    getRootDirEntry
      :: Cluster
      -> Either
          (LBS.ByteString, ByteOffset, String)
          (LBS.ByteString, ByteOffset, Entry)
    getRootDirEntry (Cluster clusterData) =
      flip runGetOrFail (LBS.fromStrict clusterData) getEntry

------------------------------------------------------------------------------
-- Finding child directory entries
------------------------------------------------------------------------------

-- | Error finding child entries.
data FindChildEntriesError
  = -- | Provided 'Entry' is not a directory.
    FindChildEntriesEntryIsNotDirectoryError !Entry
  | -- | Error looking up a chain of FAT values.
    FindChildEntriesFatValueChainLookupError !FatValueChainLookupError
  | -- | Error interpreting a cluster of data as a list of 'Entry's
    FindChildEntriesGetEntriesFromClusterError !ClusterIndex !(LBS.ByteString, ByteOffset, String)
  | -- | Integer overflow error was encountered when attempting to add an
    -- offset to a 'ClusterIndex'.
    FindChildEntriesClusterIndexOverflowError
      -- | Cluster index.
      !ClusterIndex
      -- | Offset.
      !Word32
  | -- | Error finding a cluster.
    FindChildEntriesFindClusterError !FindClusterError
  deriving stock (Show, Eq)

-- | Given a directory 'Entry', find all other 'Entry's in that directory.
findChildEntries
  :: SuperblockFatMatrixAndData
  -> Entry
  -> Either FindChildEntriesError [Entry]
findChildEntries sfmad e = do
  if not (isDirectory e)
    then Left (FindChildEntriesEntryIsNotDirectoryError e)
    else Right ()

  if eLength < 1
    then Right []
    else do
      chain <- first FindChildEntriesFatValueChainLookupError (lookupFatValueChain eCluster sfmadFatMatrix)
      take (fromIntegral eLength) <$> foldM getAndAccumEntries [] chain
  where
    SuperblockFatMatrixAndData
      { sfmadSuperblock =
          Superblock
            { sAllocOffset
            }
      , sfmadFatMatrix
      } = sfmad

    Entry
      { eLength
      , eCluster
      } = e

    getAndAccumEntries :: [Entry] -> ClusterIndex -> Either FindChildEntriesError [Entry]
    getAndAccumEntries acc ixNoOffset = do
      ixWithOffset <-
        case Cluster.addOffset ixNoOffset sAllocOffset of
          Just x -> Right x
          Nothing -> Left (FindChildEntriesClusterIndexOverflowError ixNoOffset sAllocOffset)
      cluster <- first FindChildEntriesFindClusterError $ findCluster (toSuperblockAndData sfmad) ixWithOffset
      case getEntriesFromCluster cluster of
        Left err -> Left (FindChildEntriesGetEntriesFromClusterError ixWithOffset err)
        Right (_, _, entries) -> Right (acc ++ entries)

------------------------------------------------------------------------------
-- Finding file data
------------------------------------------------------------------------------

-- | Error reading file data.
data FindFileDataError
  = -- | Provided 'Entry' does not correspond to a file.
    FindFileDataEntryIsNotFileError !Entry
  | -- | File 'Entry' has an invalid length
    FindFileDataInvalidFileLengthError !Entry
  | -- | Error looking up a chain of FAT values.
    FindFileDataFatValueChainLookupError !FatValueChainLookupError
  | -- | Integer overflow error was encountered when attempting to add an
    -- offset to a 'ClusterIndex'.
    FindFileDataClusterIndexOverflowError
      -- | Cluster index.
      !ClusterIndex
      -- | Offset.
      !Word32
  | -- | Error finding a cluster.
    FindFileDataFindClusterError !FindClusterError
  deriving stock (Show, Eq)

-- | Given a file 'Entry', find its corresponding file data.
findFileData
  :: SuperblockFatMatrixAndData
  -> Entry
  -> Either FindFileDataError BS.ByteString
findFileData sfmad e = do
  if not (isFile e)
    then Left (FindFileDataEntryIsNotFileError e)
    else Right ()

  if eLength < 1
    then Left (FindFileDataInvalidFileLengthError e)
    else Right ()

  chain <- first FindFileDataFatValueChainLookupError (lookupFatValueChain eCluster sfmadFatMatrix)
  BS.take (fromIntegral eLength) <$> foldM getAndAccumBytes BS.empty chain
  where
    SuperblockFatMatrixAndData
      { sfmadSuperblock =
          Superblock
            { sAllocOffset
            }
      , sfmadFatMatrix
      } = sfmad

    Entry
      { eLength
      , eCluster
      } = e

    getAndAccumBytes :: BS.ByteString -> ClusterIndex -> Either FindFileDataError BS.ByteString
    getAndAccumBytes acc ixNoOffset = do
      ixWithOffset <-
        case Cluster.addOffset ixNoOffset sAllocOffset of
          Just x -> Right x
          Nothing -> Left (FindFileDataClusterIndexOverflowError ixNoOffset sAllocOffset)
      cluster <- first FindFileDataFindClusterError $ findCluster (toSuperblockAndData sfmad) ixWithOffset
      Right (acc <> unCluster cluster)
