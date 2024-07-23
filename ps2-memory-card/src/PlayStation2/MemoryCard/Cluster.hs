module PlayStation2.MemoryCard.Cluster
  ( Cluster (..)
  , FindClusterError (..)
  , findCluster
  ) where

import qualified Data.ByteString as BS
import Data.Word
  ( Word16, Word32 )
import PlayStation2.MemoryCard.Cluster.Index
  ( ClusterIndex (..) )
import qualified PlayStation2.MemoryCard.Cluster.Index as Cluster
import PlayStation2.MemoryCard.Page
  ( FindPageError (..), Page (..), findPage )
import PlayStation2.MemoryCard.Page.Index
  ( PageIndex (..) )
import qualified PlayStation2.MemoryCard.Page.Index as Page
import PlayStation2.MemoryCard.PartialFilesystem
  ( SuperblockAndData (..) )
import PlayStation2.MemoryCard.Superblock
  ( Superblock (..) )
import Prelude

-- | Cluster.
newtype Cluster = Cluster
  { unCluster :: BS.ByteString }
  deriving stock (Show, Eq)

-- | Error finding a cluster in a memory card.
data FindClusterError
  = -- | Integer overflow error was encountered when attempting to multiply a
    -- 'ClusterIndex' by a value.
    FindClusterIndexOverflowError
      -- | Cluster index.
      !ClusterIndex
      -- | Multiple which caused the overflow.
      !Word32
  | -- | Integer overflow error was encountered when attempting to add an
    -- offset to a 'PageIndex'.
    FindClusterPageIndexOverflowError
      -- | Page index.
      !PageIndex
      -- | Offset
      !Word32
  | -- | Error finding a page.
    FindClusterFindPageError !FindPageError
  deriving stock (Show, Eq)

-- | Find a 'Cluster' in a memory card.
findCluster :: SuperblockAndData -> ClusterIndex -> Either FindClusterError Cluster
findCluster sad cIx = do
  firstPageIndex <- case cIx `Cluster.mul` pagesPerClusterW32 of
    Nothing -> Left (FindClusterIndexOverflowError cIx pagesPerClusterW32)
    Just c -> Right . PageIndex $ unClusterIndex c

  Cluster <$> go (Right BS.empty) sPagesPerCluster firstPageIndex
  where
    SuperblockAndData
      { sadSuperblock
      } = sad

    Superblock
      { sPagesPerCluster
      } = sadSuperblock

    pagesPerClusterW32 :: Word32
    pagesPerClusterW32 = fromIntegral sPagesPerCluster

    go :: Either FindClusterError BS.ByteString -> Word16 -> PageIndex -> Either FindClusterError BS.ByteString
    go (Left err) _ _ = Left err
    go (Right acc) 0 _ = Right acc
    go (Right acc) n pIx = do
      page <- case findPage sad pIx of
        Left err -> Left (FindClusterFindPageError err)
        Right p -> Right p

      let offset :: Word32
          offset = 1

      nextPageIndex <- case Page.addOffset pIx offset of
        Nothing -> Left (FindClusterPageIndexOverflowError pIx offset)
        Just p -> Right p

      go
        (Right $ acc <> unPage page)
        (n - 1)
        nextPageIndex
