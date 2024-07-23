-- | Types and functions pertaining to the direct file allocation table
-- (\"FAT\").
module PlayStation2.MemoryCard.Fat.Direct
  ( DirectFatValue (..)
  , allocatedBitmask
  , directFatValueFromWord32

  , DirectFatCluster (..)
  ) where

import Data.Bits
  ( xor, (.&.) )
import Data.Word
  ( Word32 )
import PlayStation2.MemoryCard.Cluster.Index
  ( ClusterIndex (..) )
import Prelude

-- | Value stored in a direct FAT cluster 'DirectFatCluster'.
data DirectFatValue
  = -- | Index corresponding to an allocated cluster.
    --
    -- This is indicated by the most significant bit of the value being set.
    -- This can be checked using the bitmask @0x80000000@
    -- ('allocatedBitmask').
    DirectFatValueClusterIndex !ClusterIndex
  | -- | Unallocated cluster.
    --
    -- This is indicated by the most significant bit of the value being clear.
    -- This can be checked using the bitmask @0x80000000@
    -- ('allocatedBitmask').
    DirectFatValueUnallocated
  | -- | Value indicating the end of a FAT cluster chain.
    --
    -- This is indicated by the value @0xFFFFFFFF@.
    DirectFatValueEndOfChain
  deriving stock (Show, Eq)

-- | Bitmask which can be used to check whether a cluster index corresponds
-- to an allocated cluster in a direct FAT.
allocatedBitmask :: Word32
allocatedBitmask = 0x80000000

-- | Convert a 'Word32' to an 'DirectFatValue'.
directFatValueFromWord32 :: Word32 -> DirectFatValue
directFatValueFromWord32 w32
  | w32 == 0xFFFFFFFF = DirectFatValueEndOfChain
  | w32 .&. allocatedBitmask > 0 =
      DirectFatValueClusterIndex . ClusterIndex $
        w32 `xor` allocatedBitmask
  | otherwise = DirectFatValueUnallocated

-- | Direct FAT cluster.
--
-- That is, a 'Cluster' consisting of FAT values.
newtype DirectFatCluster = DirectFatCluster
  { unDirectFatCluster :: [DirectFatValue] }
  deriving stock (Show, Eq)
