-- | Types and functions pertaining to the indirect file allocation table
-- (\"FAT\").
module PlayStation2.MemoryCard.Fat.Indirect
  ( IndirectFatValue (..)
  , indirectFatValueFromWord32
  , indirectFatValueToClusterIndex

  , IndirectFatCluster (..)
  ) where

import Data.Word
  ( Word32 )
import PlayStation2.MemoryCard.Cluster.Index
  ( ClusterIndex (..) )
import Prelude

-- | Value stored in an indirect FAT cluster ('IndirectFatCluster').
data IndirectFatValue
  = -- | Index corresponding to a direct FAT cluster ('DirectFatCluster').
    IndirectFatValueDirectFatClusterIndex !ClusterIndex
  | -- | Unallocated cluster.
    --
    -- This is indicated by the value @0xFFFFFFFF@.
    IndirectFatValueUnallocated
  deriving stock (Show, Eq)

-- | Convert a 'Word32' to an 'IndirectFatValue'.
indirectFatValueFromWord32 :: Word32 -> IndirectFatValue
indirectFatValueFromWord32 w32
  | w32 == 0xFFFFFFFF = IndirectFatValueUnallocated
  | otherwise = IndirectFatValueDirectFatClusterIndex (ClusterIndex w32)

indirectFatValueToClusterIndex :: IndirectFatValue -> Maybe ClusterIndex
indirectFatValueToClusterIndex v = case v of
  IndirectFatValueDirectFatClusterIndex ci -> Just ci
  IndirectFatValueUnallocated -> Nothing

-- | Indirect FAT cluster.
--
-- That is, a cluster consisting of indexes which correspond to direct FAT
-- clusters ('FatCluster').
newtype IndirectFatCluster = IndirectFatCluster
  { unIndirectFatCluster :: [IndirectFatValue] }
  deriving stock (Show, Eq)
