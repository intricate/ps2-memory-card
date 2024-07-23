module PlayStation2.MemoryCard.Cluster.Index
  ( ClusterIndex (..)
  , getClusterIndex
  , putClusterIndex
  , addOffset
  , subOffset
  , mul
  ) where

import Data.Binary.Get
  ( Get, getWord32le )
import Data.Binary.Put
  ( Put, putWord32le )
import Data.Word
  ( Word32 )
import Prelude

-- | Cluster index.
newtype ClusterIndex = ClusterIndex
  { unClusterIndex :: Word32 }
  deriving stock (Show, Eq)

getClusterIndex :: Get ClusterIndex
getClusterIndex = ClusterIndex <$> getWord32le

putClusterIndex :: ClusterIndex -> Put
putClusterIndex = putWord32le . unClusterIndex

-- | Safely add an offset to a 'ClusterIndex'.
--
-- If this operation were to overflow, the result is 'Nothing'.
addOffset :: ClusterIndex -> Word32 -> Maybe ClusterIndex
addOffset (ClusterIndex ix) offset
  | maxBound >= ix && offset <= (maxBound - ix) =
      Just $ ClusterIndex (ix + offset)
  | otherwise = Nothing

-- | Safely subtract an offset from a 'ClusterIndex'.
--
-- If this operation were to underflow, the result is 'Nothing'.
subOffset :: ClusterIndex -> Word32 -> Maybe ClusterIndex
subOffset (ClusterIndex ix) offset
  | ix >= minBound && offset <= (ix - minBound) =
      Just $ ClusterIndex (ix - offset)
  | otherwise = Nothing

-- | Safely multiply a 'ClusterIndex' by a 'Word32'.
--
-- If this operation were to overflow, the result is 'Nothing'.
mul :: ClusterIndex -> Word32 -> Maybe ClusterIndex
mul (ClusterIndex ix) b = case maxBound `divMod` b of
  (quotient, _)
    | ix > quotient -> Nothing
    | otherwise -> Just $ ClusterIndex (ix * b)
