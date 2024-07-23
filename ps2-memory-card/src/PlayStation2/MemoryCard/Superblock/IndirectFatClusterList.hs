module PlayStation2.MemoryCard.Superblock.IndirectFatClusterList
  ( IndirectFatClusterList
  , mkIndirectFatClusterList
  , unIndirectFatClusterList
  , getIndirectFatClusterList
  , putIndirectFatClusterList
  ) where

import Control.Monad
  ( replicateM )
import Data.Binary.Get
  ( Get )
import Data.Binary.Put
  ( Put )
import PlayStation2.MemoryCard.Cluster.Index
  ( ClusterIndex (..), getClusterIndex, putClusterIndex )
import Prelude

-- | List of indirect FAT clusters.
newtype IndirectFatClusterList = IndirectFatClusterList [ClusterIndex]
  deriving stock (Show, Eq)

-- | Construct an 'IndirectFatClusterList'.
--
-- Note that an indirect FAT cluster list /must/ consist of @32@
-- 'ClusterIndex's.
mkIndirectFatClusterList :: [ClusterIndex] -> Maybe IndirectFatClusterList
mkIndirectFatClusterList ws
    | length ws == 32 = Just (IndirectFatClusterList ws)
    | otherwise = Nothing

-- | Get the raw list of 'ClusterIndex's from an 'IndirectFatClusterList'.
unIndirectFatClusterList :: IndirectFatClusterList -> [ClusterIndex]
unIndirectFatClusterList (IndirectFatClusterList xs) = xs

getIndirectFatClusterList :: Get IndirectFatClusterList
getIndirectFatClusterList = IndirectFatClusterList <$> replicateM 32 getClusterIndex

putIndirectFatClusterList :: IndirectFatClusterList -> Put
putIndirectFatClusterList (IndirectFatClusterList ifcs) = mapM_ putClusterIndex ifcs
