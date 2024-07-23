-- | Operations pertaining to matrices of direct file allocation table
-- (\"FAT\") clusters.
module PlayStation2.MemoryCard.Fat.Matrix
  ( FatMatrix
  , unFatMatrix
  , mkFatMatrix
  , lookup
  , lookupFatValue
  , lookupFatValueChain

  , FatMatrixError (..)
  , renderFatMatrixError

  , FatValueChainLookupError (..)
  , renderFatValueChainLookupError
  ) where

import Data.List.NonEmpty
  ( NonEmpty )
import qualified Data.List.NonEmpty as NE
import Data.Matrix
  ( Matrix )
import qualified Data.Matrix as Matrix
import Data.Text
  ( Text )
import PlayStation2.MemoryCard.Cluster.Index
  ( ClusterIndex (..) )
import PlayStation2.MemoryCard.Fat.Direct
  ( DirectFatCluster (..), DirectFatValue (..) )
import Prelude hiding
  ( lookup )

-- | File allocation table (\"FAT\") matrix.
newtype FatMatrix = FatMatrix
  { unFatMatrix :: Matrix DirectFatValue }
  deriving stock (Show, Eq)

-- | Error constructing a FAT matrix.
data FatMatrixError
  = -- | One or more rows in the table have a different length.
    FatMatrixInvalidRowLengthError
  deriving stock (Show, Eq)

-- | Render a 'FatMatrixError' as human-readable 'Text'.
renderFatMatrixError :: FatMatrixError -> Text
renderFatMatrixError err = case err of
  FatMatrixInvalidRowLengthError -> "One or more rows in the table have a different length."

-- | Construct a FAT matrix from a non-empty list of 'DirectFatCluster's.
mkFatMatrix :: NonEmpty DirectFatCluster -> Either FatMatrixError FatMatrix
mkFatMatrix clusters = do
  let valueTable :: NonEmpty [DirectFatValue]
      valueTable@(firstRow NE.:| rows) = NE.map unDirectFatCluster clusters

  -- Validate that each row is of the same length.
  let firstRowLength = length firstRow
  if all ((== firstRowLength) . length) rows
    then pure ()
    else Left FatMatrixInvalidRowLengthError

  Right . FatMatrix . Matrix.fromLists . NE.toList $ valueTable

-- | Lookup a value in a 'FatMatrix'.
lookup
  :: Int
  -- ^ Row
  -> Int
  -- ^ Column
  -> FatMatrix
  -> Maybe DirectFatValue
lookup row col (FatMatrix matrix) = Matrix.safeGet row col matrix

-- | Lookup the FAT value for a specific cluster in a 'FatMatrix'.
lookupFatValue :: ClusterIndex -> FatMatrix -> Maybe DirectFatValue
lookupFatValue (ClusterIndex ix) fatMatrix@(FatMatrix matrix) =
  let numColumns = Matrix.ncols matrix
      -- Data.Matrix rows and columns are indexed starting by 1, so we add 1.
      row = (fst (fromIntegral ix `divMod` numColumns) `mod` numColumns) + 1
      col = (fromIntegral ix `mod` numColumns) + 1
  in lookup row col fatMatrix

-- | Error looking up a chain of FAT values.
data FatValueChainLookupError
  = -- | FAT value for a specific cluster could not be found in the
    -- 'FatMatrix'.
    FatValueChainLookupValueNotFoundError
  | -- | Reference to an unallocated cluster was found in the chain.
    FatValueChainLookupUnallocatedClusterError
  deriving stock (Show, Eq)

-- | Render a 'FatValueChainLookupError' as human-readable 'Text'.
renderFatValueChainLookupError :: FatValueChainLookupError -> Text
renderFatValueChainLookupError err = case err of
  FatValueChainLookupValueNotFoundError -> "Value for the specified cluster could not be found in the FAT."
  FatValueChainLookupUnallocatedClusterError -> "An unallocated cluster was referenced in the FAT value chain."

-- | Lookup a chain of FAT values from a 'FatMatrix' given the starting
-- 'ClusterIndex'.
lookupFatValueChain
  :: ClusterIndex
  -- ^ Starting index.
  -> FatMatrix
  -- ^ FAT matrix.
  -> Either FatValueChainLookupError [ClusterIndex]
lookupFatValueChain startingIndex matrix =
  go [] (Just (DirectFatValueClusterIndex startingIndex))
  where
    go _ Nothing = Left FatValueChainLookupValueNotFoundError
    go _ (Just DirectFatValueUnallocated) = Left FatValueChainLookupUnallocatedClusterError
    go acc (Just DirectFatValueEndOfChain) = Right acc
    go acc (Just (DirectFatValueClusterIndex next)) =
      go (acc ++ [next]) (lookupFatValue next matrix)
