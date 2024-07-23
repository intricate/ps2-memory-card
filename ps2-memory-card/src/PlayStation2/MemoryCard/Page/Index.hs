module PlayStation2.MemoryCard.Page.Index
  ( PageIndex (..)
  , addOffset
  , mul
  ) where

import Data.Word
  ( Word32 )
import Prelude

-- | Page index.
newtype PageIndex = PageIndex
  { unPageIndex :: Word32 }
  deriving stock (Show, Eq)

-- | Safely add an offset to a 'PageIndex'.
--
-- If this operation were to overflow, the result is 'Nothing'.
addOffset :: PageIndex -> Word32 -> Maybe PageIndex
addOffset (PageIndex ix) offset
  | maxBound >= ix && offset <= (maxBound - ix) =
      Just $ PageIndex (ix + offset)
  | otherwise = Nothing

-- | Safely multiply a 'PageIndex' by a 'Word32'.
--
-- If this operation were to overflow, the result is 'Nothing'.
mul :: PageIndex -> Word32 -> Maybe PageIndex
mul (PageIndex ix) b = case maxBound `divMod` b of
  (quotient, _)
    | ix > quotient -> Nothing
    | otherwise -> Just $ PageIndex (ix * b)
