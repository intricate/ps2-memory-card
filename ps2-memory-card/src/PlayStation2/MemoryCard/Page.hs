module PlayStation2.MemoryCard.Page
  ( Page (..)
  , FindPageError (..)
  , findPage
  ) where

import Data.ByteString
  ( ByteString )
import qualified Data.ByteString as BS
import Data.Word
  ( Word32 )
import PlayStation2.MemoryCard.Page.Index
  ( PageIndex (..) )
import qualified PlayStation2.MemoryCard.Page.Index as Page
import PlayStation2.MemoryCard.PartialFilesystem
  ( SuperblockAndData (..) )
import PlayStation2.MemoryCard.Superblock
  ( Metadata (..), Superblock (..) )
import Prelude

-- | Page.
newtype Page = Page
  { unPage :: ByteString }
  deriving stock (Show, Eq)

-- | Find a 'Page' in a memory card.
--
-- Note that this function does not perform safety checks.
unsafeFindPage :: SuperblockAndData -> PageIndex -> Page
unsafeFindPage sad (PageIndex ix) =
  Page $ BS.take pageLenInt (BS.drop offset sadRawMemoryCardData)
  where
    SuperblockAndData
      { sadRawMemoryCardData
      , sadSuperblock =
          Superblock
            { sPageLen
            , sMetadata =
                Metadata
                  { mPageWithSpareAreaSize
                  }
            }
      } = sad

    pageWithSpareAreaSizeW32 :: Word32
    pageWithSpareAreaSizeW32 = fromIntegral mPageWithSpareAreaSize

    offset :: Int
    offset = fromIntegral (pageWithSpareAreaSizeW32 * ix)

    pageLenInt :: Int
    pageLenInt = fromIntegral sPageLen

-- | Error finding a page in a memory card.
data FindPageError
  = -- | Page index out of bounds error.
    FindPageIndexError
      -- | Index that was out of bounds.
      !PageIndex
      -- | Maximum page index.
      !PageIndex
  | -- | Size of the retrieved page is invalid.
    FindPageInvalidSizeError
  | -- | Integer overflow error was encountered when attempting to multiply a
    -- 'PageIndex' by a value.
    FindPageIndexOverflowError
      -- | Page index.
      !PageIndex
      -- | Multiple which caused the overflow.
      !Word32
  deriving stock (Show, Eq)

-- | Find a 'Page' in a memory card.
findPage :: SuperblockAndData -> PageIndex -> Either FindPageError Page
findPage sad ix = do
  -- Check that this page index exists.
  if unPageIndex ix > unPageIndex maxIndex
    then Left (FindPageIndexError ix maxIndex)
    else Right ()

  pageByteOffsetW32 <-
    case ix `Page.mul` pageWithSpareAreaSizeW32 of
      Nothing -> Left (FindPageIndexOverflowError ix pageWithSpareAreaSizeW32)
      Just x -> Right (unPageIndex x)

  -- Check that the page is the correct size.
  if memCardSizeW32 - pageByteOffsetW32 < pageWithSpareAreaSizeW32
    then Left FindPageInvalidSizeError
    else Right ()

  Right (unsafeFindPage sad ix)
  where
    SuperblockAndData
      { sadRawMemoryCardData
      , sadSuperblock =
          Superblock
            { sMetadata =
                Metadata
                  { mPageWithSpareAreaSize
                  }
            }
      } = sad

    memCardSizeW32 :: Word32
    memCardSizeW32 = fromIntegral (BS.length sadRawMemoryCardData)

    pageWithSpareAreaSizeW32 :: Word32
    pageWithSpareAreaSizeW32 = fromIntegral mPageWithSpareAreaSize

    maxIndex :: PageIndex
    maxIndex = PageIndex . fst $ memCardSizeW32 `divMod` pageWithSpareAreaSizeW32
