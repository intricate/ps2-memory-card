-- | Types that represent the PS2 memory card filesystem at different points
-- of processing.
module PlayStation2.MemoryCard.PartialFilesystem
  ( SuperblockAndData (..)
  , mkSuperblockAndData

  , SuperblockFatMatrixAndData (..)
  , toSuperblockAndData
  ) where

import Data.Binary.Get
  ( ByteOffset, runGetOrFail )
import Data.ByteString
  ( ByteString )
import qualified Data.ByteString.Lazy as LBS
import PlayStation2.MemoryCard.Fat.Matrix
  ( FatMatrix )
import PlayStation2.MemoryCard.Superblock
  ( Superblock, getSuperblock )
import Prelude

-- | Partially processed filesystem consisting of the 'Superblock' and raw
-- memory card data.
data SuperblockAndData = SuperblockAndData
  { -- | Raw memory card data.
    sadRawMemoryCardData :: !ByteString
  , -- | Memory card superblock.
    sadSuperblock :: !Superblock
  } deriving stock (Show, Eq)

-- | Construct 'SuperblockAndData' from raw memory card data.
mkSuperblockAndData :: ByteString -> Either (LBS.ByteString, ByteOffset, String) SuperblockAndData
mkSuperblockAndData rawMemoryCardData =
  case runGetOrFail getSuperblock (LBS.fromStrict rawMemoryCardData) of
    Left err -> Left err
    Right (_, _, superblock) ->
      pure SuperblockAndData
        { sadRawMemoryCardData = rawMemoryCardData
        , sadSuperblock = superblock
        }

-- | Partially processed filesystem consisting of the 'Superblock',
-- 'FatMatrix', and raw memory card data.
data SuperblockFatMatrixAndData = SuperblockFatMatrixAndData
  { -- | Raw memory card data.
    sfmadRawMemoryCardData :: ByteString
  , -- | Memory card superblock.
    sfmadSuperblock :: !Superblock
  , -- | Direct FAT matrix.
    sfmadFatMatrix :: !FatMatrix
  } deriving stock (Show, Eq)

toSuperblockAndData :: SuperblockFatMatrixAndData -> SuperblockAndData
toSuperblockAndData sfmad =
  SuperblockAndData
    { sadRawMemoryCardData = sfmadRawMemoryCardData
    , sadSuperblock = sfmadSuperblock
    }
  where
    SuperblockFatMatrixAndData
      { sfmadRawMemoryCardData
      , sfmadSuperblock
      } = sfmad
