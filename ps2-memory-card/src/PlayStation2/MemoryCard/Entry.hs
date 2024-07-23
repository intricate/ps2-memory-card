module PlayStation2.MemoryCard.Entry
  ( EntryModeFlag (..)
  , toBitmask

  , EntryMode (..)
  , hasFlag
  , toWord16
  , fromWord16
  , getEntryMode
  , putEntryMode

  , TimeOfDay (..)
  , timeOfDayByteSize
  , getTimeOfDay
  , putTimeOfDay

  , Entry (..)
  , entryByteSize
  , isFile
  , isDirectory
  , getEntry
  , putEntry
  ) where

import Control.Monad
  ( replicateM_ )
import Data.Binary.Get
  ( Get, getByteString, getWord16le, getWord32le, getWord8, skip )
import Data.Binary.Put
  ( Put, putByteString, putWord16le, putWord32le, putWord8 )
import Data.Bits
  ( xor, (.&.) )
import Data.ByteString
  ( ByteString )
import Data.Foldable
  ( foldl' )
import Data.Set
  ( Set )
import qualified Data.Set as Set
import Data.Word
  ( Word16, Word32, Word8 )
import PlayStation2.MemoryCard.Cluster.Index
  ( ClusterIndex (..), getClusterIndex, putClusterIndex )
import Prelude

-- | Entry mode flag.
data EntryModeFlag
  = -- | Entry has read permissions.
    EntryModeReadFlag
  | -- | Entry has write permissions.
    EntryModeWriteFlag
  | -- | Entry has execute permissions.
    EntryModeExecuteFlag
  | -- | Entry is protected.
    EntryModeProtectedFlag
  | -- | Entry corresponds to a file.
    EntryModeFileFlag
  | -- | Entry corresponds to a directory.
    EntryModeDirectoryFlag
  | -- | Entry is hidden.
    EntryModeHiddenFlag
  | -- | Entry exists, i.e. it hasn't been deleted.
    EntryModeExistsFlag
  deriving stock (Show, Eq, Ord, Bounded, Enum)

-- | Convert an 'EntryModeFlag' to its bitmask.
toBitmask :: EntryModeFlag -> Word16
toBitmask flag = case flag of
  EntryModeReadFlag -> 0x0001
  EntryModeWriteFlag -> 0x0002
  EntryModeExecuteFlag -> 0x0004
  EntryModeProtectedFlag -> 0x0008
  EntryModeFileFlag -> 0x0010
  EntryModeDirectoryFlag -> 0x0020
  EntryModeHiddenFlag -> 0x2000
  EntryModeExistsFlag -> 0x8000

-- | Entry mode.
newtype EntryMode = EntryMode
  { unEntryMode :: Set EntryModeFlag }
  deriving stock (Show, Eq)

-- | Check whether an 'EntryMode' has a specified 'EntryModeFlag'.
hasFlag :: EntryMode -> EntryModeFlag -> Bool
hasFlag mode flag = flag `Set.member` unEntryMode mode

-- | Convert an 'EntryMode' to a 'Word16'.
toWord16 :: EntryMode -> Word16
toWord16 = foldl' (\acc flag -> acc `xor` toBitmask flag) 0x0000 . unEntryMode

-- | Convert a 'Word16' to an 'EntryMode'.
fromWord16 :: Word16 -> EntryMode
fromWord16 w16 = foldl' f (EntryMode Set.empty) [minBound .. maxBound]
  where
    f :: EntryMode -> EntryModeFlag -> EntryMode
    f (EntryMode accFlags) flag =
      if w16 .&. toBitmask flag > 0
        then EntryMode (Set.insert flag accFlags)
        else EntryMode accFlags

getEntryMode :: Get EntryMode
getEntryMode = fromWord16 <$> getWord16le

putEntryMode :: EntryMode -> Put
putEntryMode = putWord16le . toWord16

-- | Time of day.
data TimeOfDay = TimeOfDay
  { todSeconds :: !Word8
  , todMinutes :: !Word8
  , todHours :: !Word8
  , todDay :: !Word8
  , todMonth :: !Word8
  , todYear :: !Word16
  } deriving stock (Show, Eq)

-- | Size of a binary serialized 'TimeOfDay' in bytes.
timeOfDayByteSize :: Int
timeOfDayByteSize = 8

getTimeOfDay :: Get TimeOfDay
getTimeOfDay = do
  skip 1 -- padding
  TimeOfDay
    <$> getWord8
    <*> getWord8
    <*> getWord8
    <*> getWord8
    <*> getWord8
    <*> getWord16le

putTimeOfDay :: TimeOfDay -> Put
putTimeOfDay tod =
  putPaddingBytes 1 -- padding
    >> putWord8 todSeconds
    >> putWord8 todMinutes
    >> putWord8 todHours
    >> putWord8 todDay
    >> putWord8 todMonth
    >> putWord16le todYear
  where
    TimeOfDay
      { todSeconds
      , todMinutes
      , todHours
      , todDay
      , todMonth
      , todYear
      } = tod

    putPaddingBytes :: Int -> Put
    putPaddingBytes = flip replicateM_ (putWord8 0)

-- | Memory card filesystem entry (metadata about a directory or file).
data Entry = Entry
  { -- | Entry mode.
    eMode :: !EntryMode
  , -- | Length of the filesystem object that this entry describes.
    --
    -- If this entry corresponds to a file, then this value is that file's
    -- length in bytes. However, if it corresponds to a directory, then this
    -- value should be interpreted as number of entries.
    eLength :: !Word32
  , -- | Entry creation time.
    eCreated :: !TimeOfDay
  , -- | First cluster corresponding to this entry.
    --
    -- Note that this index is relative to 'sAllocOffset'.
    eCluster :: !ClusterIndex
  , -- | Unused.
    --
    -- Only in \".\" entries. Entry of this directory in its parent's directory.
    eDirEntry :: !Word32
  , -- | Entry modification time.
    eModified :: !TimeOfDay
  , -- | User attributes.
    eAttr :: !Word32
  , -- | Null-terminated name for this file/directory.
    eName :: !ByteString
  } deriving stock (Show, Eq)

-- | Check whether an 'Entry' corresponds to a directory.
isDirectory :: Entry -> Bool
isDirectory Entry{ eMode } = eMode `hasFlag` EntryModeDirectoryFlag

-- | Check whether an 'Entry' corresponds to a file.
isFile :: Entry -> Bool
isFile Entry{ eMode } = eMode `hasFlag` EntryModeFileFlag

-- | Size of a binary serialized 'Entry' in bytes.
entryByteSize :: Int
entryByteSize = 512

getEntry :: Get Entry
getEntry = do
  mode <- getEntryMode
  skip 2 -- padding
  entryLength <- getWord32le
  created <- getTimeOfDay
  cluster <- getClusterIndex
  dirEntry <- getWord32le
  modified <- getTimeOfDay
  attr <- getWord32le
  skip 28 -- padding
  name <- getName
  skip 416 -- padding
  pure Entry
    { eMode = mode
    , eLength = entryLength
    , eCreated = created
    , eCluster = cluster
    , eDirEntry = dirEntry
    , eModified = modified
    , eAttr = attr
    , eName = name
    }
  where
    getName :: Get ByteString
    getName = getByteString 32

putEntry :: Entry -> Put
putEntry e =
  putEntryMode eMode
    >> putPaddingBytes 2 -- padding
    >> putWord32le eLength
    >> putTimeOfDay eCreated
    >> putClusterIndex eCluster
    >> putWord32le eDirEntry
    >> putTimeOfDay eModified
    >> putWord32le eAttr
    >> putPaddingBytes 28
    >> putName
    >> putPaddingBytes 416 -- padding
  where
    Entry
      { eMode
      , eLength
      , eCreated
      , eCluster
      , eDirEntry
      , eModified
      , eAttr
      , eName
      } = e

    putName :: Put
    putName = putByteString eName

    putPaddingBytes :: Int -> Put
    putPaddingBytes = flip replicateM_ (putWord8 0)
