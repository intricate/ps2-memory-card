# ps2-memory-card

PlayStation 2 memory card filesystem library written in Haskell.

Note that this library isn't exactly "finished" or fully-featured. I was mostly just curious about the PS2 memory card filesystem and, also, I thought it'd be cool to explore [PCSX2](https://pcsx2.net/) `*.ps2` images in Haskell.

## Getting Started

### Usage

Reading the memory card filesystem:

```haskell
import PlayStation2.MemoryCard
  ( Filesystem (..)
  , ReadFilesystemError
  , readFilesystem
  )
import Prelude
import System.IO.MMap (mmapFileByteString)

f :: IO (Either ReadFilesystemError Filesystem)
f = do
  bs <- mmapFileByteString "./memcard.ps2" Nothing
  pure (readFilesystem bs)
```

From there, you could list the contents of a directory (e.g. the root directory which is available via the `fRootDirectoryEntry` field of `Filesystem`):

```haskell
listDirectoryContents fs (fRootDirectoryEntry fs)
```

... or build a `Tree` of directory contents:

```haskell
buildDirectoryTree fs (fRootDirectoryEntry fs)
```

... or read a specific file, given a reference to it:

```haskell
readFile fs someFileReference
```

## Building

Building with `cabal`:

```
cabal build ps2-memory-card
```

## Acknowledgments

- Ross Ridge ([`mymc`](http://www.csclub.uwaterloo.ca:11068/mymc/) and [PlayStation 2 Memory Card File System ](https://www.ps2savetools.com/ps2memcardformat.html))
- Florian Märkl ([`mymcplus`](https://git.sr.ht/~thestr4ng3r/mymcplus))
- [caol64](https://github.com/caol64) ([`ps2mc-browser`](https://github.com/caol64/ps2mc-browser) and [Analysis of the PS2 Memory Card Storage Format](https://babyno.top/en/posts/2023/09/parsing-ps2-memcard-file-system/))

## License

[Apache 2.0](LICENSE)
