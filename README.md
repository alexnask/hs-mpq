libmpq
======

libmpq is a library for manipulating MPQ (MoPaQ) archives, which are used by Blizzard in most of their games (e.g., Diablo, Diablo 2, StarCraft, WarCraft 3, and World of Warcraft).

More info on the official [github repo](https://github.com/mbroemme/libmpq)

Use hsc2hs to convert mpq.hsc to a hs file, then import it as a module for your projects! :D

Here is some example code:
<pre lang="haskell">
{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import MPQ
import Data.ByteString.Char8
import System.Environment

main :: IO ()
main = do
           let Left archive = openArchive "test.SC2Replay"
           print $ "Archive: " ++ (show archive)
           let Just psize = archivePackedSize archive
           print $ "Packed size: " ++ (show psize)
           let Just usize = archiveUnpackedSize archive
           print $ "Unpacked size: " ++ (show usize)
           let Just version = archiveVersion archive
           print $ "Version: " ++ (show version)
           let Just filenum = archiveFileNum archive
           print $ "Number of files: " ++ (show filenum)
           let Just offset = archiveOffset archive
           print $ "Archive offset: " ++ (show offset)
           let Just fnum = fileNumber archive "replay.initData"
           print $ "replay.initData number: " ++ (show fnum)
           let Just fsize = fileSizeUnpacked archive (fromIntegral fnum)
           print $ "replay.initData unpacked size: " ++ (show fsize)
           let Just file = fileContents archive (fromIntegral fnum)
           print $ "replay.initData: " ++ (show file)
           closeArchive archive
</pre>

