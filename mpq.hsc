{-# LANGUAGE CPP, ForeignFunctionInterface, TypeSynonymInstances #-}

#include <libmpq/mpq.h>
module MPQ where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.ForeignPtr
import Data.ByteString
import Foreign.Marshal.Alloc
import Data.Either
import Data.Maybe


newtype MPQError = MPQError { unMPQError :: CInt }
    deriving (Eq, Show)

#{enum MPQError, MPQError
  , openError          = LIBMPQ_ERROR_OPEN
  , closeError         = LIBMPQ_ERROR_CLOSE
  , seekError          = LIBMPQ_ERROR_SEEK
  , readError          = LIBMPQ_ERROR_READ
  , writeError         = LIBMPQ_ERROR_WRITE
  , mallocError        = LIBMPQ_ERROR_MALLOC
  , formatError        = LIBMPQ_ERROR_FORMAT
  , notInitializedError= LIBMPQ_ERROR_NOT_INITIALIZED
  , sizeError          = LIBMPQ_ERROR_SIZE
  , existError         = LIBMPQ_ERROR_EXIST
  , decryptError       = LIBMPQ_ERROR_DECRYPT
  , unpackError        = LIBMPQ_ERROR_UNPACK
  }

type MPQArchive = ()

foreign import ccall unsafe "libmpq/mpq.h libmpq__archive_open"
    c_archive_open  :: Ptr (Ptr MPQArchive)
                    -> CString
                    -> CLLong
                    -> IO CInt

foreign import ccall unsafe "libmpq/mpq.h libmpq__archive_close"
    c_archive_close :: Ptr MPQArchive
                    -> IO CInt

foreign import ccall unsafe "libmpq/mpq.h libmpq__archive_size_packed"
    c_archive_size_packed  :: Ptr MPQArchive
                           -> Ptr CInt
                           -> IO CInt

foreign import ccall unsafe "libmpq/mpq.h libmpq__archive_size_unpacked"
    c_archive_size_unpacked  :: Ptr MPQArchive
                             -> Ptr CInt
                             -> IO CInt

foreign import ccall unsafe "libmpq/mpq.h libmpq__archive_offset"
    c_archive_offset  :: Ptr MPQArchive
                      -> Ptr CInt
                      -> IO CInt

foreign import ccall unsafe "libmpq/mpq.h libmpq__archive_version"
    c_archive_version  :: Ptr MPQArchive
                       -> Ptr CUInt
                       -> IO CInt

foreign import ccall unsafe "libmpq/mpq.h libmpq__archive_files"
    c_archive_files  :: Ptr MPQArchive
                     -> Ptr CUInt
                     -> IO CInt

foreign import ccall unsafe "libmpq/mpq.h libmpq__file_size_packed"
    c_file_size_packed :: Ptr MPQArchive
                       -> CUInt
                       -> Ptr CInt
                       -> IO CInt

foreign import ccall unsafe "libmpq/mpq.h libmpq__file_size_unpacked"
    c_file_size_unpacked :: Ptr MPQArchive
                         -> CUInt
                         -> Ptr CInt
                         -> IO CInt

foreign import ccall unsafe "libmpq/mpq.h libmpq__file_offset"
    c_file_offset   :: Ptr MPQArchive
                    -> CUInt
                    -> Ptr CInt
                    -> IO CInt

foreign import ccall unsafe "libmpq/mpq.h libmpq__file_blocks"
    c_file_blocks   :: Ptr MPQArchive
                    -> CUInt
                    -> Ptr CUInt
                    -> IO CInt

foreign import ccall unsafe "libmpq/mpq.h libmpq__file_encrypted"
    c_file_encrypted  :: Ptr MPQArchive
                      -> CUInt
                      -> Ptr CUInt
                      -> IO CInt

foreign import ccall unsafe "libmpq/mpq.h libmpq__file_compressed"
    c_file_compressed  :: Ptr MPQArchive
                       -> CUInt
                       -> Ptr CUInt
                       -> IO CInt

foreign import ccall unsafe "libmpq/mpq.h libmpq__file_imploded"
    c_file_imploded :: Ptr MPQArchive
                    -> CUInt
                    -> Ptr CUInt
                    -> IO CInt

foreign import ccall unsafe "libmpq/mpq.h libmpq__file_number"
    c_file_number :: Ptr MPQArchive
                  -> CString
                  -> Ptr CUInt
                  -> IO CInt

foreign import ccall unsafe "libmpq/mpq.h libmpq__file_read"
    c_file_read :: Ptr MPQArchive
                -> CUInt
                -> CString
                -> CInt
                -> Ptr CInt
                -> IO CInt

data Archive = Archive !(Ptr MPQArchive)
                       !ByteString
                deriving (Eq, Show)

openArchive :: ByteString -> Either Archive String
openArchive path = unsafePerformIO $
                        useAsCString path $ \cpath -> do
                            carchive <- malloc :: IO (Ptr (Ptr MPQArchive))
                            code <- c_archive_open carchive cpath (fromIntegral (- 1))
                            if code < 0 then do
                                return $ Right "Error while opening archive" -- Todo - add more specific error
                            else do
                                return $ Left (Archive (unsafePerformIO $ peek carchive) path)                 

closeArchive :: Archive -> IO ()
closeArchive (Archive carchive _) = do
                                        code <- c_archive_close carchive
                                        return ()

archiveUnpackedSize :: Archive -> Maybe CInt
archiveUnpackedSize (Archive carchive _) = unsafePerformIO $ do
                                               size <- mallocForeignPtr :: IO (ForeignPtr CInt)
                                               poke (unsafeForeignPtrToPtr size) 0
                                               code <- c_archive_size_unpacked carchive (unsafeForeignPtrToPtr size)
                                               return $ if code < 0 then do
                                                            Nothing
                                                        else
                                                            Just $ unsafePerformIO $ peek $ unsafeForeignPtrToPtr size

archivePackedSize :: Archive -> Maybe CInt
archivePackedSize (Archive carchive _) = unsafePerformIO $ do
                                               size <- mallocForeignPtr :: IO (ForeignPtr CInt)
                                               poke (unsafeForeignPtrToPtr size) 0
                                               code <- c_archive_size_packed carchive (unsafeForeignPtrToPtr size)
                                               return $ if code < 0 then do
                                                            Nothing
                                                        else
                                                            Just $ unsafePerformIO $ peek $ unsafeForeignPtrToPtr size

archiveVersion :: Archive -> Maybe CUInt
archiveVersion (Archive carchive _) = unsafePerformIO $ do
                                            version <- mallocForeignPtr :: IO (ForeignPtr CUInt)
                                            poke (unsafeForeignPtrToPtr version) 0
                                            code <- c_archive_version carchive (unsafeForeignPtrToPtr version)
                                            return $ if code < 0 then
                                                         Nothing
                                                     else
                                                         Just $ unsafePerformIO $ peek $ unsafeForeignPtrToPtr version

archiveFileNum :: Archive -> Maybe CUInt
archiveFileNum (Archive carchive _) = unsafePerformIO $ do
                                            files <- mallocForeignPtr :: IO (ForeignPtr CUInt)
                                            poke (unsafeForeignPtrToPtr files) 0
                                            code <- c_archive_files carchive (unsafeForeignPtrToPtr files)
                                            return $ if code < 0 then
                                                         Nothing
                                                     else
                                                         Just $ unsafePerformIO $ peek $ unsafeForeignPtrToPtr files

archiveOffset :: Archive -> Maybe CInt
archiveOffset (Archive carchive _) = unsafePerformIO $ do
                                            offset <- mallocForeignPtr :: IO (ForeignPtr CInt)
                                            poke (unsafeForeignPtrToPtr offset) 0
                                            code <- c_archive_offset carchive (unsafeForeignPtrToPtr offset)
                                            return $ if code < 0 then
                                                         Nothing
                                                     else
                                                         Just $ unsafePerformIO $ peek $ unsafeForeignPtrToPtr offset

fileSizePacked :: Archive -> Int -> Maybe CInt
fileSizePacked (Archive carchive _) number = unsafePerformIO $ do
                                                    size <- mallocForeignPtr :: IO (ForeignPtr CInt)
                                                    poke (unsafeForeignPtrToPtr size) 0
                                                    code <- c_file_size_packed carchive (fromIntegral number) (unsafeForeignPtrToPtr size)
                                                    return $ if code < 0 then
                                                                 Nothing
                                                             else
                                                                 Just $ unsafePerformIO $ peek $ unsafeForeignPtrToPtr size

fileSizeUnpacked :: Archive -> Int -> Maybe CInt
fileSizeUnpacked (Archive carchive _) number = unsafePerformIO $ do
                                                    size <- mallocForeignPtr :: IO (ForeignPtr CInt)
                                                    poke (unsafeForeignPtrToPtr size) 0
                                                    code <- c_file_size_unpacked carchive (fromIntegral number) (unsafeForeignPtrToPtr size)
                                                    return $ if code < 0 then
                                                                 Nothing
                                                             else
                                                                 Just $ unsafePerformIO $ peek $ unsafeForeignPtrToPtr size

fileOffset :: Archive -> Int -> Maybe CInt
fileOffset (Archive carchive _) number = unsafePerformIO $ do
                                                    offset <- mallocForeignPtr :: IO (ForeignPtr CInt)
                                                    poke (unsafeForeignPtrToPtr offset) 0
                                                    code <- c_file_offset carchive (fromIntegral number) (unsafeForeignPtrToPtr offset)
                                                    return $ if code < 0 then
                                                                 Nothing
                                                             else
                                                                 Just $ unsafePerformIO $ peek $ unsafeForeignPtrToPtr offset

fileBlocks :: Archive -> Int -> Maybe CUInt
fileBlocks (Archive carchive _) number = unsafePerformIO $ do
                                                    blocks <- mallocForeignPtr :: IO (ForeignPtr CUInt)
                                                    poke (unsafeForeignPtrToPtr blocks) 0
                                                    code <- c_file_blocks carchive (fromIntegral number) (unsafeForeignPtrToPtr blocks)
                                                    return $ if code < 0 then
                                                                 Nothing
                                                             else
                                                                 Just $ unsafePerformIO $ peek $ unsafeForeignPtrToPtr blocks

fileIsEncrypted :: Archive -> Int -> Maybe Bool
fileIsEncrypted (Archive carchive _) number = unsafePerformIO $ do
                                                    encrypted <- mallocForeignPtr :: IO (ForeignPtr CUInt)
                                                    poke (unsafeForeignPtrToPtr encrypted) 0
                                                    code <- c_file_encrypted carchive (fromIntegral number) (unsafeForeignPtrToPtr encrypted)
                                                    return $ if code < 0 then
                                                                 Nothing
                                                             else
                                                                 let e = fromIntegral (unsafePerformIO $ peek $ unsafeForeignPtrToPtr encrypted) :: Int in
                                                                 Just $ e /= 0

fileIsCompressed :: Archive -> Int -> Maybe Bool
fileIsCompressed (Archive carchive _) number = unsafePerformIO $ do
                                                    compressed <- mallocForeignPtr :: IO (ForeignPtr CUInt)
                                                    poke (unsafeForeignPtrToPtr compressed) 0
                                                    code <- c_file_compressed carchive (fromIntegral number) (unsafeForeignPtrToPtr compressed)
                                                    return $ if code < 0 then
                                                                 Nothing
                                                             else
                                                                 let e = fromIntegral (unsafePerformIO $ peek $ unsafeForeignPtrToPtr compressed) :: Int in
                                                                 Just $ e /= 0

fileIsImploded :: Archive -> Int -> Maybe Bool
fileIsImploded (Archive carchive _) number = unsafePerformIO $ do
                                                    imploded <- mallocForeignPtr :: IO (ForeignPtr CUInt)
                                                    poke (unsafeForeignPtrToPtr imploded) 0
                                                    code <- c_file_imploded carchive (fromIntegral number) (unsafeForeignPtrToPtr imploded)
                                                    return $ if code < 0 then
                                                                 Nothing
                                                             else
                                                                 let e = fromIntegral (unsafePerformIO $ peek $ unsafeForeignPtrToPtr imploded) :: Int in
                                                                 Just $ e /= 0

fileNumber :: Archive -> ByteString -> Maybe CUInt
fileNumber (Archive carchive _) hname = unsafePerformIO $
                                            useAsCString hname $ \name -> do
                                                num <- mallocForeignPtr :: IO (ForeignPtr CUInt)
                                                poke (unsafeForeignPtrToPtr num) 0
                                                code <- c_file_number carchive name (unsafeForeignPtrToPtr num)
                                                return $ if code < 0 then
                                                             Nothing
                                                         else
                                                             Just $ unsafePerformIO $ peek $ unsafeForeignPtrToPtr num

fileContents :: Archive -> Int -> Maybe ByteString
fileContents (Archive carchive path) num = unsafePerformIO $ do
                                            let maybesize = fileSizeUnpacked (Archive carchive path) num
                                            return $ if isNothing maybesize then
                                                         Nothing
                                                     else unsafePerformIO $ do
                                                         let Just size = maybesize
                                                         buff <- mallocBytes (fromIntegral size) :: IO CString
                                                         trans <- mallocForeignPtr :: IO (ForeignPtr CInt)
                                                         poke (unsafeForeignPtrToPtr trans) 0
                                                         code <- c_file_read carchive (fromIntegral num) buff size (unsafeForeignPtrToPtr trans)
                                                         return $ if code < 0 then
                                                             Nothing
                                                         else
                                                             Just $ unsafePerformIO $ packCStringLen (buff, fromIntegral size)
