{-# LANGUAGE RecordWildCards, DeriveDataTypeable #-}

-- Copyright (c) 2014, Herbert Valerio Riedel <hvr@gnu.org>
--
-- This code is BSD3 licensed, see ../LICENSE file for details
--

-- | Internal low-level binding to liblzma
--
-- TODO: Polish, generalise, and factor out into streaming-API
--       agnostic package w/ minimal build-deps.
--       Something in the style of the incremental API in
--       "Codec.Compression.Zlib.Internal" would be nice
--
module LibLzma
    ( LzmaStream
    , LzmaRet(..)
    , IntegrityCheck(..)
    , CompressionLevel(..)

    , newDecodeLzmaStream
    , DecompressParams(..)
    , defaultDecompressParams

    , newEncodeLzmaStream
    , CompressParams(..)
    , defaultCompressParams

    , runLzmaStream

    , CompressStream(..)
    , compressIO
    ) where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString as BS
import           Data.Typeable
import           Foreign
import           Prelude

#include <lzma.h>

newtype LzmaStream = LS (ForeignPtr LzmaStream)

data LzmaRet = LzmaRetOK
             | LzmaRetStreamEnd
             | LzmaRetUnsupportedCheck
             | LzmaRetGetCheck
             | LzmaRetMemError
             | LzmaRetMemlimitError
             | LzmaRetFormatError
             | LzmaRetOptionsError
             | LzmaRetDataError
             | LzmaRetBufError
             | LzmaRetProgError
             deriving (Eq,Ord,Show,Typeable)

instance Exception LzmaRet

toLzmaRet :: Int -> Maybe LzmaRet
toLzmaRet i = case i of
    (#const LZMA_OK               ) -> Just LzmaRetOK
    (#const LZMA_STREAM_END       ) -> Just LzmaRetStreamEnd
    (#const LZMA_UNSUPPORTED_CHECK) -> Just LzmaRetUnsupportedCheck
    (#const LZMA_GET_CHECK        ) -> Just LzmaRetGetCheck
    (#const LZMA_MEM_ERROR        ) -> Just LzmaRetMemError
    (#const LZMA_MEMLIMIT_ERROR   ) -> Just LzmaRetMemlimitError
    (#const LZMA_FORMAT_ERROR     ) -> Just LzmaRetFormatError
    (#const LZMA_OPTIONS_ERROR    ) -> Just LzmaRetOptionsError
    (#const LZMA_DATA_ERROR       ) -> Just LzmaRetDataError
    (#const LZMA_BUF_ERROR        ) -> Just LzmaRetBufError
    (#const LZMA_PROG_ERROR       ) -> Just LzmaRetProgError
    _                               -> Nothing

-- | Integrity check type (only supported when compressing @.xz@ files)
data IntegrityCheck = IntegrityCheckNone   -- ^ disable integrity check (not recommended)
               | IntegrityCheckCrc32  -- ^ CRC32 using the polynomial from IEEE-802.3
               | IntegrityCheckCrc64  -- ^ CRC64 using the polynomial from ECMA-182
               | IntegrityCheckSha256 -- ^ SHA-256
               deriving (Eq,Ord,Show)

-- | Compression level presets that define the tradeoff between
-- computational complexity and compression ratio
--
-- 'CompressionLevel0' has the lowest compression ratio as well as the
-- lowest memory requirements, whereas 'CompressionLevel9' has the
-- highest compression ratio and can require over 600MiB during
-- compression (and over 60MiB during decompression). The
-- <https://www.freebsd.org/cgi/man.cgi?query=xz&sektion=1&manpath=FreeBSD+10.2-stable&arch=default&format=html man-page for xz(1)>
-- contains more detailed information with tables describing the
-- properties of all compression level presets.
--
-- 'CompressionLevel6' is the default setting in
-- 'defaultCompressParams' as it provides a good trade-off and
-- matches the default of the @xz(1)@ tool.

data CompressionLevel = CompressionLevel0
                      | CompressionLevel1
                      | CompressionLevel2
                      | CompressionLevel3
                      | CompressionLevel4
                      | CompressionLevel5
                      | CompressionLevel6
                      | CompressionLevel7
                      | CompressionLevel8
                      | CompressionLevel9
                      deriving (Eq,Ord,Show,Enum)

fromIntegrityCheck :: IntegrityCheck -> Int
fromIntegrityCheck lc = case lc of
    IntegrityCheckNone   -> #const LZMA_CHECK_NONE
    IntegrityCheckCrc32  -> #const LZMA_CHECK_CRC32
    IntegrityCheckCrc64  -> #const LZMA_CHECK_CRC64
    IntegrityCheckSha256 -> #const LZMA_CHECK_SHA256

-- | Set of parameters for decompression. The defaults are 'defaultDecompressParams'.
data DecompressParams = DecompressParams
    { decompressTellNoCheck          :: !Bool
    , decompressTellUnsupportedCheck :: !Bool
    , decompressTellAnyCheck         :: !Bool
    , decompressConcatenated         :: !Bool
    , decompressAutoDecoder          :: !Bool
    , decompressMemLimit             :: !Word64 -- ^ Set to 'maxBound' to disable memory limit
    } deriving (Eq,Show)

-- | The default set of parameters for decompression. This is typically used with the decompressWith function with specific parameters overridden.
defaultDecompressParams :: DecompressParams
defaultDecompressParams = DecompressParams {..}
  where
    decompressTellNoCheck          = False
    decompressTellUnsupportedCheck = False
    decompressTellAnyCheck         = False
    decompressConcatenated         = True
    decompressAutoDecoder          = False
    decompressMemLimit             = maxBound -- disables limit-check

-- | Set of parameters for compression. The defaults are 'defaultCompressParams'.
data CompressParams = CompressParams
    { compressIntegrityCheck :: !IntegrityCheck -- ^ Specify type of integrity check
    , compressLevel          :: !CompressionLevel -- ^ See documentation of 'CompressionLevel'
    , compressLevelExtreme   :: !Bool  -- ^ Enable slower variant of the
                                       -- 'lzmaCompLevel' preset, see @xz(1)@
                                       -- man-page for details.
    } deriving (Eq,Show)

-- | The default set of parameters for compression. This is typically used with the compressWith function with specific parameters overridden.
defaultCompressParams :: CompressParams
defaultCompressParams = CompressParams {..}
  where
    compressIntegrityCheck = IntegrityCheckCrc64
    compressLevel          = CompressionLevel6
    compressLevelExtreme   = False

newDecodeLzmaStream :: DecompressParams -> IO (Either LzmaRet LzmaStream)
newDecodeLzmaStream (DecompressParams {..}) = do
    fp <- mallocForeignPtrBytes (#size lzma_stream)
    addForeignPtrFinalizer c_hs_lzma_done_funptr fp
    rc <- withForeignPtr fp (\ptr -> c_hs_lzma_init_decoder ptr decompressAutoDecoder decompressMemLimit flags')
    rc' <- maybe (fail "newDecodeLzmaStream: invalid return code") pure $ toLzmaRet rc

    return $ case rc' of
        LzmaRetOK -> Right (LS fp)
        _         -> Left rc'
  where
    flags' =
        (if decompressTellNoCheck          then (#const LZMA_TELL_NO_CHECK)          else 0) .|.
        (if decompressTellUnsupportedCheck then (#const LZMA_TELL_UNSUPPORTED_CHECK) else 0) .|.
        (if decompressTellAnyCheck         then (#const LZMA_TELL_ANY_CHECK)         else 0) .|.
        (if decompressConcatenated         then (#const LZMA_CONCATENATED)           else 0)

newEncodeLzmaStream :: CompressParams -> IO (Either LzmaRet LzmaStream)
newEncodeLzmaStream (CompressParams {..}) = do
    fp <- mallocForeignPtrBytes (#size lzma_stream)
    addForeignPtrFinalizer c_hs_lzma_done_funptr fp
    rc <- withForeignPtr fp (\ptr -> c_hs_lzma_init_encoder ptr preset check)
    rc' <- maybe (fail "newDecodeLzmaStream: invalid return code") pure $ toLzmaRet rc

    return $ case rc' of
        LzmaRetOK -> Right (LS fp)
        _         -> Left rc'

  where
    preset = fromIntegral (fromEnum compressLevel) .|.
             (if compressLevelExtreme then (#const LZMA_PRESET_EXTREME) else 0)
    check = fromIntegrityCheck compressIntegrityCheck

runLzmaStream :: LzmaStream -> ByteString -> Bool -> Int -> IO (LzmaRet,Int,ByteString)
runLzmaStream (LS ls) ibs finish buflen
  | buflen <= 0 = fail "runLzmaStream: invalid buflen argument"
  | otherwise = withForeignPtr ls $ \lsptr -> do
      BS.unsafeUseAsCStringLen ibs $ \(ibsptr, ibslen) -> do
          (obuf,rc) <- BS.createAndTrim' buflen $ \bufptr -> do
              rc' <- c_hs_lzma_run lsptr action (castPtr ibsptr) ibslen bufptr buflen
              rc'' <- maybe (fail "runLzmaStream: invalid return code") pure $ toLzmaRet rc'

              availOut <- (#peek lzma_stream, avail_out) lsptr
              unless (buflen >= availOut && availOut >= 0) (fail "runLzmaStream: invalid avail_out")
              let produced = buflen - availOut

              return (0, produced, rc'')

          availIn <- (#peek lzma_stream, avail_in) lsptr
          unless (ibslen >= availIn && availIn >= 0) (fail "runLzmaStream: invalid avail_in")
          let consumed = ibslen - availIn

          return (rc, fromIntegral consumed, obuf)
  where
    action = if finish then (#const LZMA_FINISH) else (#const LZMA_RUN)

----------------------------------------------------------------------------
-- trivial helper wrappers defined in ../cbits/lzma_wrapper.c

foreign import ccall "hs_lzma_init_decoder"
    c_hs_lzma_init_decoder :: Ptr LzmaStream -> Bool -> Word64 -> Word32 -> IO Int

foreign import ccall "hs_lzma_init_encoder"
    c_hs_lzma_init_encoder :: Ptr LzmaStream -> Word32 -> Int -> IO Int

foreign import ccall "hs_lzma_run"
    c_hs_lzma_run :: Ptr LzmaStream -> Int -> Ptr Word8 -> Int -> Ptr Word8 -> Int -> IO Int

foreign import ccall "&hs_lzma_done"
    c_hs_lzma_done_funptr :: FunPtr (Ptr LzmaStream -> IO ())

----------------------------------------------------------------------------

-- type stolen from 'zlib', we may actually just depend on zlib at some point in the future

data CompressStream m =
     CompressInputRequired (ByteString -> m (CompressStream m))
   | CompressOutputAvailable !ByteString (m (CompressStream m))
   | CompressStreamEnd

compressIO :: CompressParams -> IO (CompressStream IO)
compressIO parms = newEncodeLzmaStream parms >>= either throwIO go
  where
    bUFSIZ = 32752

    go :: LzmaStream -> IO (CompressStream IO)
    go ls = return $ CompressInputRequired goInput
      where
        goInput :: ByteString -> IO (CompressStream IO)
        goInput chunk
          | BS.null chunk = goFinish
          | otherwise     = do
              (rc, used, obuf) <- runLzmaStream ls chunk False bUFSIZ

              unless (used > 0) $ fail "compressIO: input chunk not consumed"

              let chunk' = BS.drop used chunk

              case rc of
                  LzmaRetOK
                      | BS.null obuf -> if BS.null chunk'
                                        then return (CompressInputRequired goInput)
                                        else goInput chunk'

                      | otherwise -> return (CompressOutputAvailable obuf
                                             (if BS.null chunk'
                                              then return (CompressInputRequired goInput)
                                              else goInput chunk'))

                  _ -> throwIO rc

        goFinish :: IO (CompressStream IO)
        goFinish = do
            (rc, 0, obuf) <- runLzmaStream ls BS.empty True bUFSIZ

            case rc of
                LzmaRetOK
                    | BS.null obuf -> fail "compressIO: empty output chunk"
                    | otherwise    -> return (CompressOutputAvailable obuf goFinish)
                LzmaRetStreamEnd
                    | BS.null obuf -> return CompressStreamEnd
                    | otherwise    -> return (CompressOutputAvailable obuf (return CompressStreamEnd))

                _ -> throwIO rc
