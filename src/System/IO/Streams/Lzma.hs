-- |
-- Module      : System.IO.Streams.Lzma
-- Copyright   : © 2015 Herbert Valerio Riedel
-- License     : BSD3
--
-- Maintainer  : hvr@gnu.org
-- Stability   : experimental
-- Portability : portable
--
-- Simple IO-Streams interface for lzma/xz compression
--
-- See also the XZ Utils home page: <http://tukaani.org/xz/>
module System.IO.Streams.Lzma
    ( -- * 'ByteString' decompression
      decompress
    , decompressWith
    , Lzma.defaultDecompressParams
    , Lzma.DecompressParams
    , Lzma.compressIntegrityCheck
    , Lzma.compressLevel
    , Lzma.compressLevelExtreme

      -- * 'ByteString' compression
    , compress
    , compressWith
    , Lzma.defaultCompressParams
    , Lzma.CompressParams
    , Lzma.decompressTellNoCheck
    , Lzma.decompressTellUnsupportedCheck
    , Lzma.decompressTellAnyCheck
    , Lzma.decompressConcatenated
    , Lzma.decompressAutoDecoder
    , Lzma.decompressMemLimit
    , Lzma.IntegrityCheck(..)
    , Lzma.CompressionLevel(..)

    ) where

import           Codec.Compression.Lzma (DecompressStream(..), CompressStream(..))
import qualified Codec.Compression.Lzma as Lzma

import           Control.Exception
import           Control.Monad
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import           Data.IORef
import           Data.Maybe
import           System.IO.Streams      (InputStream, OutputStream,
                                         makeInputStream, makeOutputStream)
import qualified System.IO.Streams      as Streams

-- | Decompress an 'InputStream' of strict 'ByteString's from the @.xz@ format
decompress :: InputStream ByteString -> IO (InputStream ByteString)
decompress = decompressWith Lzma.defaultDecompressParams

-- | Like 'decompress' but with the ability to specify various decompression
-- parameters. Typical usage:
--
-- > decompressWith defaultDecompressParams { decompress... = ... }
decompressWith :: Lzma.DecompressParams -> InputStream ByteString -> IO (InputStream ByteString)
decompressWith parms ibs = do
    st <- newIORef =<< Lzma.decompressIO parms
    makeInputStream (go st)
  where
    go stref = do
        st' <- goFeed =<< readIORef stref
        case st' of
            DecompressInputRequired _ -> do
                writeIORef stref st'
                fail "the impossible happened"

            DecompressOutputAvailable outb next -> do
                writeIORef stref =<< next
                return (Just outb)

            DecompressStreamEnd leftover -> do
                unless (BS.null leftover) $ do
                    Streams.unRead leftover ibs
                writeIORef stref (DecompressStreamEnd BS.empty)
                return Nothing

            DecompressStreamError rc -> do
                writeIORef stref st'
                throwIO rc

    -- feed engine
    goFeed (DecompressInputRequired supply) =
        goFeed =<< supply . fromMaybe BS.empty =<< getChunk
    goFeed s = return s

    -- wrapper around 'read ibs' to retry until a non-empty ByteString or Nothing is returned
    getChunk = do
        mbs <- Streams.read ibs
        case mbs of
            Just bs | BS.null bs -> getChunk
            _                    -> return mbs

----------------------------------------------------------------------------
----------------------------------------------------------------------------

-- | Convert an 'OutputStream' that consumes compressed 'ByteString's
-- (in the @.xz@ format) into an 'OutputStream' that consumes
-- uncompressed 'ByteString's
compress :: OutputStream ByteString -> IO (OutputStream ByteString)
compress = compressWith Lzma.defaultCompressParams

-- | Like 'compress' but with the ability to specify various compression
-- parameters. Typical usage:
--
-- > compressWith defaultCompressParams { compress... = ... }
compressWith :: Lzma.CompressParams -> OutputStream ByteString -> IO (OutputStream ByteString)
compressWith parms obs = do
    st <- newIORef =<< Lzma.compressIO parms
    makeOutputStream (go st)
  where
    go stref (Just chunk) = do
        st <- readIORef stref
        st' <- case st of
            CompressInputRequired flush supply
              | BS.null chunk -> goOutput =<< flush
              | otherwise     -> goOutput =<< supply chunk
            _ -> fail "compressWith: unexpected state"
        writeIORef stref st'
        case st' of
            CompressInputRequired _ _ -> return ()
            _ -> fail "compressWith:  unexpected state"

    -- EOF
    go stref Nothing = do
        st <- readIORef stref
        st' <- case st of
            CompressInputRequired _ supply -> goOutput =<< supply BS.empty
            _ -> fail "compressWith[EOF]: unexpected state"
        writeIORef stref st'
        case st' of
            CompressStreamEnd -> return ()
            _ -> fail "compressWith[EOF]:  unexpected state"

    -- Drain output from CompressStream
    goOutput st@(CompressInputRequired _ _) = return st
    goOutput (CompressOutputAvailable obuf next) = do
        Streams.write (Just obuf) obs
        goOutput =<< next
    goOutput st@CompressStreamEnd = do
        Streams.write Nothing obs
        return st
