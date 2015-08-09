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
    , defaultDecompressParams
    , DecompressParams(..)

      -- * 'ByteString' compression
    , compress
    , compressWith
    , defaultCompressParams
    , CompressParams(..)
    , IntegrityCheck(..)
    , CompressionLevel(..)

    ) where

import           Control.Exception
import           Control.Monad
import           Data.ByteString   (ByteString)
import qualified Data.ByteString   as BS
import           Data.IORef
import           LibLzma
import           System.IO.Streams (InputStream, OutputStream, makeInputStream,
                                    makeOutputStream)
import qualified System.IO.Streams as Streams

-- | Decompress an 'InputStream' of strict 'ByteString's from the @.xz@ format
decompress :: InputStream ByteString -> IO (InputStream ByteString)
decompress = decompressWith defaultDecompressParams

-- | Like 'decompress' but with the ability to specify various decompression
-- parameters. Typical usage:
--
-- > decompressWith defaultDecompressParams { decompress... = ... }
decompressWith :: DecompressParams -> InputStream ByteString -> IO (InputStream ByteString)
decompressWith flags ibs
    = newDecodeLzmaStream flags >>= either throwIO (wrapLzmaInStream ibs)

-- TODO: figure out sensible buffer-size & refactor into generic
-- incremental API in the style of zlib's incremental API
wrapLzmaInStream :: InputStream ByteString -> LzmaStream -> IO (InputStream ByteString)
wrapLzmaInStream ibs ls0 = do
    st <- newIORef (Right ls0)
    makeInputStream (go st)
  where
    go st = readIORef st >>= either goLeft goRight
      where
        goRight ls = do
            ibuf <- getChunk

            (rc, _, obuf) <- case ibuf of
                Nothing -> runLzmaStream ls BS.empty LzmaFinish bUFSIZ
                Just bs -> do
                    retval@(_, consumed, _) <- runLzmaStream ls bs LzmaRun bUFSIZ
                    when (consumed < BS.length bs) $ do
                        Streams.unRead (BS.drop consumed bs) ibs
                    return retval

            unless (rc == LzmaRetOK) $ do
                writeIORef st (Left rc)
                unless (rc == LzmaRetStreamEnd) $
                    throwIO rc

            case rc of
                LzmaRetOK -> if (BS.null obuf)
                             then goRight ls -- feed de/encoder some more
                             else return (Just obuf)

                LzmaRetStreamEnd -> do
                    writeIORef st (Left rc)
                    if BS.null obuf
                        then return Nothing
                        else return (Just obuf)

                _ -> writeIORef st (Left rc) >> throwIO rc

    goLeft err = case err of
        LzmaRetStreamEnd -> return Nothing
        _                -> throwIO err

    bUFSIZ = 32752

    -- wrapper around 'read ibs' to retry until a non-empty ByteString or Nothing is returned
    -- TODO: consider implementing flush semantics
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
compress = compressWith defaultCompressParams

-- | Like 'compress' but with the ability to specify various compression
-- parameters. Typical usage:
--
-- > compressWith defaultCompressParams { compress... = ... }
compressWith :: CompressParams -> OutputStream ByteString -> IO (OutputStream ByteString)
compressWith parms obs = do
    st <- newIORef =<< compressIO parms
    makeOutputStream (go st)
  where
    go stref (Just chunk)
      | BS.null chunk = return () -- we don't support flushing yet
      | otherwise = do
          st <- readIORef stref
          st' <- case st of
              CompressInputRequired supply -> goOutput =<< supply chunk
              _ -> fail "compressWith: unexpected state"
          writeIORef stref st'

          case st' of
              CompressInputRequired _ -> return ()
              _ -> fail "compressWith:  unexpected state"


    -- EOF
    go stref Nothing = do
        st <- readIORef stref
        st' <- case st of
            CompressInputRequired supply -> goOutput =<< supply BS.empty
            _ -> fail "compressWith[EOF]: unexpected state"
        writeIORef stref st'
        case st' of
            CompressStreamEnd -> return ()
            _ -> fail "compressWith[EOF]:  unexpected state"

    goOutput st@(CompressInputRequired _) = do
        return st
    goOutput (CompressOutputAvailable obuf next) = do
        Streams.write (Just obuf) obs
        goOutput =<< next
    goOutput st@CompressStreamEnd = do
        Streams.write Nothing obs
        return st
