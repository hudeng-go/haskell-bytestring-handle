{-# LANGUAGE CPP, DeriveDataTypeable #-}
module Data.ByteString.Handle.Write
    ( writeHandle
    ) where

import Control.Monad ( when )
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy.Internal as BLI
import Data.IORef ( IORef, newIORef, readIORef, modifyIORef, writeIORef )
import Data.Typeable ( Typeable )
import System.IO
    ( Handle, hClose, IOMode( WriteMode )
    , noNewlineTranslation, nativeNewlineMode
    )

import GHC.IO.Buffer ( BufferState(..), emptyBuffer, Buffer(..) )
import GHC.IO.BufferedIO ( BufferedIO(..) )
import GHC.IO.Device ( IODevice(..), IODeviceType(..), SeekMode(..) )
#if MIN_VERSION_base(4,5,0)
import GHC.IO.Encoding ( getLocaleEncoding )
#else
import GHC.IO.Encoding ( localeEncoding )
#endif
import GHC.IO.Exception
    ( ioException, unsupportedOperation
    , IOException(IOError), IOErrorType(InvalidArgument)
    )
import GHC.IO.Handle ( mkFileHandle )

data SeekState =
    SeekState {
        seek_pos :: Integer,
        -- the start position of the current chunk
        seek_base :: Integer
    }

data WriteState =
    WriteState {
        -- the Integer is the cumulative size of the chunk + all those before it in the 
        write_chunks_backwards :: IORef [(Integer, B.ByteString)],
        write_seek_state :: IORef SeekState,
        write_size :: IORef Integer
    }
    deriving Typeable

nextChunkSize :: Int -> Int
nextChunkSize lastSize
    | lastSize < 16 = 16 -- the minimum size currently targeted by Data.ByteString.Lazy.cons'
    | 2 * lastSize >= BLI.defaultChunkSize = BLI.defaultChunkSize
    | otherwise = 2 * lastSize

chunkForPosition :: Integer -> IORef [(Integer, B.ByteString)] -> IO (Integer, B.ByteString)
chunkForPosition pos chunks_backwards_ref = do
    when (pos > 1000000) $ error "gone"
    chunks_backwards <- readIORef chunks_backwards_ref
    let (curSize, lastSize) =
         case chunks_backwards of
             [] -> (0, 0)
             ((sz, c):_) -> (sz, B.length c)
    if pos < curSize
        then do
              let (sz, c) = head $ dropWhile (\(sz, c) -> pos < sz - fromIntegral (B.length c)) chunks_backwards
              return (sz - fromIntegral (B.length c), c)
        else do
              let sz = nextChunkSize lastSize
              newChunk <- BI.mallocByteString sz
              let bs = BI.fromForeignPtr newChunk 0 sz
              writeIORef chunks_backwards_ref ((curSize + fromIntegral sz, bs):chunks_backwards)
              chunkForPosition pos chunks_backwards_ref

initialWriteState :: IO WriteState
initialWriteState = do
    chunks <- newIORef []
    pos <- newIORef $ SeekState { seek_pos = 0, seek_base = 0 }
    sz <- newIORef 0

    return $    
        WriteState {
            write_chunks_backwards = chunks,
            write_seek_state = pos,
            write_size = sz
        }

instance BufferedIO WriteState where
    newBuffer _ ReadBuffer = ioException unsupportedOperation
    newBuffer ws WriteBuffer = do
       ss <- readIORef (write_seek_state ws) 
       (chunkBase, chunk) <- chunkForPosition (seek_pos ss) (write_chunks_backwards ws)
       let chunkOffset = fromIntegral (seek_pos ss - chunkBase)
       let (ptr, bsOffset, len) = BI.toForeignPtr chunk
           buf = (emptyBuffer ptr (bsOffset + len) WriteBuffer) {
                     bufL = bsOffset + chunkOffset, bufR = bsOffset + chunkOffset
           }
       writeIORef (write_seek_state ws) (ss { seek_base = chunkBase - fromIntegral bsOffset })
       return buf

    -- default impl for emptyWriteBuffer

    flushWriteBuffer ws buf = do
        ss <- readIORef (write_seek_state ws)
        let newPos = seek_base ss + fromIntegral (bufR buf)
        writeIORef (write_seek_state ws)
                   (SeekState { seek_pos = newPos,
                                seek_base = error "seek_base needs to be updated"
                   })
        modifyIORef (write_size ws) (`max` newPos)
        newBuffer ws WriteBuffer

    flushWriteBuffer0 ws buf = do
        let count = bufR buf - bufL buf
        newBuf <- flushWriteBuffer ws buf
        return (count, newBuf)

    fillReadBuffer _ _ = ioException unsupportedOperation

    fillReadBuffer0 _ _ = ioException unsupportedOperation

instance IODevice WriteState where
    ready _ _ _ = return True
    close ws = return ()

    isSeekable _ = return True

    seek ws seekMode seekPos = do
        curSeekState <- readIORef (write_seek_state ws)
        newSeekPos <-
              case seekMode of
                  AbsoluteSeek -> return seekPos
                  RelativeSeek -> return $ seek_pos curSeekState + seekPos
                  -- can probably assume last buffer is flushed, so could probably count the
                  -- current end pos if we really wanted to
                  SeekFromEnd -> ioException unsupportedOperation
        when (newSeekPos < 0) $ ioe_seekOutOfRange
        writeIORef (write_seek_state ws)
                   (SeekState { seek_pos = newSeekPos,
                                seek_base = error "seek_base needs to be updated"
                   })
        modifyIORef (write_size ws) (`max` newSeekPos)

    tell ws = do
        ss <- readIORef (write_seek_state ws)
        return (seek_pos ss)

    getSize ws = readIORef (write_size ws)
    setSize ws sz = do
        writeIORef (write_size ws) sz
        -- force chunk creation
        _ <- chunkForPosition sz (write_chunks_backwards ws)
        return ()

    devType _ = return RegularFile -- TODO: is this correct?

ioe_seekOutOfRange :: IO a
ioe_seekOutOfRange =
    ioException $ IOError Nothing InvalidArgument ""
                          "attempt to seek outside the file" Nothing Nothing

writeHandle :: Bool -> (Handle -> IO a) -> IO (BL.ByteString, a)
writeHandle binary doOutput = do

    ws <- initialWriteState

#if MIN_VERSION_base(4,5,0)
    localeEnc <- getLocaleEncoding
#else
    localeEnc <- return localeEncoding
#endif

    let (encoding, newline)
         | binary    = (Nothing       , noNewlineTranslation)
         | otherwise = (Just localeEnc, nativeNewlineMode   )

    handle <- mkFileHandle ws "ByteString" WriteMode encoding newline

    res <- doOutput handle

    hClose handle

    sz <- readIORef (write_size ws)
    chunks_backwards <- readIORef (write_chunks_backwards ws)

    let bs = BL.take (fromIntegral sz) . BL.fromChunks . reverse . map snd $ chunks_backwards

    return (bs, res)
