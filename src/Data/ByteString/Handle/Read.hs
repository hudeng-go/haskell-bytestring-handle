{-# LANGUAGE CPP, DeriveDataTypeable, ForeignFunctionInterface #-}
module Data.ByteString.Handle.Read
    ( readHandle
    ) where

import Control.Monad ( when )
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy as BL
import Data.IORef ( IORef, newIORef, readIORef, modifyIORef, writeIORef )
import Data.Maybe ( fromMaybe )
import Data.Typeable ( Typeable )
import Data.Word ( Word8 )
import Foreign.C.Types ( CSize(..) )
import Foreign.ForeignPtr ( newForeignPtr_ )
import Foreign.Ptr ( Ptr, nullPtr, plusPtr )
import System.IO
    ( Handle, IOMode( ReadMode )
    , noNewlineTranslation, nativeNewlineMode
    )

import GHC.IO.Buffer
    ( BufferState(..), Buffer(..)
    , emptyBuffer, isEmptyBuffer, newBuffer, newByteBuffer
    , bufferElems, withBuffer, withRawBuffer )
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
       -- a reversed list of the chunks before the current seek position
       seek_before :: [B.ByteString],
       -- a list of the chunks including and after the current seek position
       seek_after :: [B.ByteString],
       -- an index into the first chunk of seek_after
       seek_pos :: !Int,
       -- total length of seek_before : redundant info for cheaply answering 'tell'
       seek_before_length :: !Integer
    }

data ReadState =
    ReadState {
        read_chunks :: [B.ByteString],
        -- reverse list for use with SeekFromEnd - lazily constructed
        read_chunks_backwards :: [B.ByteString],
        -- for use with getSize and SeekFromEnd - lazily constructed
        read_length :: Integer,
        read_seek_state :: IORef SeekState
    }
    deriving Typeable

nullReadBuffer = do
    ptr <- newForeignPtr_ nullPtr
    return $ emptyBuffer ptr
                         0
                         ReadBuffer

foreign import ccall unsafe "memmove"
   memmove :: Ptr a -> Ptr a -> CSize -> IO (Ptr a)

instance BufferedIO ReadState where
    emptyWriteBuffer _ _ = ioException unsupportedOperation
    flushWriteBuffer _ _ = ioException unsupportedOperation
    flushWriteBuffer0 _ _ = ioException unsupportedOperation

    newBuffer _ WriteBuffer = ioException unsupportedOperation
    newBuffer rs ReadBuffer = nullReadBuffer

    fillReadBuffer rs bufIn = do
       (count, buf) <- fillReadBuffer0 rs bufIn
       return (fromMaybe 0 count, buf)

    fillReadBuffer0 rs bufIn = do
        ss <- readIORef (read_seek_state rs)
        case seek_after ss of
              [] -> do
                return (Nothing, bufIn)
              (chunk:chunks) ->
                  let (ptr, bsOffset_noseek, _) = BI.toForeignPtr chunk
                      bsOffset = bsOffset_noseek + seek_pos ss
                      bsOffsetEnd = bsOffset_noseek + B.length chunk
                  in do buf <- if isEmptyBuffer bufIn
                                then return (emptyBuffer ptr bsOffsetEnd ReadBuffer) {
                                                bufL = bsOffset, bufR = bsOffsetEnd
                                            }
                                else do let sz = bufferElems bufIn + B.length chunk - seek_pos ss
                                        buf <- newByteBuffer sz ReadBuffer
                                        withBuffer buf $ \buf_ptr -> do
                                          withBuffer bufIn $ \buf_in_ptr ->
                                            memmove buf_ptr (buf_in_ptr `plusPtr` bufL bufIn) (fromIntegral $ bufferElems bufIn)
                                          withRawBuffer ptr $ \ptr_ptr ->
                                            memmove (buf_ptr `plusPtr` bufferElems bufIn) (ptr_ptr `plusPtr` bsOffset) (fromIntegral (bsOffsetEnd - bsOffset))
                                        return (buf { bufR = sz })
                        writeIORef (read_seek_state rs)
                                   (SeekState {
                                        seek_before = chunk:seek_before ss,
                                        seek_after = chunks,
                                        seek_pos = 0,
                                        seek_before_length = fromIntegral (B.length chunk) + seek_before_length ss
                                    })
                        return (Just (B.length chunk - seek_pos ss), buf)

normalisedSeekState :: [B.ByteString] -> [B.ByteString] -> Integer -> Integer -> Maybe SeekState
normalisedSeekState (x:before) after beforeLen pos
    | pos < 0 = normalisedSeekState
                     before
                     (x:after)
                     (beforeLen - fromIntegral (B.length x))
                     (pos + fromIntegral (B.length x))
normalisedSeekState [] _ _ pos
    | pos < 0 = Nothing
normalisedSeekState before (x:after) beforeLen pos
    | pos >= fromIntegral (B.length x)
        = normalisedSeekState
                      (x:before)
                      after
                      (beforeLen + fromIntegral (B.length x))
                      (pos - fromIntegral (B.length x))
normalisedSeekState _ [] _ pos
    | pos > 0 = Nothing
normalisedSeekState before after beforeLen pos =
    Just (SeekState {
                seek_before = before,
                seek_after = after,
                seek_pos = fromIntegral pos,
                seek_before_length = beforeLen
         })


instance IODevice ReadState where
    ready _ _ _ = return True
    close _ = return ()

    isSeekable _ = return True

    seek rs seekMode seekPos = do
        size <- getSize rs
        curSeekState <- readIORef (read_seek_state rs)
        let newSeekState =
              case seekMode of
                  AbsoluteSeek -> normalisedSeekState [] (read_chunks rs) 0 seekPos
                  RelativeSeek -> normalisedSeekState (seek_before curSeekState)
                                                      (seek_after curSeekState)
                                                      (seek_before_length curSeekState)
                                                      (fromIntegral (seek_pos curSeekState) + seekPos)
                  SeekFromEnd -> normalisedSeekState (read_chunks_backwards rs) [] (read_length rs) seekPos
        maybe ioe_seekOutOfRange (writeIORef (read_seek_state rs)) newSeekState

    tell rs = do
        ss <- readIORef (read_seek_state rs)
        return (seek_before_length ss + fromIntegral (seek_pos ss))

    getSize = return . read_length
    setSize _ _ = ioException unsupportedOperation

    devType _ = return RegularFile -- TODO: is this correct?

ioe_seekOutOfRange :: IO a
ioe_seekOutOfRange =
    ioException $ IOError Nothing InvalidArgument ""
                          "attempt to seek outside the file" Nothing Nothing

readHandle :: Bool -> BL.ByteString -> IO Handle
readHandle binary bs = do
    let chunks = BL.toChunks bs

    let ss = SeekState {
        seek_before = [],
        seek_after = chunks,
        seek_pos = 0,
        seek_before_length = 0
    }

    ssref <- newIORef ss

    let rs = ReadState {
        read_chunks = chunks,
        read_chunks_backwards = reverse chunks,
        read_seek_state = ssref,
        read_length = sum (map (fromIntegral . B.length) chunks)
    }

#if MIN_VERSION_base(4,5,0)
    localeEnc <- getLocaleEncoding
#else
    localeEnc <- return localeEncoding
#endif

    let (encoding, newline)
         | binary    = (Nothing       , noNewlineTranslation)
         | otherwise = (Just localeEnc, nativeNewlineMode   )

    mkFileHandle rs "ByteString" ReadMode encoding newline
