{-# LANGUAGE ScopedTypeVariables #-}
import Control.Applicative ( (<$>) )
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Char ( chr )
import Data.Int ( Int64 )
import System.IO
    ( Handle, hGetContents, hSeek, SeekMode(..), hPutStr
    , TextEncoding, hSetEncoding, utf8, latin1
    )
import System.IO.Unsafe ( unsafePerformIO )

import Test.HUnit
import Test.Framework ( defaultMain )
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 ( testProperty )
import Test.QuickCheck ( (==>), Property )
import Test.QuickCheck.Arbitrary ( Arbitrary(..) )

import Data.ByteString.Handle ( readHandle, writeHandle )

instance Arbitrary BL.ByteString where
    arbitrary = fmap BL.pack arbitrary

readViaHandle :: Bool -> BL.ByteString -> (Handle -> IO a) -> a
readViaHandle binary bs f = unsafePerformIO $ readHandle binary bs >>= f

writeViaHandle :: Bool -> (Handle -> IO ()) -> BL.ByteString
writeViaHandle binary f = fst $ unsafePerformIO $ writeHandle binary f

simpleBinaryReadRoundTrip :: BL.ByteString -> Bool
simpleBinaryReadRoundTrip bs =
    -- ironically, BL.hGetContents doesn't like ByteString-backed handles - fails with "Not an FD"
    bs == readViaHandle True bs (\h -> BLC.pack <$> hGetContents h)

prop_SimpleBinaryReadRoundTrip :: BL.ByteString -> Bool
prop_SimpleBinaryReadRoundTrip bs = simpleBinaryReadRoundTrip bs

simpleBinaryWriteRoundTrip :: BL.ByteString -> Bool
simpleBinaryWriteRoundTrip bs =
    bs == writeViaHandle True (\h -> hPutStr h (BLC.unpack bs))

prop_SimpleBinaryWriteRoundTrip :: BL.ByteString -> Bool
prop_SimpleBinaryWriteRoundTrip bs = simpleBinaryWriteRoundTrip bs

-- testing segments of bytestrings is important because they are internally represented
-- using offsets into the original bytestrings

tailBinaryReadRoundTrip :: BL.ByteString -> Bool
tailBinaryReadRoundTrip bs =
    BL.tail bs == readViaHandle True (BL.tail bs) (\h -> BLC.pack <$> hGetContents h)

prop_TailBinaryReadRoundTrip :: BL.ByteString -> Property
prop_TailBinaryReadRoundTrip bs = not (BL.null bs) ==> tailBinaryReadRoundTrip bs

seekBinaryReadRoundTrip :: BL.ByteString -> Bool
seekBinaryReadRoundTrip bs =
    BL.tail bs == readViaHandle True bs (\h -> do { hSeek h AbsoluteSeek 1 ; BLC.pack <$> hGetContents h })

prop_SeekBinaryReadRoundTrip :: BL.ByteString -> Property
prop_SeekBinaryReadRoundTrip bs = not (BL.null bs) ==> seekBinaryReadRoundTrip bs

initBinaryReadRoundTrip :: BL.ByteString -> Bool
initBinaryReadRoundTrip bs =
    BL.init bs == readViaHandle True (BL.init bs) (\h -> BLC.pack <$> hGetContents h)

prop_InitBinaryReadRoundTrip :: BL.ByteString -> Property
prop_InitBinaryReadRoundTrip bs = not (BL.null bs) ==> initBinaryReadRoundTrip bs

takeBinaryReadRoundTrip :: Int64 -> BL.ByteString -> Bool
takeBinaryReadRoundTrip n bs =
    BL.take n bs == BL.take n (readViaHandle True bs (\h -> BLC.pack <$> hGetContents h))

takeSeekBinaryReadRoundTrip :: Int64 -> BL.ByteString -> Bool
takeSeekBinaryReadRoundTrip n bs =
    BL.take n (BL.tail bs) == BL.take n (readViaHandle True bs (\h -> do { hSeek h AbsoluteSeek 1 ; BLC.pack <$> hGetContents h }))

-- several times the default 32K chunk size
bigByteString :: BL.ByteString
bigByteString = BL.pack $ concat $ replicate 1000 $ [0..255] ++ [1..100]

infiniteByteString :: BL.ByteString
infiniteByteString = BL.cycle $ BL.pack $ [0..255] ++ [1..100]

encode :: TextEncoding -> String -> BL.ByteString
encode te str = writeViaHandle True (\h -> do { hSetEncoding h te ; hPutStr h str })

decode :: TextEncoding -> BL.ByteString -> String
decode te bs = readViaHandle True bs (\h -> do { hSetEncoding h te ; hGetContents h })

tests =
    [ testProperty "Simple binary read roundtrip" prop_SimpleBinaryReadRoundTrip
    , testProperty "Tail binary read roundtrip" prop_TailBinaryReadRoundTrip
    , testProperty "Init binary read roundtrip" prop_InitBinaryReadRoundTrip
    , testProperty "Seek binary read roundtrip" prop_SeekBinaryReadRoundTrip
    , testCase "Big bytestring binary read roundtrip" $
           assertBool "roundtrip succeeded" (simpleBinaryReadRoundTrip bigByteString)
    , testCase "Big bytestring tail binary read roundtrip" $
           assertBool "roundtrip succeeded" (tailBinaryReadRoundTrip bigByteString)
    , testCase "Big bytestring init binary read roundtrip" $
           assertBool "roundtrip succeeded" (initBinaryReadRoundTrip bigByteString)
    , testCase "Big bytestring seek binary read roundtrip" $
           assertBool "roundtrip succeeded" (seekBinaryReadRoundTrip bigByteString)
    , testCase "Infinite bytestring read roundtrip" $
           assertBool "roundtrip succeeded" (takeBinaryReadRoundTrip 100000 infiniteByteString)
    , testCase "Infinite bytestring seek read roundtrip" $
           assertBool "roundtrip succeeded" (takeSeekBinaryReadRoundTrip 100000 infiniteByteString)
    , testProperty "Simple binary write roundtrip" prop_SimpleBinaryWriteRoundTrip
    , testCase "Sterling symbol latin1 encode" $
           assertEqual "latin1 sterling" (BL.pack [163]) (encode latin1 [chr 163])
    , testCase "Sterling symbol utf8 encode" $
           assertEqual "latin1 sterling" (BL.pack [194,163]) (encode utf8 [chr 163])
    , testCase "Sterling symbol latin1 decode" $
           assertEqual "latin1 sterling" [chr 163] (decode latin1 (BL.pack [163]))
    , testCase "Sterling symbol utf8 decode" $
           assertEqual "latin1 sterling" [chr 163] (decode utf8 (BL.pack [194,163]))
    ]

main = defaultMain tests