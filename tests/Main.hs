module Main where

import Prelude
import Bug
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.QuickCheck.Instances
import qualified PtrMagic.Encode as B
import qualified PtrMagic.Decode as C
import qualified PtrMagic.Codec as E
import qualified Data.ByteString as D


main =
  defaultMain $
  testGroup "All tests"
  [
    testProperty "Encode and decode (bytes)" $ \input -> input === fromJust (encodeAndDecode (B.bytes (D.length input)) (C.bytes (D.length input))) input
    ,
    testProperty "Encode and decode (word8)" $ \input -> input === fromJust (encodeAndDecode B.word8 C.word8) input
    ,
    testProperty "Encode and decode (beWord32)" $ \input -> input === fromJust (encodeAndDecode B.beWord32 C.beWord32) input
    ,
    testProperty "Encode and decode (beWord64)" $ \input -> input === fromJust (encodeAndDecode B.beWord64 C.beWord64) input
    ,
    testProperty "Codec composition" $ \input -> input === codec ((,) <$> lmap fst E.word8 <*> lmap snd E.beWord32) input
  ]

encodeAndDecode :: B.Encode a -> C.Decode a -> Maybe (a -> a)
encodeAndDecode (B.Encode encodeSize encodeIO) (C.Decode decodeSize decodeIO) =
  if encodeSize /= decodeSize
    then Nothing
    else Just $ \input -> unsafePerformIO $ do
      fp <- mallocForeignPtrBytes encodeSize
      withForeignPtr fp $ \p -> do
        encodeIO p input
        decodeIO p

codec :: E.Codec input output -> input -> output
codec (E.Codec size poke peek) input =
  unsafePerformIO $ do
    fp <- mallocForeignPtrBytes size
    withForeignPtr fp $ \p -> do
      poke p input
      peek p
