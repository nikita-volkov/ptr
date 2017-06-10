module Main where

import Prelude
import Bug
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.QuickCheck.Instances
import qualified PtrMagic.Encoder as B
import qualified PtrMagic.Decoder as C
import qualified Data.ByteString as D


main =
  defaultMain $
  testGroup "All tests"
  [
    testProperty "Encoder and decoder (bytes)" $ \input -> input === fromJust (encoderAndDecoder (B.bytes (D.length input)) (C.bytes (D.length input))) input
    ,
    testProperty "Encoder and decoder (word8)" $ \input -> input === fromJust (encoderAndDecoder B.word8 C.word8) input
    ,
    testProperty "Encoder and decoder (beWord32)" $ \input -> input === fromJust (encoderAndDecoder B.beWord32 C.beWord32) input
    ,
    testProperty "Encoder and decoder (beWord64)" $ \input -> input === fromJust (encoderAndDecoder B.beWord64 C.beWord64) input
  ]

encoderAndDecoder :: B.Encoder a -> C.Decoder a -> Maybe (a -> a)
encoderAndDecoder (B.Encoder encoderSize encoderIO) (C.Decoder decoderSize decoderIO) =
  if encoderSize /= decoderSize
    then Nothing
    else Just $ \input -> unsafePerformIO $ do
      fp <- mallocForeignPtrBytes encoderSize
      withForeignPtr fp $ \p -> do
        encoderIO p input
        decoderIO p
