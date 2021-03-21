module Main where

import Prelude
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.QuickCheck.Instances
import qualified Ptr.Poke as B
import qualified Ptr.Peek as C
import qualified Ptr.PokeAndPeek as E
import qualified Ptr.ByteString as A
import qualified Ptr.Poking as F
import qualified Ptr.Parse as G
import qualified Ptr.Read as H
import qualified Data.ByteString as D
import qualified Data.Vector.Unboxed as UnboxedVector


main =
  defaultMain $
  testGroup "All tests"
  [
    testProperty "ASCII Numbers ByteString Roundtrip" $ \ (numbers :: [Word64]) -> let
      expected = foldMap (fromString . show) numbers
      actual = A.poking (foldMap F.asciiIntegral numbers)
      in expected === actual
    ,
    testProperty "Poke and peek (bytes)" $ \input -> input === fromJust (pokeThenPeek (B.bytes (D.length input)) (C.bytes (D.length input))) input
    ,
    testProperty "Poke and peek (word8)" $ \input -> input === fromJust (pokeThenPeek B.word8 C.word8) input
    ,
    testProperty "Poke and peek (beWord32)" $ \input -> input === fromJust (pokeThenPeek B.beWord32 C.beWord32) input
    ,
    testProperty "Poke and peek (beWord64)" $ \input -> input === fromJust (pokeThenPeek B.beWord64 C.beWord64) input
    ,
    testProperty "PokeAndPeek composition" $ \input -> input === pokeAndPeek ((,) <$> lmap fst E.word8 <*> lmap snd E.beWord32) input
    ,
    testGroup "Poking"
    [
      testCase "asciiPaddedAndTrimmedIntegral" $ do
        assertEqual "" "001" (A.poking (F.asciiPaddedAndTrimmedIntegral 3 1))
        assertEqual "" "001" (A.poking (F.asciiPaddedAndTrimmedIntegral 3 2001))
        assertEqual "" "000" (A.poking (F.asciiPaddedAndTrimmedIntegral 3 (-1)))
      ,
      testCase "asciiUtcTimeInIso8601" $ do
        assertEqual "" "2017-02-01T05:03:58Z" (A.poking (F.asciiUtcTimeInIso8601 (read "2017-02-01 05:03:58Z")))
      ,
      testCase "fromString" $ do
        assertEqual "" "123" (A.poking "123")
      ,
      testCase "intercalateVector" $ do
        assertEqual "" "1,2,3,4" (A.poking (F.intercalateVector F.asciiIntegral "," (UnboxedVector.fromList [1 :: Word8, 2, 3, 4])))
    ]
    ,
    parsing
    ,
    testGroup "Regression" [
        testCase "https://github.com/nikita-volkov/hasql-dynamic-statements/issues/2" $
          assertEqual "" "$1000" (A.poking (F.word8 36 <> F.asciiIntegral 1000))
      ]
    ,
    testGroup "Read" $ let
      consumeManyByteStrings :: H.Read a -> [ByteString] -> Maybe a
      consumeManyByteStrings read = \case
        head : tail ->
          H.runOnByteString read head & \case
            Left newRead -> consumeManyByteStrings newRead tail
            Right (res, rem) -> Just res
        _ ->
          Nothing
      againstByteString :: (Eq a, Show a) => (ByteString -> a) -> H.Read a -> [ByteString] -> Property
      againstByteString fromByteString read chunks =
        consumeManyByteStrings read chunks & \case
          Nothing ->
            discard
          Just res ->
            fromByteString (mconcat chunks) === res
      in [
        testProperty "byteString"
          $ \a -> againstByteString (D.take a) (H.byteString (max 0 a))
        ,
        testProperty "skip & byteString" $ \a b ->
          againstByteString
            (D.take b . D.drop a)
            (H.skip (max 0 a) *> H.byteString (max 0 b))
      ]
  ]

parsing :: TestTree
parsing =
  testGroup "Parsing" $ let
    assertParsesTo expected input parser =
      assertEqual "" (Right expected) (A.parse input (fmap Right parser) (Left . Left) (Left . Right))
    in [
        testCase "bytesWhile" $ assertParsesTo "123" "123456" $ G.bytesWhile (< 52)
        ,
        testCase "bytesWhile on full input" $ assertParsesTo "123456" "123456" $ G.bytesWhile (< 59)
        ,
        testCase "skipWhile on full input" $ assertParsesTo () "123456" $ G.skipWhile (< 59)
      ]

pokeThenPeek :: B.Poke a -> C.Peek a -> Maybe (a -> a)
pokeThenPeek (B.Poke pokeSize pokeIO) (C.Peek peekSize peekIO) =
  if pokeSize /= peekSize
    then Nothing
    else Just $ \input -> unsafePerformIO $ do
      fp <- mallocForeignPtrBytes pokeSize
      withForeignPtr fp $ \p -> do
        pokeIO p input
        peekIO p

pokeAndPeek :: E.PokeAndPeek input output -> input -> output
pokeAndPeek (E.PokeAndPeek size poke peek) input =
  unsafePerformIO $ do
    fp <- mallocForeignPtrBytes size
    withForeignPtr fp $ \p -> do
      poke p input
      peek p
