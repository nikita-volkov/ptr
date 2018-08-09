module Main where

import Prelude
import Bug
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
import qualified Data.ByteString as D
import qualified Data.Vector.Unboxed as UnboxedVector


main =
  defaultMain $
  testGroup "All tests"
  [
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
        assertEqual "" "2017-02-01T05:03:58Z" (A.poking (F.asciiUtcTimeInIso8601 (read "2017-02-01 05:03:58")))
      ,
      testCase "fromString" $ do
        assertEqual "" "123" (A.poking "123")
      ,
      testCase "intercalateVector" $ do
        assertEqual "" "1,2,3,4" (A.poking (F.intercalateVector F.asciiIntegral "," (UnboxedVector.fromList [1 :: Word8, 2, 3, 4])))
    ]
    ,
    parsing
  ]

parsing :: TestTree
parsing =
  testGroup "Parsing" $
  [
    testCase "bytesWhile" $ assertEqual "" "123" (A.parse "123456" (G.bytesWhile (< 52)) undefined undefined)
    ,
    testCase "bytesWhile 2" $ assertEqual "" (Right "123456") (A.parse "123456" (fmap Right (G.bytesWhile (< 59))) (Left . Left) (Left . Right))
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
