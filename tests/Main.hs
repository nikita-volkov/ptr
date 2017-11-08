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
import qualified Data.ByteString as D


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
      testCase "paddedAndTrimmedAsciiIntegral" $ do
        assertEqual "" "001" (A.poking (F.paddedAndTrimmedAsciiIntegral 3 1))
        assertEqual "" "001" (A.poking (F.paddedAndTrimmedAsciiIntegral 3 2001))
        assertEqual "" "000" (A.poking (F.paddedAndTrimmedAsciiIntegral 3 (-1)))
      ,
      testCase "utcTimeInIso8601InAscii" $ do
        assertEqual "" "2017-02-01T05:03:58Z" (A.poking (F.utcTimeInIso8601InAscii (read "2017-02-01 05:03:58")))
    ]
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
