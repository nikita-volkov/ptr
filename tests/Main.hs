module Main where

import Prelude
import Bug
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.QuickCheck.Instances
import qualified PtrMagic.Poke as B
import qualified PtrMagic.Peek as C
import qualified PtrMagic.Codec as E
import qualified Data.ByteString as D


main =
  defaultMain $
  testGroup "All tests"
  [
    testProperty "Poke and peek (bytes)" $ \input -> input === fromJust (pokeAndPeek (B.bytes (D.length input)) (C.bytes (D.length input))) input
    ,
    testProperty "Poke and peek (word8)" $ \input -> input === fromJust (pokeAndPeek B.word8 C.word8) input
    ,
    testProperty "Poke and peek (beWord32)" $ \input -> input === fromJust (pokeAndPeek B.beWord32 C.beWord32) input
    ,
    testProperty "Poke and peek (beWord64)" $ \input -> input === fromJust (pokeAndPeek B.beWord64 C.beWord64) input
    ,
    testProperty "Codec composition" $ \input -> input === codec ((,) <$> lmap fst E.word8 <*> lmap snd E.beWord32) input
  ]

pokeAndPeek :: B.Poke a -> C.Peek a -> Maybe (a -> a)
pokeAndPeek (B.Poke pokeSize pokeIO) (C.Peek peekSize peekIO) =
  if pokeSize /= peekSize
    then Nothing
    else Just $ \input -> unsafePerformIO $ do
      fp <- mallocForeignPtrBytes pokeSize
      withForeignPtr fp $ \p -> do
        pokeIO p input
        peekIO p

codec :: E.Codec input output -> input -> output
codec (E.Codec size poke peek) input =
  unsafePerformIO $ do
    fp <- mallocForeignPtrBytes size
    withForeignPtr fp $ \p -> do
      poke p input
      peek p
