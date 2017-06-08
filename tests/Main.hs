module Main where

import Prelude
import Bug
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.QuickCheck.Instances
import qualified PtrMagic.Push as B
import qualified PtrMagic.Pull as C
import qualified Data.ByteString as D


main =
  defaultMain $
  testGroup "All tests"
  [
    testProperty "Push and pull (bytes)" $ \input -> input === fromJust (pushAndPull (B.bytes (D.length input)) (C.bytes (D.length input))) input
    ,
    testProperty "Push and pull (word8)" $ \input -> input === fromJust (pushAndPull B.word8 C.word8) input
    ,
    testProperty "Push and pull (beWord32)" $ \input -> input === fromJust (pushAndPull B.beWord32 C.beWord32) input
    ,
    testProperty "Push and pull (beWord64)" $ \input -> input === fromJust (pushAndPull B.beWord64 C.beWord64) input
  ]

pushAndPull :: B.Push a -> C.Pull a -> Maybe (a -> a)
pushAndPull (B.Push pushSize pushIO) (C.Pull pullSize pullIO) =
  if pushSize /= pullSize
    then Nothing
    else Just $ \input -> unsafePerformIO $ do
      fp <- mallocForeignPtrBytes pushSize
      withForeignPtr fp $ \p -> do
        pushIO p input
        pullIO p
