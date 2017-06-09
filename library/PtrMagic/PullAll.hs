module PtrMagic.PullAll
where

import PtrMagic.Prelude
import qualified PtrMagic.IO as A


{-|
An action reading all the available bytes.
-}
newtype PullAll pulled =
  PullAll (Ptr Word8 -> Int -> IO pulled)

instance Functor PullAll where
  {-# INLINE fmap #-}
  fmap fn (PullAll io) =
    PullAll (\ptr space -> fmap fn (io ptr space))

{-# INLINE bytes #-}
bytes :: PullAll ByteString
bytes =
  PullAll (\ptr space -> A.peekBytes ptr space)
