module PtrMagic.DynamicDecoder
where

import PtrMagic.Prelude
import qualified PtrMagic.IO as A


{-|
An action reading all the available bytes.
-}
newtype DynamicDecoder output =
  DynamicDecoder (Ptr Word8 -> Int -> IO output)

instance Functor DynamicDecoder where
  {-# INLINE fmap #-}
  fmap fn (DynamicDecoder io) =
    DynamicDecoder (\ptr space -> fmap fn (io ptr space))

{-# INLINE bytes #-}
bytes :: DynamicDecoder ByteString
bytes =
  DynamicDecoder (\ptr space -> A.peekBytes ptr space)
