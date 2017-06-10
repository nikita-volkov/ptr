module PtrMagic.Encoding
where

import PtrMagic.Prelude
import qualified PtrMagic.IO as A
import qualified PtrMagic.Encoder as C
import qualified Data.ByteString.Internal as B


{-|
Efficiently composable specification of how to encode data.
-}
data Encoding =
  {-|
  * Amount of bytes the encoded data will occupy.
  * Exception-free action, which populates the pointer to the encoded data.
  -}
  Encoding !Int !(Ptr Word8 -> IO ())

instance Semigroup Encoding where
  {-# INLINE (<>) #-}
  (<>) (Encoding space1 action1) (Encoding space2 action2) =
    Encoding (space1 + space2) (\ptr -> action1 ptr *> action2 (plusPtr ptr space1))

instance Monoid Encoding where
  {-# INLINE mempty #-}
  mempty =
    Encoding 0 (const (pure ()))
  {-# INLINE mappend #-}
  mappend =
    (<>)

{-|
Same as 'mappend' and '<>',
but performs the serialization concurrently.
This comes at the cost of an overhead,
so it is only advised to use this function when the merged serializations are heavy.
-}
prependConc :: Encoding -> Encoding -> Encoding
prependConc (Encoding space1 action1) (Encoding space2 action2) =
  Encoding (space1 + space2) action
  where
    action ptr =
      do
        lock <- newEmptyMVar
        forkIO $ do
          action1 ptr
          putMVar lock ()
        action2 (plusPtr ptr space1)
        takeMVar lock

{-# INLINE word8 #-}
word8 :: Word8 -> Encoding
word8 x =
  Encoding 1 (flip A.pokeWord8 x)

{-# INLINE beWord32 #-}
beWord32 :: Word32 -> Encoding
beWord32 x =
  Encoding 4 (flip A.pokeBEWord32 x)

{-# INLINE beWord64 #-}
beWord64 :: Word64 -> Encoding
beWord64 x =
  Encoding 8 (flip A.pokeBEWord64 x)

{-# INLINE bytes #-}
bytes :: ByteString -> Encoding
bytes (B.PS bytesFPtr offset length) =
  Encoding length (\ptr -> withForeignPtr bytesFPtr (\bytesPtr -> B.memcpy ptr (plusPtr bytesPtr offset) length))

{-# INLINE encoder #-}
encoder :: C.Encoder input -> input -> Encoding
encoder (C.Encoder space action) input =
  Encoding space (\ptr -> action ptr input)
