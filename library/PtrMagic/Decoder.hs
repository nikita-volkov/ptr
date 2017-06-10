module PtrMagic.Decoder
where

import PtrMagic.Prelude
import qualified PtrMagic.IO as A
import qualified PtrMagic.Codec as B


data Decoder output =
  Decoder !Int !(Ptr Word8 -> IO output)

instance Functor Decoder where
  {-# INLINE fmap #-}
  fmap fn (Decoder size io) =
    Decoder size (fmap fn . io)

instance Applicative Decoder where
  {-# INLINE pure #-}
  pure x =
    Decoder 0 (const (pure x))
  {-# INLINE (<*>) #-}
  (<*>) (Decoder leftSize leftIO) (Decoder rightSize rightIO) =
    Decoder (leftSize + rightSize) io
    where
      io ptr =
        leftIO ptr <*> rightIO (plusPtr ptr leftSize)


{-# INLINE word8 #-}
word8 :: Decoder Word8
word8 =
  Decoder 1 A.peekWord8

{-# INLINE beWord32 #-}
beWord32 :: Decoder Word32
beWord32 =
  Decoder 4 A.peekBEWord32

{-# INLINE leWord32 #-}
leWord32 :: Decoder Word32
leWord32 =
  Decoder 4 A.peekLEWord32

{-# INLINE beWord64 #-}
beWord64 :: Decoder Word64
beWord64 =
  Decoder 8 A.peekBEWord64

{-# INLINE leWord64 #-}
leWord64 :: Decoder Word64
leWord64 =
  Decoder 8 A.peekLEWord64

{-# INLINE bytes #-}
bytes :: Int -> Decoder ByteString
bytes amount =
  Decoder amount (\ptr -> A.peekBytes ptr amount)

{-# INLINE codec #-}
codec :: B.Codec input output -> Decoder output
codec (B.Codec size _ io) =
  Decoder size io
