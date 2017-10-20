module PtrMagic.Decoder
where

import PtrMagic.Prelude
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
  codec B.word8

{-# INLINE beWord32 #-}
beWord32 :: Decoder Word32
beWord32 =
  codec B.beWord32

{-# INLINE beWord64 #-}
beWord64 :: Decoder Word64
beWord64 =
  codec B.beWord64

{-# INLINE bytes #-}
bytes :: Int -> Decoder ByteString
bytes amount =
  codec (B.bytes amount)

{-# INLINE codec #-}
codec :: B.Codec input output -> Decoder output
codec (B.Codec size _ io) =
  Decoder size io
