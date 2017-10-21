module PtrMagic.Encode
where

import PtrMagic.Prelude
import qualified PtrMagic.Codec as B


data Encode input =
  Encode !Int !(Ptr Word8 -> input -> IO ())

instance Contravariant Encode where
  {-# INLINE contramap #-}
  contramap fn (Encode size io) =
    Encode size (\ptr input -> io ptr (fn input))

instance Divisible Encode where
  {-# INLINE conquer #-}
  conquer =
    Encode 0 (\_ _ -> pure ())
  {-# INLINE divide #-}
  divide fn (Encode size1 io1) (Encode size2 io2) =
    Encode (size1 + size2) (\ptr input -> case fn input of (input1, input2) -> io1 ptr input1 *> io2 (plusPtr ptr size1) input2)


{-# INLINE word8 #-}
word8 :: Encode Word8
word8 =
  codec B.word8

{-# INLINE beWord32 #-}
beWord32 :: Encode Word32
beWord32 =
  codec B.beWord32

{-# INLINE beWord64 #-}
beWord64 :: Encode Word64
beWord64 =
  codec B.beWord64

{-# INLINE bytes #-}
bytes :: Int -> Encode ByteString
bytes amount =
  codec (B.bytes amount)

{-# INLINE codec #-}
codec :: B.Codec input output -> Encode input
codec (B.Codec size io _) =
  Encode size io
