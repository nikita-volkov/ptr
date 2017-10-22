module PtrMagic.Poke
where

import PtrMagic.Prelude
import qualified PtrMagic.Codec as B


data Poke input =
  Poke !Int !(Ptr Word8 -> input -> IO ())

instance Contravariant Poke where
  {-# INLINE contramap #-}
  contramap fn (Poke size io) =
    Poke size (\ptr input -> io ptr (fn input))

instance Divisible Poke where
  {-# INLINE conquer #-}
  conquer =
    Poke 0 (\_ _ -> pure ())
  {-# INLINE divide #-}
  divide fn (Poke size1 io1) (Poke size2 io2) =
    Poke (size1 + size2) (\ptr input -> case fn input of (input1, input2) -> io1 ptr input1 *> io2 (plusPtr ptr size1) input2)


{-# INLINE word8 #-}
word8 :: Poke Word8
word8 =
  codec B.word8

{-# INLINE beWord32 #-}
beWord32 :: Poke Word32
beWord32 =
  codec B.beWord32

{-# INLINE beWord64 #-}
beWord64 :: Poke Word64
beWord64 =
  codec B.beWord64

{-# INLINE bytes #-}
bytes :: Int -> Poke ByteString
bytes amount =
  codec (B.bytes amount)

{-# INLINE codec #-}
codec :: B.Codec input output -> Poke input
codec (B.Codec size io _) =
  Poke size io
