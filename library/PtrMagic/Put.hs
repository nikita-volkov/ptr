module PtrMagic.Put
where

import PtrMagic.Prelude
import qualified PtrMagic.IO as A


data Put put =
  Put !Int !(Ptr Word8 -> put -> IO ())

instance Contravariant Put where
  {-# INLINE contramap #-}
  contramap fn (Put size io) =
    Put size (\ptr input -> io ptr (fn input))

instance Divisible Put where
  {-# INLINE conquer #-}
  conquer =
    Put 0 (\_ _ -> pure ())
  {-# INLINE divide #-}
  divide fn (Put size1 io1) (Put size2 io2) =
    Put (size1 + size2) (\ptr input -> case fn input of (input1, input2) -> io1 ptr input1 *> io2 (plusPtr ptr size1) input2)


{-# INLINE word8 #-}
word8 :: Put Word8
word8 =
  Put 1 A.pokeWord8

{-# INLINE beWord32 #-}
beWord32 :: Put Word32
beWord32 =
  Put 4 A.pokeBEWord32

{-# INLINE beWord64 #-}
beWord64 :: Put Word64
beWord64 =
  Put 8 A.pokeBEWord64

{-# INLINE bytes #-}
bytes :: Int -> Put ByteString
bytes amount =
  Put amount (\ptr -> A.pokeBytes ptr amount)
