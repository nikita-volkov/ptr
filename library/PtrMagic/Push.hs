module PtrMagic.Push
where

import PtrMagic.Prelude
import qualified PtrMagic.IO as A
import qualified PtrMagic.Codec as B


data Push pushed =
  Push !Int !(Ptr Word8 -> pushed -> IO ())

instance Contravariant Push where
  {-# INLINE contramap #-}
  contramap fn (Push size io) =
    Push size (\ptr input -> io ptr (fn input))

instance Divisible Push where
  {-# INLINE conquer #-}
  conquer =
    Push 0 (\_ _ -> pure ())
  {-# INLINE divide #-}
  divide fn (Push size1 io1) (Push size2 io2) =
    Push (size1 + size2) (\ptr input -> case fn input of (input1, input2) -> io1 ptr input1 *> io2 (plusPtr ptr size1) input2)


{-# INLINE word8 #-}
word8 :: Push Word8
word8 =
  Push 1 A.pokeWord8

{-# INLINE beWord32 #-}
beWord32 :: Push Word32
beWord32 =
  Push 4 A.pokeBEWord32

{-# INLINE beWord64 #-}
beWord64 :: Push Word64
beWord64 =
  Push 8 A.pokeBEWord64

{-# INLINE bytes #-}
bytes :: Int -> Push ByteString
bytes amount =
  Push amount (\ptr -> A.pokeBytesTrimming ptr amount)

{-# INLINE codec #-}
codec :: B.Codec input output -> Push input
codec (B.Codec size io _) =
  Push size io
