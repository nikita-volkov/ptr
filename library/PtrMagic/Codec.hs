module PtrMagic.Codec
where

import PtrMagic.Prelude
import qualified PtrMagic.IO as A


data Codec value =
  Codec !Int !(Ptr Word8 -> value -> IO ()) !(Ptr Word8 -> IO value)

instance Invariant Codec where
  {-# INLINE invmap #-}
  invmap fn1 fn2 (Codec size poke peek) =
    Codec size (\ptr -> poke ptr . fn2) (\ptr -> fmap fn1 (peek ptr))

{-# INLINE word8 #-}
word8 :: Codec Word8
word8 =
  Codec 1 A.pokeWord8 A.peekWord8

{-# INLINE beWord32 #-}
beWord32 :: Codec Word32
beWord32 =
  Codec 4 A.pokeBEWord32 A.peekBEWord32

{-# INLINE beWord64 #-}
beWord64 :: Codec Word64
beWord64 =
  Codec 8 A.pokeBEWord64 A.peekBEWord64

{-# INLINE bytes #-}
bytes :: Int -> Codec ByteString
bytes amount =
  Codec amount (\ptr -> A.pokeBytesTrimming ptr amount) (\ptr -> A.peekBytes ptr amount)
