module PtrMagic.Codec
where

import PtrMagic.Prelude
import qualified PtrMagic.IO as A


{-|
Encoder and decoder of the same binary representation.
-}
data Codec input output =
  Codec !Int !(Ptr Word8 -> input -> IO ()) !(Ptr Word8 -> IO output)

instance Profunctor Codec where
  {-# INLINE dimap #-}
  dimap fn1 fn2 (Codec size poke peek) =
    Codec size (\ptr -> poke ptr . fn1) (\ptr -> fmap fn2 (peek ptr))

instance Functor (Codec input) where
  {-# INLINE fmap #-}
  fmap fn (Codec size poke peek) =
    Codec size poke (fmap fn . peek)

instance Applicative (Codec input) where
  {-# INLINE pure #-}
  pure x =
    Codec 0 (\_ _ -> pure ()) (\_ -> pure x)
  {-# INLINE (<*>) #-}
  (<*>) (Codec leftSize leftPoke leftPeek) (Codec rightSize rightPoke rightPeek) =
    Codec (leftSize + rightSize) poke peek
    where
      poke ptr input =
        leftPoke ptr input *> rightPoke (plusPtr ptr leftSize) input
      peek ptr =
        leftPeek ptr <*> rightPeek (plusPtr ptr leftSize)

{-# INLINE word8 #-}
word8 :: Codec Word8 Word8
word8 =
  Codec 1 A.pokeWord8 A.peekWord8

{-# INLINE beWord32 #-}
beWord32 :: Codec Word32 Word32
beWord32 =
  Codec 4 A.pokeBEWord32 A.peekBEWord32

{-# INLINE beWord64 #-}
beWord64 :: Codec Word64 Word64
beWord64 =
  Codec 8 A.pokeBEWord64 A.peekBEWord64

{-# INLINE bytes #-}
bytes :: Int -> Codec ByteString ByteString
bytes amount =
  Codec amount (\ptr -> A.pokeBytesTrimming ptr amount) (\ptr -> A.peekBytes ptr amount)
