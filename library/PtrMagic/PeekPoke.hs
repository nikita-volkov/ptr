module PtrMagic.PeekPoke
where

import PtrMagic.Prelude
import qualified PtrMagic.IO as A


{-|
Encoder and decoder of the same binary representation.

You can compose both the covariant and contravariant parameters of PeekPoke
using Applicative and Profunctor. E.g.,

>word8AndWord32 :: PeekPoke (Word8, Word32) (Word8, Word32)
>word8AndWord32 =
>  (,) <$> lmap fst word8 <*> lmap snd beWord32
-}
data PeekPoke input output =
  PeekPoke !Int (Ptr Word8 -> input -> IO ()) (Ptr Word8 -> IO output)

{-|
A codec, which encodes and decodes the same type. E.g.,

>word8AndWord32 :: InvPeekPoke (Word8, Word32)
>word8AndWord32 =
>  (,) <$> lmap fst word8 <*> lmap snd beWord32
-}
type InvPeekPoke value =
  PeekPoke value value

instance Profunctor PeekPoke where
  {-# INLINE dimap #-}
  dimap fn1 fn2 (PeekPoke size poke peek) =
    PeekPoke size (\ptr -> poke ptr . fn1) (\ptr -> fmap fn2 (peek ptr))

instance Functor (PeekPoke input) where
  {-# INLINE fmap #-}
  fmap fn (PeekPoke size poke peek) =
    PeekPoke size poke (fmap fn . peek)

instance Applicative (PeekPoke input) where
  {-# INLINE pure #-}
  pure x =
    PeekPoke 0 (\_ _ -> pure ()) (\_ -> pure x)
  {-# INLINE (<*>) #-}
  (<*>) (PeekPoke leftSize leftPoke leftPeek) (PeekPoke rightSize rightPoke rightPeek) =
    PeekPoke (leftSize + rightSize) poke peek
    where
      poke ptr input =
        leftPoke ptr input *> rightPoke (plusPtr ptr leftSize) input
      peek ptr =
        leftPeek ptr <*> rightPeek (plusPtr ptr leftSize)

{-# INLINE word8 #-}
word8 :: InvPeekPoke Word8
word8 =
  PeekPoke 1 A.pokeWord8 A.peekWord8

{-# INLINE beWord32 #-}
beWord32 :: InvPeekPoke Word32
beWord32 =
  PeekPoke 4 A.pokeBEWord32 A.peekBEWord32

{-# INLINE beWord64 #-}
beWord64 :: InvPeekPoke Word64
beWord64 =
  PeekPoke 8 A.pokeBEWord64 A.peekBEWord64

{-# INLINE bytes #-}
bytes :: Int -> InvPeekPoke ByteString
bytes amount =
  PeekPoke amount (\ptr -> A.pokeBytesTrimming ptr amount) (\ptr -> A.peekBytes ptr amount)
