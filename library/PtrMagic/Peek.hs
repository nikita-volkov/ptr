module PtrMagic.Peek
where

import PtrMagic.Prelude
import qualified PtrMagic.PeekPoke as B


data Peek output =
  Peek !Int !(Ptr Word8 -> IO output)

instance Functor Peek where
  {-# INLINE fmap #-}
  fmap fn (Peek size io) =
    Peek size (fmap fn . io)

instance Applicative Peek where
  {-# INLINE pure #-}
  pure x =
    Peek 0 (const (pure x))
  {-# INLINE (<*>) #-}
  (<*>) (Peek leftSize leftIO) (Peek rightSize rightIO) =
    Peek (leftSize + rightSize) io
    where
      io ptr =
        leftIO ptr <*> rightIO (plusPtr ptr leftSize)


{-# INLINE word8 #-}
word8 :: Peek Word8
word8 =
  peekPoke B.word8

{-# INLINE beWord32 #-}
beWord32 :: Peek Word32
beWord32 =
  peekPoke B.beWord32

{-# INLINE beWord64 #-}
beWord64 :: Peek Word64
beWord64 =
  peekPoke B.beWord64

{-# INLINE bytes #-}
bytes :: Int -> Peek ByteString
bytes amount =
  peekPoke (B.bytes amount)

{-# INLINE peekPoke #-}
peekPoke :: B.PeekPoke input output -> Peek output
peekPoke (B.PeekPoke size _ io) =
  Peek size io
