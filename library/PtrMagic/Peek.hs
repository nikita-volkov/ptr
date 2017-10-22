module PtrMagic.Peek
where

import PtrMagic.Prelude
import qualified PtrMagic.Codec as B


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
  codec B.word8

{-# INLINE beWord32 #-}
beWord32 :: Peek Word32
beWord32 =
  codec B.beWord32

{-# INLINE beWord64 #-}
beWord64 :: Peek Word64
beWord64 =
  codec B.beWord64

{-# INLINE bytes #-}
bytes :: Int -> Peek ByteString
bytes amount =
  codec (B.bytes amount)

{-# INLINE codec #-}
codec :: B.Codec input output -> Peek output
codec (B.Codec size _ io) =
  Peek size io
