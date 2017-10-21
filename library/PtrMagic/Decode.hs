module PtrMagic.Decode
where

import PtrMagic.Prelude
import qualified PtrMagic.Codec as B


data Decode output =
  Decode !Int !(Ptr Word8 -> IO output)

instance Functor Decode where
  {-# INLINE fmap #-}
  fmap fn (Decode size io) =
    Decode size (fmap fn . io)

instance Applicative Decode where
  {-# INLINE pure #-}
  pure x =
    Decode 0 (const (pure x))
  {-# INLINE (<*>) #-}
  (<*>) (Decode leftSize leftIO) (Decode rightSize rightIO) =
    Decode (leftSize + rightSize) io
    where
      io ptr =
        leftIO ptr <*> rightIO (plusPtr ptr leftSize)


{-# INLINE word8 #-}
word8 :: Decode Word8
word8 =
  codec B.word8

{-# INLINE beWord32 #-}
beWord32 :: Decode Word32
beWord32 =
  codec B.beWord32

{-# INLINE beWord64 #-}
beWord64 :: Decode Word64
beWord64 =
  codec B.beWord64

{-# INLINE bytes #-}
bytes :: Int -> Decode ByteString
bytes amount =
  codec (B.bytes amount)

{-# INLINE codec #-}
codec :: B.Codec input output -> Decode output
codec (B.Codec size _ io) =
  Decode size io
