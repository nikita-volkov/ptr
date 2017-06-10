module PtrMagic.Pull
where

import PtrMagic.Prelude
import qualified PtrMagic.IO as A
import qualified PtrMagic.Codec as B


data Pull pulled =
  Pull !Int !(Ptr Word8 -> IO pulled)

instance Functor Pull where
  {-# INLINE fmap #-}
  fmap fn (Pull size io) =
    Pull size (fmap fn . io)

instance Applicative Pull where
  {-# INLINE pure #-}
  pure x =
    Pull 0 (const (pure x))
  {-# INLINE (<*>) #-}
  (<*>) (Pull leftSize leftIO) (Pull rightSize rightIO) =
    Pull (leftSize + rightSize) io
    where
      io ptr =
        leftIO ptr <*> rightIO (plusPtr ptr leftSize)


{-# INLINE word8 #-}
word8 :: Pull Word8
word8 =
  Pull 1 A.peekWord8

{-# INLINE beWord32 #-}
beWord32 :: Pull Word32
beWord32 =
  Pull 4 A.peekBEWord32

{-# INLINE leWord32 #-}
leWord32 :: Pull Word32
leWord32 =
  Pull 4 A.peekLEWord32

{-# INLINE beWord64 #-}
beWord64 :: Pull Word64
beWord64 =
  Pull 8 A.peekBEWord64

{-# INLINE leWord64 #-}
leWord64 :: Pull Word64
leWord64 =
  Pull 8 A.peekLEWord64

{-# INLINE bytes #-}
bytes :: Int -> Pull ByteString
bytes amount =
  Pull amount (\ptr -> A.peekBytes ptr amount)

{-# INLINE codec #-}
codec :: B.Codec value -> Pull value
codec (B.Codec size _ io) =
  Pull size io
