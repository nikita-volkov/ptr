module PtrMagic.Consume
where

import PtrMagic.Prelude
import qualified PtrMagic.IO as A


data Consume consumed =
  Consume !Int !(Ptr Word8 -> IO consumed)

instance Functor Consume where
  {-# INLINE fmap #-}
  fmap fn (Consume size io) =
    Consume size (fmap fn . io)

instance Applicative Consume where
  {-# INLINE pure #-}
  pure x =
    Consume 0 (const (pure x))
  {-# INLINE (<*>) #-}
  (<*>) (Consume leftSize leftIO) (Consume rightSize rightIO) =
    Consume (leftSize + rightSize) io
    where
      io ptr =
        leftIO ptr <*> rightIO (plusPtr ptr leftSize)


{-# INLINE word8 #-}
word8 :: Consume Word8
word8 =
  Consume 1 A.peekWord8

{-# INLINE beWord32 #-}
beWord32 :: Consume Word32
beWord32 =
  Consume 4 A.peekBEWord32

{-# INLINE leWord32 #-}
leWord32 :: Consume Word32
leWord32 =
  Consume 4 A.peekLEWord32

{-# INLINE beWord64 #-}
beWord64 :: Consume Word64
beWord64 =
  Consume 8 A.peekBEWord64

{-# INLINE leWord64 #-}
leWord64 :: Consume Word64
leWord64 =
  Consume 8 A.peekLEWord64

{-# INLINE bytes #-}
bytes :: Int -> Consume ByteString
bytes amount =
  Consume amount (A.peekBytes amount)
