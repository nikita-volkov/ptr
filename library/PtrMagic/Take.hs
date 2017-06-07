module PtrMagic.Take
where

import PtrMagic.Prelude
import qualified PtrMagic.IO as A


data Take taken =
  Take !Int !(Ptr Word8 -> IO taken)

instance Functor Take where
  {-# INLINE fmap #-}
  fmap fn (Take size io) =
    Take size (fmap fn . io)

instance Applicative Take where
  {-# INLINE pure #-}
  pure x =
    Take 0 (const (pure x))
  {-# INLINE (<*>) #-}
  (<*>) (Take leftSize leftIO) (Take rightSize rightIO) =
    Take (leftSize + rightSize) io
    where
      io ptr =
        leftIO ptr <*> rightIO (plusPtr ptr leftSize)


{-# INLINE word8 #-}
word8 :: Take Word8
word8 =
  Take 1 A.peekWord8

{-# INLINE beWord32 #-}
beWord32 :: Take Word32
beWord32 =
  Take 4 A.peekBEWord32

{-# INLINE leWord32 #-}
leWord32 :: Take Word32
leWord32 =
  Take 4 A.peekLEWord32

{-# INLINE beWord64 #-}
beWord64 :: Take Word64
beWord64 =
  Take 8 A.peekBEWord64

{-# INLINE leWord64 #-}
leWord64 :: Take Word64
leWord64 =
  Take 8 A.peekLEWord64

{-# INLINE bytes #-}
bytes :: Int -> Take ByteString
bytes amount =
  Take amount (\ptr -> A.peekBytes ptr amount)
