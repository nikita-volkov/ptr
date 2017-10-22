module PtrMagic.Take
where

import PtrMagic.Prelude hiding (peek)
import qualified PtrMagic.Peek as A
import qualified Data.ByteString.Char8 as B
import qualified PtrMagic.Prelude as C


newtype Take output =
  Take (StateT (Ptr Word8) IO output)
  deriving (Functor, Applicative, Monad)

{-# INLINE peek #-}
peek :: A.Peek a -> Take a
peek (A.Peek amount ptrIO) =
  Take (StateT (\ptr -> fmap (\output -> (output, plusPtr ptr amount)) (ptrIO ptr)))

{-# INLINE word8 #-}
word8 :: Take Word8
word8 =
  peek A.word8

{-# INLINE beWord32 #-}
beWord32 :: Take Word32
beWord32 =
  peek A.beWord32

{-# INLINE beWord64 #-}
beWord64 :: Take Word64
beWord64 =
  peek A.beWord64

{-# INLINE bytes #-}
bytes :: Int -> Take ByteString
bytes amount =
  peek (A.bytes amount)

{-# INLINE nullTerminatedBytes #-}
nullTerminatedBytes :: Take ByteString
nullTerminatedBytes =
  Take $ StateT $ \ptr ->
  fmap (\bytes -> (bytes, plusPtr ptr (succ (B.length bytes)))) (B.packCString (castPtr ptr))

{-# INLINE bytesWhile #-}
bytesWhile :: (Word8 -> Bool) -> Take ByteString
bytesWhile predicate =
  Take $ StateT $ \ptr -> do
    (newPtr, length) <- iterate ptr 0
    bytes <- B.packCStringLen (castPtr ptr, length)
    return (bytes, newPtr)
  where
    iterate !ptr !i =
      do
        byte <- C.peek ptr
        if predicate byte
          then iterate (plusPtr ptr 1) (succ i)
          else return (ptr, i)

{-# INLINE skipWhile #-}
skipWhile :: (Word8 -> Bool) -> Take ()
skipWhile predicate =
  Take $ StateT $ \ptr -> do
    newPtr <- iterate ptr
    return ((), newPtr)
  where
    iterate !ptr =
      do
        byte <- C.peek ptr
        if predicate byte
          then iterate (plusPtr ptr 1)
          else return ptr

{-# INLINE foldWhile #-}
foldWhile :: (Word8 -> Bool) -> (state -> Word8 -> state) -> state -> Take state
foldWhile predicate step start =
  Take $ StateT $ \ptr -> iterate ptr start
  where
    iterate !ptr !state =
      do
        byte <- C.peek ptr
        if predicate byte
          then iterate (plusPtr ptr 1) (step state byte)
          else return (state, ptr)

-- |
-- Unsigned integral number encoded in ASCII.
{-# INLINE unsignedASCIIIntegral #-}
unsignedASCIIIntegral :: Integral a => Take a
unsignedASCIIIntegral =
  foldWhile byteIsDigit step 0
  where
    byteIsDigit byte =
      byte - 48 <= 9
    step !state !byte =
      state * 10 + fromIntegral byte - 48
