module PtrMagic.Take
where

import PtrMagic.Prelude hiding (peek, take)
import qualified PtrMagic.Peek as A
import qualified Data.ByteString.Char8 as B
import qualified PtrMagic.Prelude as C


newtype Take output =
  Take (StateT (Int, Ptr Word8) (MaybeT IO) output)
  deriving (Functor, Applicative, Monad)

{-# INLINE take #-}
take :: (Int -> Ptr Word8 -> IO (Maybe (a, (Int, Ptr Word8)))) -> Take a
take io =
  Take (StateT (\(limitAmount, ptr) -> MaybeT (io limitAmount ptr)))

{-# INLINE peek #-}
peek :: A.Peek a -> Take a
peek (A.Peek requiredAmount ptrIO) =
  Take $ StateT $ \(limitAmount, ptr) -> MaybeT $
  if requiredAmount <= limitAmount
    then return Nothing
    else do
      result <- ptrIO ptr
      return (Just (result, (limitAmount - requiredAmount, plusPtr ptr requiredAmount)))

{-# INLINE word8 #-}
word8 :: Take Word8
word8 =
  peek A.word8

{-# INLINE beWord16 #-}
beWord16 :: Take Word16
beWord16 =
  peek A.beWord16

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
  take $ \limitAmount ptr -> do
    bytes <- B.packCString (castPtr ptr)
    case succ (B.length bytes) of
      consumedAmount -> if consumedAmount <= limitAmount
        then return (Just (bytes, (limitAmount - consumedAmount, plusPtr ptr consumedAmount)))
        else return Nothing

{-# INLINE bytesWhile #-}
bytesWhile :: (Word8 -> Bool) -> Take ByteString
bytesWhile predicate =
  take $ \limitAmount ptr -> do
    (newPtr, consumedAmount) <- iterate ptr 0
    if consumedAmount <= limitAmount
      then do
        bytes <- B.packCStringLen (castPtr ptr, consumedAmount)
        return (Just (bytes, (limitAmount - consumedAmount, newPtr)))
      else return Nothing
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
  take $ \limitAmount ptr -> do
    (newPtr, consumedAmount) <- iterate ptr 0
    if consumedAmount <= limitAmount
      then return (Just ((), (limitAmount - consumedAmount, newPtr)))
      else return Nothing
  where
    iterate !ptr !i =
      do
        byte <- C.peek ptr
        if predicate byte
          then iterate (plusPtr ptr 1) (succ i)
          else return (ptr, i)

{-# INLINE foldWhile #-}
foldWhile :: (Word8 -> Bool) -> (state -> Word8 -> state) -> state -> Take state
foldWhile predicate step start =
  take (iterate start)
  where
    iterate !state !limit !ptr =
      if limit > 0
        then do
          byte <- C.peek ptr
          if predicate byte
            then iterate (step state byte) (pred limit) (plusPtr ptr (-1))
            else return (Just (state, (limit, ptr)))
        else return Nothing

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
