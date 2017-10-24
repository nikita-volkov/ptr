module Ptr.Take
where

import Ptr.Prelude hiding (peek, take)
import qualified Ptr.PokeAndPeek as A
import qualified Data.ByteString.Char8 as B
import qualified Ptr.Prelude as C
import qualified Ptr.IO as D


newtype Take output =
  Take (StateT (Int, Ptr Word8) (MaybeT IO) output)
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus)

{-# INLINE take #-}
take :: (Int -> Ptr Word8 -> IO (Maybe (a, (Int, Ptr Word8)))) -> Take a
take io =
  Take (StateT (\(!availableAmount, !ptr) -> MaybeT (io availableAmount ptr)))

{-# INLINE pokeAndPeek #-}
pokeAndPeek :: A.PokeAndPeek input output -> Take output
pokeAndPeek (A.PokeAndPeek requiredAmount _ ptrIO) =
  take $ \availableAmount ptr ->
  if availableAmount >= requiredAmount
    then do
      result <- ptrIO ptr
      return (Just (result, (availableAmount - requiredAmount, plusPtr ptr requiredAmount)))
    else return Nothing

{-# INLINE word8 #-}
word8 :: Take Word8
word8 =
  {-# SCC "word8" #-} 
  pokeAndPeek A.word8

{-# INLINE beWord16 #-}
beWord16 :: Take Word16
beWord16 =
  {-# SCC "beWord16" #-} 
  pokeAndPeek A.beWord16

{-# INLINE beWord32 #-}
beWord32 :: Take Word32
beWord32 =
  {-# SCC "beWord32" #-} 
  pokeAndPeek A.beWord32

{-# INLINE beWord64 #-}
beWord64 :: Take Word64
beWord64 =
  {-# SCC "beWord64" #-} 
  pokeAndPeek A.beWord64

{-# INLINE bytes #-}
bytes :: Int -> Take ByteString
bytes amount =
  {-# SCC "bytes" #-} 
  pokeAndPeek (A.bytes amount)

{-# INLINE allBytes #-}
allBytes :: Take ByteString
allBytes =
  {-# SCC "allBytes" #-} 
  take $ \availableAmount ptr -> do
    bytes <- D.peekBytes ptr availableAmount
    return (Just (bytes, (0, plusPtr ptr availableAmount)))

{-# INLINE nullTerminatedBytes #-}
nullTerminatedBytes :: Take ByteString
nullTerminatedBytes =
  {-# SCC "nullTerminatedBytes" #-} 
  take $ \availableAmount ptr -> do
    bytes <- B.packCString (castPtr ptr)
    case succ (B.length bytes) of
      consumedAmount -> if consumedAmount <= availableAmount
        then return (Just (bytes, (availableAmount - consumedAmount, plusPtr ptr consumedAmount)))
        else return Nothing

{-# INLINE bytesWhile #-}
bytesWhile :: (Word8 -> Bool) -> Take ByteString
bytesWhile predicate =
  {-# SCC "bytesWhile" #-} 
  take (\availableAmount -> iterate availableAmount availableAmount)
  where
    iterate !availableAmount !unconsumedAmount !ptr =
      if unconsumedAmount > 0
        then do
          byte <- C.peek ptr
          if predicate byte
            then iterate availableAmount (pred unconsumedAmount) (plusPtr ptr 1)
            else do
              bytes <- B.packCStringLen (castPtr ptr, availableAmount - unconsumedAmount)
              return (Just (bytes, (unconsumedAmount, ptr)))
        else return Nothing

{-# INLINE skipWhile #-}
skipWhile :: (Word8 -> Bool) -> Take ()
skipWhile predicate =
  {-# SCC "skipWhile" #-} 
  take (\availableAmount -> iterate availableAmount availableAmount)
  where
    iterate !availableAmount !unconsumedAmount !ptr =
      if unconsumedAmount > 0
        then do
          byte <- C.peek ptr
          if predicate byte
            then iterate availableAmount (pred unconsumedAmount) (plusPtr ptr 1)
            else return (Just ((), (unconsumedAmount, ptr)))
        else return Nothing

{-# INLINE foldWhile #-}
foldWhile :: (Word8 -> Bool) -> (state -> Word8 -> state) -> state -> Take state
foldWhile predicate step start =
  {-# SCC "foldWhile" #-} 
  take (iterate start)
  where
    iterate !state !unconsumedAmount !ptr =
      if unconsumedAmount > 0
        then do
          byte <- C.peek ptr
          if predicate byte
            then iterate (step state byte) (pred unconsumedAmount) (plusPtr ptr 1)
            else return (Just (state, (unconsumedAmount, ptr)))
        else return Nothing

-- |
-- Unsigned integral number encoded in ASCII.
{-# INLINE unsignedASCIIIntegral #-}
unsignedASCIIIntegral :: Integral a => Take a
unsignedASCIIIntegral =
  {-# SCC "unsignedASCIIIntegral" #-} 
  foldWhile byteIsDigit step 0
  where
    byteIsDigit byte =
      byte - 48 <= 9
    step !state !byte =
      state * 10 + fromIntegral byte - 48
