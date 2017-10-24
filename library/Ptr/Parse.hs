module Ptr.Parse
where

import Ptr.Prelude hiding (peek, take)
import qualified Ptr.PokeAndPeek as A
import qualified Data.ByteString.Char8 as B
import qualified Ptr.Prelude as C
import qualified Ptr.IO as D


newtype Parse output =
  Parse (StateT (Int, Ptr Word8) (ExceptT Text IO) output)
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadError Text)

{-# INLINE take #-}
take :: (Int -> Ptr Word8 -> IO (Either Text (a, (Int, Ptr Word8)))) -> Parse a
take io =
  Parse (StateT (\(availableAmount, ptr) -> ExceptT (io availableAmount ptr)))

{-# INLINE pokeAndPeek #-}
pokeAndPeek :: A.PokeAndPeek input output -> Parse output
pokeAndPeek (A.PokeAndPeek requiredAmount _ ptrIO) =
  take $ \availableAmount ptr ->
  if availableAmount >= requiredAmount
    then do
      result <- ptrIO ptr
      return (Right (result, (availableAmount - requiredAmount, plusPtr ptr requiredAmount)))
    else return (Left "End of input")

{-# INLINE word8 #-}
word8 :: Parse Word8
word8 =
  pokeAndPeek A.word8

{-# INLINE beWord16 #-}
beWord16 :: Parse Word16
beWord16 =
  pokeAndPeek A.beWord16

{-# INLINE beWord32 #-}
beWord32 :: Parse Word32
beWord32 =
  pokeAndPeek A.beWord32

{-# INLINE beWord64 #-}
beWord64 :: Parse Word64
beWord64 =
  pokeAndPeek A.beWord64

{-# INLINE bytes #-}
bytes :: Int -> Parse ByteString
bytes amount =
  pokeAndPeek (A.bytes amount)

{-# INLINE allBytes #-}
allBytes :: Parse ByteString
allBytes =
  take $ \availableAmount ptr -> do
    bytes <- D.peekBytes ptr availableAmount
    return (Right (bytes, (0, plusPtr ptr availableAmount)))

{-# INLINE nullTerminatedBytes #-}
nullTerminatedBytes :: Parse ByteString
nullTerminatedBytes =
  take $ \availableAmount ptr -> do
    bytes <- B.packCString (castPtr ptr)
    case succ (B.length bytes) of
      consumedAmount -> if consumedAmount <= availableAmount
        then return (Right (bytes, (availableAmount - consumedAmount, plusPtr ptr consumedAmount)))
        else return (Left "End of input")

{-# INLINE bytesWhile #-}
bytesWhile :: (Word8 -> Bool) -> Parse ByteString
bytesWhile predicate =
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
              return (Right (bytes, (unconsumedAmount, ptr)))
        else return (Left "End of input")

{-# INLINE skipWhile #-}
skipWhile :: (Word8 -> Bool) -> Parse ()
skipWhile predicate =
  take (\availableAmount -> iterate availableAmount availableAmount)
  where
    iterate !availableAmount !unconsumedAmount !ptr =
      if unconsumedAmount > 0
        then do
          byte <- C.peek ptr
          if predicate byte
            then iterate availableAmount (pred unconsumedAmount) (plusPtr ptr 1)
            else return (Right ((), (unconsumedAmount, ptr)))
        else return (Left "End of input")

{-# INLINE foldWhile #-}
foldWhile :: (Word8 -> Bool) -> (state -> Word8 -> state) -> state -> Parse state
foldWhile predicate step start =
  take (iterate start)
  where
    iterate !state !unconsumedAmount !ptr =
      if unconsumedAmount > 0
        then do
          byte <- C.peek ptr
          if predicate byte
            then iterate (step state byte) (pred unconsumedAmount) (plusPtr ptr 1)
            else return (Right (state, (unconsumedAmount, ptr)))
        else return (Left "End of input")

-- |
-- Unsigned integral number encoded in ASCII.
{-# INLINE unsignedASCIIIntegral #-}
unsignedASCIIIntegral :: Integral a => Parse a
unsignedASCIIIntegral =
  foldWhile byteIsDigit step 0
  where
    byteIsDigit byte =
      byte - 48 <= 9
    step !state !byte =
      state * 10 + fromIntegral byte - 48
