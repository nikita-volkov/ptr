module Ptr.Take
where

import Ptr.Prelude hiding (peek, take)
import qualified Ptr.PokeAndPeek as A
import qualified Data.ByteString.Char8 as B
import qualified Ptr.Prelude as C
import qualified Ptr.IO as D


newtype Take output =
  Take (Int -> Ptr Word8 -> forall result. (Int -> IO result) -> (Text -> IO result) -> (output -> Int -> Ptr Word8 -> IO result) -> IO result)

deriving instance Functor Take

instance Applicative Take where
  pure x =
    Take (\ availableAmount ptr _ _ succeed -> succeed x availableAmount ptr)
  {-# INLINE (<*>) #-}
  (<*>) (Take left) (Take right) =
    Take $ \ availableAmount ptr failWithEOI failWithMessage succeed ->
    left availableAmount ptr failWithEOI failWithMessage $ \ leftOutput leftAvailableAmount leftPtr ->
    right leftAvailableAmount leftPtr failWithEOI failWithMessage $ \ rightOutput rightAvailableAmount rightPtr ->
    succeed (leftOutput rightOutput) rightAvailableAmount rightPtr

instance Alternative Take where
  empty =
    Take (\ _ _ failWithEOI _ _ -> failWithEOI 0)
  {-# INLINE (<|>) #-}
  (<|>) (Take left) (Take right) =
    Take $ \ availableAmount ptr failWithEOI failWithMessage succeed ->
    left availableAmount ptr 
      (\ _ -> right availableAmount ptr failWithEOI failWithMessage succeed)
      failWithMessage succeed

instance Monad Take where
  return = pure
  {-# INLINE (>>=) #-}
  (>>=) (Take left) rightK =
    Take $ \ availableAmount ptr failWithEOI failWithMessage succeed ->
    left availableAmount ptr failWithEOI failWithMessage $ \ leftOutput leftAvailableAmount leftPtr ->
    case rightK leftOutput of
      Take right ->
        right leftAvailableAmount leftPtr failWithEOI failWithMessage succeed

instance MonadPlus Take where
  mzero = empty
  mplus = (<|>)

{-# INLINE fail #-}
fail :: Text -> Take output
fail message =
  Take $ \ _ _ _ failWithMessage _ -> failWithMessage message

{-# INLINE pokeAndPeek #-}
pokeAndPeek :: A.PokeAndPeek input output -> Take output
pokeAndPeek (A.PokeAndPeek requiredAmount _ ptrIO) =
  Take $ \ !availableAmount !ptr failWithEOI failWithMessage succeed ->
  if availableAmount >= requiredAmount
    then do
      !result <- ptrIO ptr
      succeed result (availableAmount - requiredAmount) (plusPtr ptr requiredAmount)
    else failWithEOI (requiredAmount - availableAmount)

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
  Take $ \ !availableAmount !ptr failWithEOI failWithMessage succeed -> do
    bytes <- D.peekBytes ptr availableAmount
    succeed bytes 0 (plusPtr ptr availableAmount)

{-# INLINE nullTerminatedBytes #-}
nullTerminatedBytes :: Take ByteString
nullTerminatedBytes =
  {-# SCC "nullTerminatedBytes" #-}
  Take $ \ !availableAmount !ptr failWithEOI failWithMessage succeed -> do
    bytes <- B.packCString (castPtr ptr)
    case succ (B.length bytes) of
      consumedAmount -> if consumedAmount <= availableAmount
        then succeed bytes (availableAmount - consumedAmount) (plusPtr ptr consumedAmount)
        else failWithEOI (consumedAmount - availableAmount)

{-# INLINE bytesWhile #-}
bytesWhile :: (Word8 -> Bool) -> Take ByteString
bytesWhile predicate =
  {-# SCC "bytesWhile" #-}
  Take $ \ !availableAmount !ptr failWithEOI failWithMessage succeed ->
  let
    iterate !availableAmount !unconsumedAmount !ptr =
      if unconsumedAmount > 0
        then do
          byte <- C.peek ptr
          if predicate byte
            then iterate availableAmount (pred unconsumedAmount) (plusPtr ptr 1)
            else do
              bytes <- B.packCStringLen (castPtr ptr, availableAmount - unconsumedAmount)
              succeed bytes unconsumedAmount ptr
        else failWithEOI 0
    in iterate availableAmount availableAmount ptr

{-# INLINE skipWhile #-}
skipWhile :: (Word8 -> Bool) -> Take ()
skipWhile predicate =
  {-# SCC "skipWhile" #-} 
  Take $ \ !availableAmount !ptr failWithEOI failWithMessage succeed ->
  let
    iterate !availableAmount !unconsumedAmount !ptr =
      if unconsumedAmount > 0
        then do
          byte <- C.peek ptr
          if predicate byte
            then iterate availableAmount (pred unconsumedAmount) (plusPtr ptr 1)
            else succeed () unconsumedAmount ptr
        else failWithEOI 0
    in iterate availableAmount availableAmount ptr

{-# INLINE foldWhile #-}
foldWhile :: (Word8 -> Bool) -> (state -> Word8 -> state) -> state -> Take state
foldWhile predicate step start =
  {-# SCC "foldWhile" #-} 
  Take $ \ !availableAmount !ptr failWithEOI failWithMessage succeed ->
  let
    iterate !state !unconsumedAmount !ptr =
      if unconsumedAmount > 0
        then do
          byte <- C.peek ptr
          if predicate byte
            then iterate (step state byte) (pred unconsumedAmount) (plusPtr ptr 1)
            else succeed state unconsumedAmount ptr
        else failWithEOI 0
    in iterate start availableAmount ptr

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
