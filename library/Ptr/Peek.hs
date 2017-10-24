module Ptr.Peek
where

import Ptr.Prelude hiding (take)
import qualified Ptr.PokeAndPeek as B
import qualified Ptr.Parse as C


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
  {-# SCC "word8" #-} 
  pokeAndPeek B.word8

{-# INLINE beWord16 #-}
beWord16 :: Peek Word16
beWord16 =
  {-# SCC "beWord16" #-} 
  pokeAndPeek B.beWord16

{-# INLINE beWord32 #-}
beWord32 :: Peek Word32
beWord32 =
  {-# SCC "beWord32" #-} 
  pokeAndPeek B.beWord32

{-# INLINE beWord64 #-}
beWord64 :: Peek Word64
beWord64 =
  {-# SCC "beWord64" #-} 
  pokeAndPeek B.beWord64

{-# INLINE bytes #-}
bytes :: Int -> Peek ByteString
bytes amount =
  {-# SCC "bytes" #-} 
  pokeAndPeek (B.bytes amount)

{-# INLINE pokeAndPeek #-}
pokeAndPeek :: B.PokeAndPeek input output -> Peek output
pokeAndPeek (B.PokeAndPeek size _ io) =
  {-# SCC "pokeAndPeek" #-} 
  Peek size io

{-|
Given the length of the data and a specification of its sequential consumption,
produces Peek, which results in Just the successfully taken value,
or Nothing, the specified length of data wasn't enough.
-}
{-# INLINE parse #-}
parse :: Int -> C.Parse a -> (Int -> a) -> (Text -> a) -> Peek a
parse amount (C.Parse parseIO) eoi error =
  {-# SCC "parse" #-} 
  Peek amount $ \ptr ->
  parseIO amount ptr (return . eoi) (return . error) (\result _ _ -> return result)

{-|
A standard idiom, where a header specifies the length of the body.

Produces Peek, which itself produces another Peek, which is the same as the result of the 'parse' function.
-}
{-# INLINE peekAmountAndParse #-}
peekAmountAndParse :: Peek Int -> C.Parse a -> (Int -> a) -> (Text -> a) -> Peek (Peek a)
peekAmountAndParse peekAmount parse_ eoi error =
  {-# SCC "peekAmountAndParse" #-} 
  flip fmap peekAmount $ \amount ->
  parse amount parse_ eoi error
