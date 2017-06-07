module PtrMagic.Peek
(
  run,
  Peek,
  word8,
  beWord32,
  leWord32,
  bytes,
)
where

import PtrMagic.Prelude
import qualified PtrMagic.IO as A


{-# INLINE run #-}
run :: Peek peeked -> (Ptr Word8 -> IO peeked)
run (Peek ptrIO) =
  ptrIO

newtype Peek peeked =
  Peek (Ptr Word8 -> IO peeked)

deriving instance Functor Peek

instance Applicative Peek where
  {-# INLINE pure #-}
  pure x =
    Peek (const (pure x))
  {-# INLINE (<*>) #-}
  (<*>) (Peek leftIO) (Peek rightIO) =
    Peek ((<*>) <$> leftIO <*> rightIO)


{-# INLINE word8 #-}
word8 :: Peek Word8
word8 =
  Peek A.peekWord8

{-# INLINE beWord32 #-}
beWord32 :: Peek Word32
beWord32 =
  Peek A.peekBEWord32

{-# INLINE leWord32 #-}
leWord32 :: Peek Word32
leWord32 =
  Peek A.peekLEWord32

{-# INLINE bytes #-}
bytes :: Int -> Peek ByteString
bytes amount =
  Peek (A.peekBytes amount)
