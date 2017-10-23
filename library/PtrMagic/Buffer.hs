module PtrMagic.Buffer
(
  A.Buffer,
  A.new,
  push,
  pull,
  A.getBytes,
  A.getSpace,
)
where

import PtrMagic.Prelude
import qualified Buffer as A
import qualified PtrMagic.Poking as B
import qualified PtrMagic.Peek as C


{-# INLINE push #-}
push :: A.Buffer -> B.Poking -> IO ()
push buffer (B.Poking amount ptrIO) =
  A.push buffer amount ptrIO

{-# INLINE pull #-}
pull :: A.Buffer -> C.Peek pulled -> (Int -> IO pulled) -> IO pulled
pull buffer (C.Peek amount ptrIO) refill =
  A.pull buffer amount ptrIO refill
