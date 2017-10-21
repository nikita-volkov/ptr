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
import qualified ByteRingBuffer as A
import qualified PtrMagic.Encoding as B
import qualified PtrMagic.Decode as C


{-# INLINE push #-}
push :: A.Buffer -> B.Encoding -> IO ()
push buffer (B.Encoding amount ptrIO) =
  A.push buffer amount ptrIO

{-# INLINE pull #-}
pull :: A.Buffer -> C.Decode pulled -> (Int -> IO pulled) -> IO pulled
pull buffer (C.Decode amount ptrIO) refill =
  A.pull buffer amount ptrIO refill
