module PtrMagic.ByteString
where

import PtrMagic.Prelude
import qualified PtrMagic.Encoding as A
import qualified Data.ByteString.Internal as B


{-# INLINE encoding #-}
encoding :: A.Encoding -> B.ByteString
encoding (A.Encoding size population) =
  B.unsafeCreate size population
