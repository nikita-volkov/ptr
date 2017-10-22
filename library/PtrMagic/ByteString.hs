module PtrMagic.ByteString
where

import PtrMagic.Prelude
import qualified PtrMagic.Poking as A
import qualified Data.ByteString.Internal as B


{-# INLINE poking #-}
poking :: A.Poking -> B.ByteString
poking (A.Poking size population) =
  B.unsafeCreate size population
