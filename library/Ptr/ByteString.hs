module Ptr.ByteString
where

import Ptr.Prelude
import qualified Ptr.Poking as A
import qualified Data.ByteString.Internal as B


{-# INLINE poking #-}
poking :: A.Poking -> B.ByteString
poking (A.Poking size population) =
  B.unsafeCreate size population
