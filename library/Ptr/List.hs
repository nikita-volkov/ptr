module Ptr.List
where

import Ptr.Prelude


{-# INLINE reverseDigits #-}
reverseDigits :: Integral a => a {-^ Radix -} -> a {-^ Number -} -> [a]
reverseDigits radix =
  let
    loop x = case divMod x radix of
      (next, digit) -> digit : if next <= 0 then [] else loop next
    in loop
