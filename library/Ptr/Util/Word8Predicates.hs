module Ptr.Util.Word8Predicates
where

import Ptr.Prelude


asciiUpperLetter :: Word8 -> Bool = \x -> x - 65 <= 25

asciiDigit :: Word8 -> Bool = \x -> x - 48 <= 9
