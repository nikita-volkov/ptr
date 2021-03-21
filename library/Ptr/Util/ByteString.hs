module Ptr.Util.ByteString
where

import Ptr.Prelude
import Data.ByteString.Internal
import qualified StrictList
import qualified Data.ByteString as ByteString


{-|
__Warning:__

It is your responsibility to ensure that the size is correct.
-}
fromReverseStrictListWithSize :: Int -> List ByteString -> ByteString
fromReverseStrictListWithSize size chunks =
  unsafeCreate size (\ptr -> loop (plusPtr ptr size) chunks)
  where
    loop endPtr =
      \case
        StrictList.Cons (PS fp off len) tail ->
          do
            withForeignPtr fp $ \src -> memcpy ptr (plusPtr src off) len
            loop ptr tail
          where
            ptr = plusPtr endPtr (negate len)
        StrictList.Nil ->
          return ()

fromPtrWithSize :: Int -> Ptr Word8 -> ByteString
fromPtrWithSize size src =
  unsafeCreate size (\dst -> memcpy dst src size)
