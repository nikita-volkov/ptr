module Ptr.Util.ByteString
where

import Ptr.Prelude hiding (length)
import Data.ByteString
import Data.ByteString.Internal
import qualified StrictList
import qualified Data.ByteString as ByteString


{-|
__Warning:__

It is your responsibility to ensure that the size is correct.
-}
fromReverseStrictList :: Int -> List ByteString -> ByteString
fromReverseStrictList size chunks =
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

fromReverseStrictListWithHead :: ByteString -> Int -> List ByteString -> ByteString
fromReverseStrictListWithHead head sizeInTail tail =
  if sizeInTail == 0
    then head
    else fromReverseStrictList (sizeInTail + length head) (StrictList.Cons head tail)

fromPtr :: Int -> Ptr Word8 -> ByteString
fromPtr size src =
  unsafeCreate size (\dst -> memcpy dst src size)
