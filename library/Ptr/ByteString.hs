module Ptr.ByteString
where

import Ptr.Prelude
import qualified Ptr.Poking as A
import qualified Ptr.Take as C
import qualified Ptr.Peek as D
import qualified Data.ByteString.Internal as B


{-# INLINE poking #-}
poking :: A.Poking -> B.ByteString
poking (A.Poking size population) =
  B.unsafeCreate size population

{-# INLINE take #-}
take :: B.ByteString -> C.Take result -> (Int -> result) -> (Text -> result) -> result
take (B.PS fp offset length) (C.Take takeIO) eoi error =
  {-# SCC "take" #-} 
  unsafePerformIO $
  withForeignPtr fp $ \ptr ->
  takeIO length (plusPtr ptr offset) (return . eoi) (return . error) (\result _ _ -> return result)

{-# INLINE peek #-}
peek :: B.ByteString -> D.Peek result -> Maybe result
peek (B.PS fp offset length) (D.Peek amount io) =
  {-# SCC "peek" #-} 
  if amount <= length
    then Just $ unsafePerformIO $ withForeignPtr fp $ \ptr ->
      io (plusPtr ptr offset)
    else Nothing
