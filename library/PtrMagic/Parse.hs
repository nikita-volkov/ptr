module PtrMagic.Parse
where

import PtrMagic.Prelude
import qualified PtrMagic.Take as A


newtype Parse parsed =
  Parse (forall result. Ptr Word8 -> Int -> (Text -> Int -> IO result) -> (parsed -> Int -> IO result) -> IO result)

instance Functor Parse where
  {-# INLINE fmap #-}
  fmap fn (Parse io) =
    Parse (\ptr available fail succeed -> io ptr available fail (succeed . fn))

instance Applicative Parse where
  {-# INLINE pure #-}
  pure x =
    Parse (\_ _ _ succeed -> succeed x 0)
  {-# INLINE (<*>) #-}
  (<*>) =
    ap

instance Monad Parse where
  {-# INLINE return #-}
  return =
    pure
  {-# INLINABLE (>>=) #-}
  (>>=) (Parse io1) parse2 =
    Parse
      (\ptr available1 fail succeed ->
        io1 ptr available1 fail
          (\parsed1 consumed1 -> 
            case parse2 parsed1 of
              Parse io2 ->
                io2
                  (plusPtr ptr consumed1)
                  (available1 - consumed1)
                  (\message2 consumed2 -> fail message2 (consumed1 + consumed2))
                  (\parsed2 consumed2 -> succeed parsed2 (consumed1 + consumed2))))

{-# INLINE consume #-}
consume :: A.Take consumed -> Parse consumed
consume (A.Take amount ptrIO) =
  Parse
    (\ptr available fail succeed ->
      if available >= amount
        then ptrIO ptr >>= \consumed -> succeed consumed amount
        else fail "Not enough bytes to be consumed" amount)
