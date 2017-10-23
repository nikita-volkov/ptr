module Ptr.Poking
where

import Ptr.Prelude
import qualified Ptr.IO as A
import qualified Ptr.Poke as C
import qualified Ptr.PokeAndPeek as D
import qualified Data.ByteString.Internal as B


{-|
Efficiently composable specification of how to populate a pointer.
-}
data Poking =
  {-|
  * Amount of bytes the encoded data will occupy.
  * Exception-free action, which populates the pointer to the encoded data.
  -}
  Poking !Int !(Ptr Word8 -> IO ())

instance Semigroup Poking where
  {-# INLINE (<>) #-}
  (<>) (Poking space1 action1) (Poking space2 action2) =
    Poking (space1 + space2) (\ptr -> action1 ptr *> action2 (plusPtr ptr space1))

instance Monoid Poking where
  {-# INLINE mempty #-}
  mempty =
    Poking 0 (const (pure ()))
  {-# INLINE mappend #-}
  mappend =
    (<>)

{-|
Same as 'mappend' and '<>',
but performs the serialization concurrently.
This comes at the cost of an overhead,
so it is only advised to use this function when the merged serializations are heavy.
-}
prependConc :: Poking -> Poking -> Poking
prependConc (Poking space1 action1) (Poking space2 action2) =
  Poking (space1 + space2) action
  where
    action ptr =
      do
        lock <- newEmptyMVar
        forkIO $ do
          action1 ptr
          putMVar lock ()
        action2 (plusPtr ptr space1)
        takeMVar lock

{-# INLINE word8 #-}
word8 :: Word8 -> Poking
word8 x =
  Poking 1 (flip A.pokeWord8 x)

{-# INLINE beWord32 #-}
beWord32 :: Word32 -> Poking
beWord32 x =
  Poking 4 (flip A.pokeBEWord32 x)

{-# INLINE beWord64 #-}
beWord64 :: Word64 -> Poking
beWord64 x =
  Poking 8 (flip A.pokeBEWord64 x)

{-# INLINE bytes #-}
bytes :: ByteString -> Poking
bytes (B.PS bytesFPtr offset length) =
  Poking length (\ptr -> withForeignPtr bytesFPtr (\bytesPtr -> B.memcpy ptr (plusPtr bytesPtr offset) length))

{-# INLINE poke #-}
poke :: C.Poke input -> input -> Poking
poke (C.Poke space poke) input =
  Poking space (\ptr -> poke ptr input)

{-# INLINE pokeAndPeek #-}
pokeAndPeek :: D.PokeAndPeek input output -> input -> Poking
pokeAndPeek (D.PokeAndPeek space poke _) input =
  Poking space (\ptr -> poke ptr input)
