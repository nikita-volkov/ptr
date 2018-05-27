module Ptr.Poking
where

import Ptr.Prelude hiding (length)
import qualified Ptr.IO as A
import qualified Ptr.Poke as C
import qualified Ptr.PokeAndPeek as D
import qualified Ptr.PokeIO as E
import qualified Data.ByteString.Internal as B
import qualified Data.Vector as F


{-|
An efficiently composable unmaterialised specification of how to populate a pointer.

Once composed it can be materialized into a specific data-structure like ByteString or
to directly populate a pointer in some low-level API.
-}
data Poking =
  {-|
  * Amount of bytes the encoded data will occupy.
  * Exception-free action, which populates the pointer to the encoded data.
  -}
  Poking !Int (Ptr Word8 -> IO ())

instance Semigroup Poking where
  {-|
  When the pokings are both larger than 2048 bits,
  the serialization is performed concurrently.
  -}
  {-# INLINABLE (<>) #-}
  (<>) (Poking space1 action1) (Poking space2 action2) =
    Poking (space1 + space2) action
    where
      action =
        if space1 < 2048 || space2 < 2048
          then E.sequentially space1 action1 action2
          else E.concurrently space1 action1 action2

instance Monoid Poking where
  {-# INLINE mempty #-}
  mempty =
    Poking 0 (const (pure ()))
  {-# INLINE mappend #-}
  mappend =
    (<>)

{-# INLINE null #-}
null :: Poking -> Bool
null =
  (== 0) . length

{-# INLINE length #-}
length :: Poking -> Int
length (Poking size _) =
  size

{-# INLINE word8 #-}
word8 :: Word8 -> Poking
word8 x =
  Poking 1 (flip A.pokeWord8 x)

{-# INLINE leWord16 #-}
leWord16 :: Word16 -> Poking
leWord16 x =
  Poking 2 (flip A.pokeLEWord16 x)

{-# INLINE leWord32 #-}
leWord32 :: Word32 -> Poking
leWord32 x =
  Poking 4 (flip A.pokeLEWord32 x)

{-# INLINE leWord64 #-}
leWord64 :: Word64 -> Poking
leWord64 x =
  Poking 8 (flip A.pokeLEWord64 x)

{-# INLINE beWord16 #-}
beWord16 :: Word16 -> Poking
beWord16 x =
  Poking 2 (flip A.pokeBEWord16 x)

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

{-# INLINABLE asciiIntegral #-}
asciiIntegral :: Integral a => a -> Poking
asciiIntegral =
  \case
    0 ->
      word8 48
    x ->
      bool ((<>) (word8 45)) id (x >= 0) $
      loop mempty $
      abs x
  where
    loop builder remainder =
      case remainder of
        0 ->
          builder
        _ ->
          case quotRem remainder 10 of
            (quot, rem) ->
              loop (word8 (48 + fromIntegral rem) <> builder) quot

{-# INLINE asciiChar #-}
asciiChar :: Char -> Poking
asciiChar =
  word8 . fromIntegral . ord

{-# INLINABLE asciiPaddedAndTrimmedIntegral #-}
asciiPaddedAndTrimmedIntegral :: Integral a => Int -> a -> Poking
asciiPaddedAndTrimmedIntegral !length !integral =
  if length > 0
    then
      if integral >= 0
        then case quotRem integral 10 of
          (quot, rem) ->
            asciiPaddedAndTrimmedIntegral (pred length) quot <>
            word8 (48 + fromIntegral rem)
        else stimes length (word8 48)
    else mempty

{-# INLINABLE asciiUtcTimeInIso8601 #-}
{-
2017-02-01T05:03:58Z
-}
asciiUtcTimeInIso8601 :: UTCTime -> Poking
asciiUtcTimeInIso8601 utcTime =
  asciiPaddedAndTrimmedIntegral 4 year <> word8 45 <> 
  asciiPaddedAndTrimmedIntegral 2 month <> word8 45 <>
  asciiPaddedAndTrimmedIntegral 2 day <>
  word8 84 <>
  asciiPaddedAndTrimmedIntegral 2 hour <> word8 58 <>
  asciiPaddedAndTrimmedIntegral 2 minute <> word8 58 <>
  asciiPaddedAndTrimmedIntegral 2 (round second) <>
  word8 90
  where
    LocalTime date (TimeOfDay hour minute second) = utcToLocalTime utc utcTime
    (year, month, day) = toGregorian date

{-# INLINE list #-}
list :: (element -> Poking) -> [element] -> Poking
list element =
  loop mempty
  where
    loop state =
      \ case
        head : tail -> loop (state <> word8 1 <> element head) tail
        _ -> state <> word8 0

{-# INLINABLE vector #-}
vector :: (element -> Poking) -> F.Vector element -> Poking
vector element vectorValue =
  Poking byteSize io
  where
    byteSize =
      foldl' step 0 vectorValue
      where
        step !byteSize elementValue =
          case element elementValue of
            Poking elementByteSize _ -> byteSize + elementByteSize
    io ptr =
      F.foldM'_ step ptr vectorValue
      where
        step ptr elementValue =
          case element elementValue of
            Poking elementByteSize elementIO -> do
              elementIO ptr
              return (plusPtr ptr elementByteSize)
