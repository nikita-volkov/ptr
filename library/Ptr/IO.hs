{-# LANGUAGE CPP #-}
module Ptr.IO
where

import Ptr.Prelude
import qualified Data.ByteString.Internal as A
import qualified Data.ByteString.Short.Internal as B
import qualified Ptr.UncheckedShifting as D


{-# INLINE peekStorable #-}
peekStorable :: Storable storable => Ptr Word8 -> IO storable
peekStorable =
  {-# SCC "peekStorable" #-} 
  peek . castPtr

{-# INLINE peekWord8 #-}
peekWord8 :: Ptr Word8 -> IO Word8
peekWord8 =
  {-# SCC "peekWord8" #-} 
  peekStorable

-- | Big-endian word of 2 bytes.
{-# INLINE peekBEWord16 #-}
peekBEWord16 :: Ptr Word8 -> IO Word16
#ifdef WORDS_BIGENDIAN
peekBEWord16 =
  {-# SCC "peekBEWord16" #-} 
  peekStorable
#else
peekBEWord16 =
  {-# SCC "peekBEWord16" #-} 
  fmap byteSwap16 . peekStorable
#endif

-- | Little-endian word of 2 bytes.
{-# INLINE peekLEWord16 #-}
peekLEWord16 :: Ptr Word8 -> IO Word16
#ifdef WORDS_BIGENDIAN
peekLEWord16 =
  {-# SCC "peekLEWord16" #-} 
  fmap byteSwap16 . peekStorable
#else
peekLEWord16 =
  {-# SCC "peekLEWord16" #-} 
  peekStorable
#endif

-- | Big-endian word of 4 bytes.
{-# INLINE peekBEWord32 #-}
peekBEWord32 :: Ptr Word8 -> IO Word32
#ifdef WORDS_BIGENDIAN
peekBEWord32 =
  {-# SCC "peekBEWord32" #-} 
  peekStorable
#else
peekBEWord32 =
  {-# SCC "peekBEWord32" #-} 
  fmap byteSwap32 . peekStorable
#endif

-- | Little-endian word of 4 bytes.
{-# INLINE peekLEWord32 #-}
peekLEWord32 :: Ptr Word8 -> IO Word32
#ifdef WORDS_BIGENDIAN
peekLEWord32 =
  {-# SCC "peekLEWord32" #-} 
  fmap byteSwap32 . peekStorable
#else
peekLEWord32 =
  {-# SCC "peekLEWord32" #-} 
  peekStorable
#endif

-- | Big-endian word of 8 bytes.
{-# INLINE peekBEWord64 #-}
peekBEWord64 :: Ptr Word8 -> IO Word64
#ifdef WORDS_BIGENDIAN
peekBEWord64 =
  {-# SCC "peekBEWord64" #-} 
  peekStorable
#else
peekBEWord64 =
  {-# SCC "peekBEWord64" #-} 
  fmap byteSwap64 . peekStorable
#endif

-- | Little-endian word of 8 bytes.
{-# INLINE peekLEWord64 #-}
peekLEWord64 :: Ptr Word8 -> IO Word64
#ifdef WORDS_BIGENDIAN
peekLEWord64 =
  {-# SCC "peekLEWord64" #-} 
  fmap byteSwap64 . peekStorable
#else
peekLEWord64 =
  {-# SCC "peekLEWord64" #-} 
  peekStorable
#endif

{-|
Allocate a new byte array with @memcpy@.
-}
{-# INLINE peekBytes #-}
peekBytes :: Ptr Word8 -> Int -> IO ByteString
peekBytes ptr amount =
  {-# SCC "peekBytes" #-} 
  A.create amount $ \destPtr -> A.memcpy destPtr ptr amount

{-# INLINE peekShortByteString #-}
peekShortByteString :: Ptr Word8 -> Int -> IO ShortByteString
peekShortByteString ptr amount =
  B.createFromPtr ptr amount

{-# INLINE peekNullTerminatedShortByteString #-}
peekNullTerminatedShortByteString :: Ptr Word8 -> (Int -> IO ShortByteString -> IO a) -> IO a
peekNullTerminatedShortByteString ptr cont =
  do
    !length <- fromIntegral <$> A.c_strlen (castPtr ptr)
    cont length (B.createFromPtr ptr length)

{-# INLINE pokeStorable #-}
pokeStorable :: Storable a => Ptr Word8 -> a -> IO ()
pokeStorable ptr value =
  {-# SCC "pokeStorable" #-} 
  poke (castPtr ptr) value

{-# INLINE pokeWord8 #-}
pokeWord8 :: Ptr Word8 -> Word8 -> IO ()
pokeWord8 ptr value =
  {-# SCC "pokeWord8" #-} 
  poke ptr value

{-# INLINE pokeWord8Off #-}
pokeWord8Off :: Ptr Word8 -> Int -> Word8 -> IO ()
pokeWord8Off ptr off value =
  {-# SCC "pokeWord8Off" #-} 
  pokeByteOff ptr off value

{-# INLINE pokeBEWord16 #-}
pokeBEWord16 :: Ptr Word8 -> Word16 -> IO ()
#ifdef WORDS_BIGENDIAN
pokeBEWord16 =
  {-# SCC "pokeBEWord16" #-} 
  pokeStorable
#else
pokeBEWord16 ptr value =
  {-# SCC "pokeBEWord16" #-} 
  do
    pokeStorable ptr (fromIntegral (D.shiftr_w16 value 8) :: Word8)
    pokeByteOff ptr 1 (fromIntegral value :: Word8)
#endif

{-# INLINE pokeBEWord32 #-}
pokeBEWord32 :: Ptr Word8 -> Word32 -> IO ()
#ifdef WORDS_BIGENDIAN
pokeBEWord32 =
  {-# SCC "pokeBEWord32" #-} 
  pokeStorable
#else
pokeBEWord32 ptr value =
  {-# SCC "pokeBEWord32" #-} 
  do
    pokeStorable ptr (fromIntegral (D.shiftr_w32 value 24) :: Word8)
    pokeByteOff ptr 1 (fromIntegral (D.shiftr_w32 value 16) :: Word8)
    pokeByteOff ptr 2 (fromIntegral (D.shiftr_w32 value 8) :: Word8)
    pokeByteOff ptr 3 (fromIntegral value :: Word8)
#endif

{-# INLINE pokeBEWord64 #-}
pokeBEWord64 :: Ptr Word8 -> Word64 -> IO ()
#ifdef WORDS_BIGENDIAN
pokeBEWord64 =
  {-# SCC "pokeBEWord64" #-} 
  pokeStorable
#else
#if WORD_SIZE_IN_BITS < 64
--
-- To avoid expensive 64 bit shifts on 32 bit machines, we cast to
-- Word32, and write that
--
pokeBEWord64 ptr value =
  {-# SCC "pokeBEWord64" #-} 
  do
    pokeBEWord32 ptr (fromIntegral (D.shiftr_w64 value 32))
    pokeBEWord32 (plusPtr ptr 4) (fromIntegral value)
#else
pokeBEWord64 ptr value =
  {-# SCC "pokeBEWord64" #-} 
  do
    pokeStorable ptr (fromIntegral (D.shiftr_w64 value 56) :: Word8)
    pokeByteOff ptr 1 (fromIntegral (D.shiftr_w64 value 48) :: Word8)
    pokeByteOff ptr 2 (fromIntegral (D.shiftr_w64 value 40) :: Word8)
    pokeByteOff ptr 3 (fromIntegral (D.shiftr_w64 value 32) :: Word8)
    pokeByteOff ptr 4 (fromIntegral (D.shiftr_w64 value 24) :: Word8)
    pokeByteOff ptr 5 (fromIntegral (D.shiftr_w64 value 16) :: Word8)
    pokeByteOff ptr 6 (fromIntegral (D.shiftr_w64 value  8) :: Word8)
    pokeByteOff ptr 7 (fromIntegral value :: Word8)
#endif
#endif

{-# INLINE pokeLEWord16 #-}
pokeLEWord16 :: Ptr Word8 -> Word16 -> IO ()
#ifdef WORDS_BIGENDIAN
pokeLEWord16 p w =
  {-# SCC "pokeLEWord16" #-} 
  do
    pokeWord8 p (fromIntegral w)
    pokeWord8Off p 1 (fromIntegral (D.shiftr_w16 w 8))
#else
pokeLEWord16 =
  {-# SCC "pokeLEWord16" #-} 
  pokeStorable
#endif

{-# INLINE pokeLEWord32 #-}
pokeLEWord32 :: Ptr Word8 -> Word32 -> IO ()
#ifdef WORDS_BIGENDIAN
pokeLEWord32 p w =
  {-# SCC "pokeLEWord32" #-}
  do
    pokeWord8 p (fromIntegral w)
    pokeWord8Off p 1 (fromIntegral (D.shiftr_w32 w 8))
    pokeWord8Off p 2 (fromIntegral (D.shiftr_w32 w 16))
    pokeWord8Off p 3 (fromIntegral (D.shiftr_w32 w 24))
#else
pokeLEWord32 =
  {-# SCC "pokeLEWord32" #-} 
  pokeStorable
#endif

{-# INLINE pokeLEWord64 #-}
pokeLEWord64 :: Ptr Word8 -> Word64 -> IO ()
#ifdef WORDS_BIGENDIAN
#if WORD_SIZE_IN_BITS < 64
--
-- To avoid expensive 64 bit shifts on 32 bit machines, we cast to
-- Word32, and write that
--
pokeLEWord64 p w =
  {-# SCC "pokeLEWord64" #-} 
  do
    let b = fromIntegral (D.shiftr_w64 w 32) :: Word32
        a = fromIntegral w                   :: Word32
    pokeWord8 p (fromIntegral a)
    pokeWord8Off p 1 (fromIntegral (D.shiftr_w32 a 8))
    pokeWord8Off p 2 (fromIntegral (D.shiftr_w32 a 16))
    pokeWord8Off p 3 (fromIntegral (D.shiftr_w32 a 24))
    pokeWord8Off p 4 (fromIntegral b)
    pokeWord8Off p 5 (fromIntegral (D.shiftr_w32 b 8))
    pokeWord8Off p 6 (fromIntegral (D.shiftr_w32 b 16))
    pokeWord8Off p 7 (fromIntegral (D.shiftr_w32 b 24))
#else
pokeLEWord64 p w =
  {-# SCC "pokeLEWord64" #-} 
  do
    pokeWord8 p (fromIntegral w)
    pokeWord8Off p 1 (fromIntegral (D.shiftr_w64 w 8))
    pokeWord8Off p 2 (fromIntegral (D.shiftr_w64 w 16))
    pokeWord8Off p 3 (fromIntegral (D.shiftr_w64 w 24))
    pokeWord8Off p 4 (fromIntegral (D.shiftr_w64 w 32))
    pokeWord8Off p 5 (fromIntegral (D.shiftr_w64 w 40))
    pokeWord8Off p 6 (fromIntegral (D.shiftr_w64 w 48))
    pokeWord8Off p 7 (fromIntegral (D.shiftr_w64 w 56))
#endif
#else
pokeLEWord64 =
  {-# SCC "pokeLEWord64" #-} 
  pokeStorable
#endif

{-# INLINE pokeBytesTrimming #-}
pokeBytesTrimming :: Ptr Word8 -> Int -> ByteString -> IO ()
pokeBytesTrimming ptr maxLength (A.PS fptr offset length) =
  {-# SCC "pokeBytesTrimming" #-} 
  withForeignPtr fptr $ \bytesPtr -> A.memcpy ptr (plusPtr bytesPtr offset) (min length maxLength)

{-# INLINE pokeBytes #-}
pokeBytes :: Ptr Word8 -> ByteString -> IO ()
pokeBytes ptr (A.PS fptr offset length) =
  {-# SCC "pokeBytes" #-} 
  withForeignPtr fptr $ \bytesPtr -> A.memcpy ptr (plusPtr bytesPtr offset) length
