module Ptr.Read
(
  Read,
  runOnByteString,
  skip,
  skipWhile,
  byteString,
  byteStringWhile,
  foldlWhile',
  word8,
  int16InBe,
  int32InBe,
  int64InBe,
  nullTerminatedByteString,
  asciiIntegral,
)
where

import Ptr.Prelude hiding (Read)
import qualified Ptr.IO as IO
import qualified Ptr.Util.Word8Predicates as Word8Predicates
import qualified Ptr.Util.ByteString as ByteString
import qualified Data.ByteString.Internal as ByteString
import qualified StrictList


{-|
Deserializer highly optimized to reading from pointers.

Parsing ByteString is just a special case.
-}
newtype Read a =
  {-|
  It's safe to assume that at least 1 byte is available.
  -}
  Read (Ptr Word8 -> Ptr Word8 -> IO (Status a))

instance Functor Read where
  fmap f (Read cont) =
    Read (\start end -> fmap (fmap f) (cont start end))

instance Applicative Read where
  pure a =
    Read (\s e -> pure (FinishedStatus s a))
  Read lGetStatus <*> Read rGetStatus =
    Read $ \start end -> do
      lGetStatus start end >>= \case
        FinishedStatus lAfter lRes ->
          if lAfter < end
            then rGetStatus lAfter end & fmap (fmap lRes)
            else return (UnfinishedStatus (fmap lRes (Read rGetStatus)))
        UnfinishedStatus lNextPeek ->
          return (UnfinishedStatus (lNextPeek <*> Read rGetStatus))

instance Monad Read where
  return = pure
  Read lGetStatus >>= k =
    Read $ \start end ->
      lGetStatus start end >>= \case
        FinishedStatus lAfter lRes ->
          if lAfter < end
            then k lRes & \(Read rGetStatus) -> rGetStatus lAfter end
            else return (UnfinishedStatus (k lRes))
        UnfinishedStatus lNextPeek ->
          return (UnfinishedStatus (lNextPeek >>= k))

{-|
Result of a single iteration.

Errors can be achieved by using Either for output.
-}
data Status a =
  FinishedStatus (Ptr Word8) a
  |
  UnfinishedStatus (Read a)
  deriving (Functor)


-- *
-------------------------

runOnByteString :: Read a -> ByteString -> Either (Read a) (a, ByteString)
runOnByteString (Read read) (ByteString.PS bsFp bsOff bsSize) =
  if bsSize == 0
    then Left (Read read)
    else
      unsafePerformIO $
      withForeignPtr bsFp $ \p ->
        let
          startP = plusPtr p bsOff
          endP = plusPtr startP bsSize
          in
            read startP endP <&> \case
              FinishedStatus newStartP res ->
                let newBsOff = minusPtr newStartP p
                    newBs = ByteString.PS bsFp newBsOff (bsSize - (newBsOff - bsOff))
                    in Right (res, newBs)
              UnfinishedStatus next ->
                Left next


-- *
-------------------------

skip ::
  {-|
  Amount of bytes to skip.
  
  __Warning:__ It is your responsibility to ensure that it is not negative.
  -}
  Int ->
  Read ()
skip =
  Read . loop
  where
    loop needed start end =
      if post <= end
        then return (FinishedStatus post ())
        else return (UnfinishedStatus (Read (loop nextNeeded)))
      where
        post = plusPtr start needed
        nextNeeded = minusPtr post end

skipWhile :: (Word8 -> Bool) -> Read ()
skipWhile predicate =
  Read loop
  where
    loop start end =
      if post <= end
        then do
          w <- IO.peekWord8 start
          if predicate w
            then loop post end
            else return (FinishedStatus start ())
        else return (UnfinishedStatus (Read loop))
      where
        post = plusPtr start 1

byteString ::
  {-|
  Size of the bytestring.

  __Warning:__ It is your responsibility to ensure that it is not negative.
  -}
  Int ->
  Read ByteString
byteString totalNeededSize =
  Read (collectChunks totalNeededSize Nil)
  where
    collectChunks neededSize chunks startPtr endPtr =
      let
        nextPtr = plusPtr startPtr neededSize
        in
          -- If there's enough
          if nextPtr <= endPtr
            then let
              lastChunkLength = minusPtr nextPtr startPtr
              !chunk = ByteString.fromPtrWithSize lastChunkLength startPtr
              merged = case chunks of
                Nil -> chunk
                _ ->
                  ByteString.fromReverseStrictListWithSize totalNeededSize
                    (Cons chunk chunks)
              in return (FinishedStatus nextPtr merged)
            else let
              lastChunkLength = minusPtr endPtr startPtr
              !chunk = ByteString.fromPtrWithSize lastChunkLength startPtr
              newNeededSize = neededSize - lastChunkLength
              newChunks = Cons chunk chunks
              loop = collectChunks newNeededSize newChunks
              in return (UnfinishedStatus (Read loop))

byteStringWhile :: (Word8 -> Bool) -> Read ByteString
byteStringWhile predicate =
  Read (collectChunks 0 Nil)
  where
    collectChunks totalLength chunks startPtr endPtr =
      populateChunk startPtr
      where
        populateChunk curPtr =
          if curPtr < endPtr
            then do
              w <- IO.peekWord8 curPtr
              if predicate w
                then populateChunk (plusPtr curPtr 1)
                else let
                  chunkLength =
                    minusPtr curPtr startPtr
                  !chunk =
                    ByteString.fromPtrWithSize chunkLength startPtr
                  merged =
                    case chunks of
                      Nil ->
                        chunk
                      _ ->
                        ByteString.fromReverseStrictListWithSize
                          (totalLength + chunkLength)
                          (Cons chunk chunks)
                  in return (FinishedStatus curPtr merged)
            else let
              chunkLength =
                minusPtr endPtr startPtr
              !chunk =
                ByteString.fromPtrWithSize chunkLength startPtr
              newTotalLength =
                totalLength + chunkLength
              newChunks =
                Cons chunk chunks
              in return (UnfinishedStatus (Read (collectChunks newTotalLength newChunks)))

foldlWhile' :: (Word8 -> Bool) -> (acc -> Word8 -> acc) -> acc -> Read acc
foldlWhile' predicate step =
  Read . loop
  where
    loop !acc start end =
      if post <= end
        then do
          w <- IO.peekWord8 start
          if predicate w
            then loop (step acc w) post end
            else return (FinishedStatus start acc)
        else return (UnfinishedStatus (Read (loop acc)))
      where
        post = plusPtr start 1


-- *
-------------------------

word8 :: Read Word8
word8 =
  Read $ \start end -> let
    post = plusPtr start 1
    in IO.peekWord8 start <&> FinishedStatus post

int16InBe :: Read Int16
int16InBe =
  with0
  where
    with0 =
      Read $ \start end -> let
        completePost = plusPtr start 2
        in if completePost <= end
          then
            IO.peekBEInt16 start <&> FinishedStatus completePost
          else
            IO.peekWord8 start <&> UnfinishedStatus . with1 . fromIntegral
    with1 !acc =
      Read $ \start end ->
        IO.peekWord8 start
          <&> \w -> FinishedStatus (plusPtr start 1) (unsafeShiftL acc 8 .|. fromIntegral w)

int32InBe :: Read Int32
int32InBe =
  Read inWhole
  where
    inWhole start end =
      if inWholePost <= end
        then
          IO.peekBEInt32 start <&> FinishedStatus inWholePost
        else
          do
            w <- IO.peekWord8 start
            bytely 3 (fromIntegral w) (plusPtr start 1) end
      where
        inWholePost = plusPtr start 4
    bytely !needed !acc start end =
      if start < end
        then
          do
            w <- IO.peekWord8 start
            let newAcc = unsafeShiftL acc 8 .|. fromIntegral w
                newStart = plusPtr start 1
                in if needed > 1
                  then bytely (pred needed) newAcc newStart end
                  else return (FinishedStatus newStart newAcc)
        else
          return (UnfinishedStatus (Read (bytely needed acc)))

int64InBe :: Read Int64
int64InBe =
  Read inWhole
  where
    inWhole start end =
      if inWholePost <= end
        then
          IO.peekBEInt64 start <&> FinishedStatus inWholePost
        else
          do
            w <- IO.peekWord8 start
            bytely 7 (fromIntegral w) (plusPtr start 1) end
      where
        inWholePost = plusPtr start 8
    bytely !needed !acc start end =
      if start < end
        then
          do
            w <- IO.peekWord8 start
            let newAcc = unsafeShiftL acc 8 .|. fromIntegral w
                newStart = plusPtr start 1
                in if needed > 1
                  then bytely (pred needed) newAcc newStart end
                  else return (FinishedStatus newStart newAcc)
        else
          return (UnfinishedStatus (Read (bytely needed acc)))

nullTerminatedByteString :: Read ByteString
nullTerminatedByteString =
  byteStringWhile (/= 0) <* skip 1

{-|
Integral number encoded in ASCII.
-}
{-# INLINE asciiIntegral #-}
asciiIntegral :: Integral a => Read a
asciiIntegral =
  foldlWhile' Word8Predicates.asciiDigit step 0
  where
    step acc byte =
      acc * 10 + fromIntegral byte - 48
