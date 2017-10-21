module PtrMagic.Receive.Core
where

import PtrMagic.Prelude
import qualified Data.ByteString.Internal as A


write :: (Ptr Word8 -> Int -> IO (Either Text Int)) -> ForeignPtr Word8 -> IORef (Int, Int) -> Int -> Int -> Ptr Word8 -> IO (Either Text ())
write fetch bufferFP bufferStateRef chunkSize howMany destination =
  do
    (offset, end) <- readIORef bufferStateRef
    if end == offset
      then
        -- Buffer is empty, we need to fetch right away
        fetchMany fetch bufferFP bufferStateRef chunkSize howMany destination
      else
        -- We still have something in the buffer, so we'll read from it first
        withForeignPtr bufferFP $ \bufferPtr ->
        let
          amountInBuffer = end - offset
          in if amountInBuffer >= howMany
            then
              -- Buffer contains all we need, so we don't need to fetch at all
              do
                A.memcpy destination (plusPtr bufferPtr offset) howMany
                writeIORef bufferStateRef (offset + howMany, end)
                return (Right ())
            else
              do
                A.memcpy destination (plusPtr bufferPtr offset) amountInBuffer
                fetchMany fetch bufferFP bufferStateRef chunkSize (howMany - amountInBuffer) (plusPtr destination amountInBuffer)

fetchMany :: (Ptr Word8 -> Int -> IO (Either Text Int)) -> ForeignPtr Word8 -> IORef (Int, Int) -> Int -> Int -> Ptr Word8 -> IO (Either Text ())
fetchMany fetch bufferFP bufferStateRef chunkSize remaining destination =
  if remaining >= chunkSize
    then
      -- Circumvent the buffer and write to destination directly
      fetchingSome destination chunkSize $ \amountFetched ->
      if amountFetched == remaining
        then
          -- We've fetched all we've wanted, time to stop
          do
            writeIORef bufferStateRef (0, 0)
            return (Right ())
        else
          -- Go on and get some more
          fetchMany fetch bufferFP bufferStateRef chunkSize (remaining - amountFetched) (plusPtr destination amountFetched)
    else
      -- Write to buffer first and then stream a part of it to the destination
      withForeignPtr bufferFP $ \bufferPtr ->
      fetchingSome bufferPtr chunkSize $ \amountFetched ->
      do
        A.memcpy destination bufferPtr remaining
        writeIORef bufferStateRef (remaining, amountFetched)
        return (Right ())
  where
    fetchingSome destination amount handle =
      do
        fetchResult <- fetch destination amount
        case fetchResult of
          Left msg -> return (Left msg)
          Right amountFetched ->
            if amountFetched == 0
              then return (Left "End of input")
              else handle amountFetched

decode :: (Ptr Word8 -> Int -> IO (Either Text Int)) -> ForeignPtr Word8 -> IORef (Int, Int) -> Int -> Int -> (Ptr Word8 -> IO decoded) -> IO (Either Text decoded)
decode fetch bufferFP bufferStateRef chunkSize howMany decode =
  do
    (offset, end) <- readIORef bufferStateRef
    let
      amountInBuffer = end - offset
      in if amountInBuffer >= howMany
        then
          -- We have enough bytes in the buffer, so need not to allocate anything and can directly decode from the buffer
          withForeignPtr bufferFP $ \bufferPtr ->
          do
            decoded <- decode bufferPtr
            writeIORef bufferStateRef (offset + howMany, end)
            return (Right decoded)
        else
          -- We have to allocate a temporary space to prefetch the data into
          allocaBytes howMany $ \tmpPtr ->
          do
            writeResult <-
              if end == offset
                then
                  -- Buffer is empty, we need to fetch right away
                  fetchMany fetch bufferFP bufferStateRef chunkSize howMany tmpPtr
                else
                  -- We still have something in the buffer, so we'll read from it first
                  withForeignPtr bufferFP $ \bufferPtr ->
                  do
                    A.memcpy tmpPtr (plusPtr bufferPtr offset) amountInBuffer
                    fetchMany fetch bufferFP bufferStateRef chunkSize (howMany - amountInBuffer) (plusPtr tmpPtr amountInBuffer)
            case writeResult of
              Right () -> do
                decoded <- decode tmpPtr
                return (Right decoded)
              Left msg -> return (Left msg)
