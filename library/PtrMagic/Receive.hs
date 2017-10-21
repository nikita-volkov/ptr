module PtrMagic.Receive
(
  Receive,
  create,
  decode,
)
where

import PtrMagic.Prelude
import qualified PtrMagic.Receive.Core as A
import qualified PtrMagic.Decode as B


{-|
A wrapper of a receiving action, which extends it with bufferization.
-}
data Receive =
  {-|
  * Exception-free action to receive another chunk of bytes. E.g., an exception-free wrapper of @Network.Socket.recvBuf@
  * Buffer
  * Size of the buffer
  * Chunk size
  -}
  Receive !(Ptr Word8 -> Int -> IO (Either Text Int)) !(ForeignPtr Word8) !(IORef (Int, Int)) !Int

create :: (Ptr Word8 -> Int -> IO (Either Text Int)) -> Int -> IO Receive
create fetch chunkSize =
  do
    bufferFP <- mallocForeignPtrBytes chunkSize
    bufferStateRef <- newIORef (0, 0)
    return (Receive fetch bufferFP bufferStateRef chunkSize)

{-|
Receive as many bytes as is required by the provided decoder and decode immediately.

If all you need is just to get a 'ByteString' chunk then use the 'B.bytes' decoder.
-}
decode :: Receive -> B.Decode decoded -> IO (Either Text decoded)
decode (Receive fetch bufferFP bufferStateRef chunkSize) (B.Decode amount action) =
  A.decode fetch bufferFP bufferStateRef chunkSize amount action
