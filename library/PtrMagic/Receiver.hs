module PtrMagic.Receiver
where

import PtrMagic.Prelude
import qualified PtrMagic.Receiver.Core as A


{-|
A wrapper of a receiving action, which extends it with bufferization.
-}
data Receiver =
  {-|
  * Exception-free action to receive another chunk of bytes. E.g., an exception-free wrapper of 'Network.Socket.recvBuf'
  * Buffer
  * Size of the buffer
  * Chunk size
  -}
  Receiver !(Ptr Word8 -> Int -> IO (Either Text Int)) !(ForeignPtr Word8) !(IORef (Int, Int)) !Int

newtype Effect a =
  Effect (ReaderT Receiver (ExceptT Text IO) a)
  deriving (Functor, Applicative, Monad, MonadIO)

{-# INLINE eitherIO #-}
eitherIO :: (Receiver -> IO (Either Text a)) -> Effect a
eitherIO eitherIO =
  Effect (ReaderT (\receiver -> ExceptT (eitherIO receiver)))

write :: Int -> Ptr Word8 -> Effect ()
write howMany destination =
  eitherIO (\(Receiver fetch bufferFP bufferStateRef chunkSize) -> A.write fetch bufferFP bufferStateRef chunkSize howMany destination)

{-|
Receive the specified amount of bytes,
immediately decoding as many bytes from a pointer into some result.
-}
decode :: Int -> (Ptr Word8 -> IO decoded) -> Effect decoded
decode howMany decode =
  eitherIO (\(Receiver fetch bufferFP bufferStateRef chunkSize) -> A.decode fetch bufferFP bufferStateRef chunkSize howMany decode)
  
getBufferFilling :: Effect Int
getBufferFilling =
  eitherIO (\(Receiver _ _ bufferStateRef _) -> fmap (\(offset, end) -> Right (end - offset)) (readIORef bufferStateRef))
