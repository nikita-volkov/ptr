{-|
Sketches of new module, which implements the poking actions.
-}
module Ptr.PokeIO
where

import Ptr.Prelude


type PokeIO =
  Ptr Word8 -> IO ()

{-# INLINE sequentially #-}
sequentially :: Int -> PokeIO -> PokeIO -> PokeIO
sequentially space1 action1 action2 ptr =
  action1 ptr *> action2 (plusPtr ptr space1)

{-# INLINE concurrently #-}
concurrently :: Int -> PokeIO -> PokeIO -> PokeIO
concurrently space1 action1 action2 ptr =
  do
    lock <- newEmptyMVar
    forkIO $ do
      action1 ptr
      putMVar lock ()
    action2 (plusPtr ptr space1)
    takeMVar lock
