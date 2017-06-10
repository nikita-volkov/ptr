module PtrMagic.Prelude
(
  module Exports,
  forMToZero_,
  forMFromZero_,
  strictCons,
  traceEventIO,
  traceEvent,
  traceMarkerIO,
  traceMarker,
)
where


-- base-prelude
-------------------------
import BasePrelude as Exports hiding (assert, left, right, isLeft, isRight, (<>), First(..), Last(..), ProtocolError, traceEvent, traceEventIO, traceMarker, traceMarkerIO)

-- transformers
-------------------------
import Control.Monad.IO.Class as Exports
import Control.Monad.Trans.Class as Exports
import Control.Monad.Trans.Cont as Exports hiding (shift, callCC)
import Control.Monad.Trans.Except as Exports (ExceptT(ExceptT), Except, except, runExcept, runExceptT, mapExcept, mapExceptT, withExcept, withExceptT, throwE, catchE)
import Control.Monad.Trans.Maybe as Exports
import Control.Monad.Trans.Reader as Exports (Reader, runReader, mapReader, withReader, ReaderT(ReaderT), runReaderT, mapReaderT, withReaderT)
import Control.Monad.Trans.State.Strict as Exports (State, runState, evalState, execState, mapState, withState, StateT(StateT), runStateT, evalStateT, execStateT, mapStateT, withStateT)
import Control.Monad.Trans.Writer.Strict as Exports (Writer, runWriter, execWriter, mapWriter, WriterT(..), execWriterT, mapWriterT)

-- mtl
-------------------------
import Control.Monad.Cont.Class as Exports
import Control.Monad.Error.Class as Exports hiding (Error(..))
import Control.Monad.Reader.Class as Exports
import Control.Monad.State.Class as Exports
import Control.Monad.Writer.Class as Exports

-- contravariant
-------------------------
import Data.Functor.Contravariant as Exports
import Data.Functor.Contravariant.Divisible as Exports

-- invariant
-------------------------
import Data.Functor.Invariant as Exports

-- profunctors
-------------------------
import Data.Profunctor.Unsafe as Exports

-- semigroups
-------------------------
import Data.Semigroup as Exports

-- bytestring
-------------------------
import Data.ByteString as Exports (ByteString)

-- text
-------------------------
import Data.Text as Exports (Text)

-- bug
-------------------------
import Bug as Exports

-- 
-------------------------

import qualified GHC.RTS.Flags as A
import qualified BasePrelude as B


-- * Workarounds for unremoved event logging
-------------------------

{-# NOINLINE matchTraceUserEvents #-}
matchTraceUserEvents :: a -> a -> a
matchTraceUserEvents =
  case A.user (unsafeDupablePerformIO A.getTraceFlags) of
    True -> \_ x -> x
    False -> \x _ -> x

{-# NOINLINE traceEventIO #-}
!traceEventIO =
  matchTraceUserEvents (const (return ())) B.traceEventIO

{-# NOINLINE traceEvent #-}
!traceEvent =
  matchTraceUserEvents (const id) B.traceEvent

{-# NOINLINE traceMarkerIO #-}
!traceMarkerIO =
  matchTraceUserEvents (const (return ())) B.traceMarkerIO

{-# NOINLINE traceMarker #-}
!traceMarker =
  matchTraceUserEvents (const id) B.traceMarker

{-# INLINE forMToZero_ #-}
forMToZero_ :: Applicative m => Int -> (Int -> m a) -> m ()
forMToZero_ !startN f =
  ($ pred startN) $ fix $ \loop !n -> if n >= 0 then f n *> loop (pred n) else pure ()

{-# INLINE forMFromZero_ #-}
forMFromZero_ :: Applicative m => Int -> (Int -> m a) -> m ()
forMFromZero_ !endN f =
  ($ 0) $ fix $ \loop !n -> if n < endN then f n *> loop (succ n) else pure ()

{-# INLINE strictCons #-}
strictCons :: a -> [a] -> [a]
strictCons !a b =
  let !c = a : b in c
