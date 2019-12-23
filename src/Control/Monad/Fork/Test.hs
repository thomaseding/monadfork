{-# LANGUAGE Safe #-}

module Control.Monad.Fork.Test where

import qualified Control.Concurrent as IO
import qualified Control.Concurrent.Event as Event
import qualified Control.Exception as Exn
import qualified Control.Monad.Except as M
import qualified Control.Monad.Fork as M
import qualified Control.Monad.Identity as M
import qualified Control.Monad.Reader as M
import qualified Control.Monad.State.Lazy as ML
import qualified Control.Monad.State.Strict as MS
import qualified Control.Monad.Writer.Lazy as ML
import qualified Control.Monad.Writer.Strict as MS
import           Data.Functor ( (<&>) )
import qualified Data.IORef as IO

doPrint = mapM_ print [1..10]

force :: Functor f => f (Either Exn.SomeException a) -> f a
force = fmap $ either (error . show) id

fork1 :: IO ()
fork1 = do
  M.void $ M.fork $ \_tid -> do
    doPrint

fork2 :: IO ()
fork2 = do
  M.void $ M.fork $ \_tid -> do
    doPrint
  doPrint

fork3 :: IO ()
fork3 = do
  tid <- M.fork $ \_tid -> pure ()
  print tid
  IO.threadDelay $ 1000 * 1000 * 4
  print tid

fork4 :: IO ()
fork4 = do
  tid <- M.fork IO.killThread
  print tid

fork5 :: IO ()
fork5 = do
  tid <- M.fork $ \tid -> do
    print "Good"
    IO.killThread tid
    print "Bad"
  IO.threadDelay $ 1000 * 1000 * 4
  print tid

fork6 :: IO ()
fork6 = do
  M.void $ M.runIdentityT $ do
    M.fork $ \_tid -> do
      M.lift doPrint

fork7 :: IO ()
fork7 = do
  M.void $ M.runIdentityT $ do
    M.fork $ \_tid -> do
      M.lift doPrint

--------------------------------------------------------------------------------

forkJoin1 :: IO ()
forkJoin1 = do
  force $ M.forkJoin $ \_tid -> do
    doPrint

forkJoin2 :: IO ()
forkJoin2 = do
  force $ M.forkJoin $ \_tid -> do
    doPrint
  doPrint

