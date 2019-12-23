{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE Safe #-}

module Control.Monad.Fork (
  MonadFork(..),
  MonadForkJoin(..),
) where

import qualified Control.Concurrent as IO
import qualified Control.Concurrent.Event as Event
import qualified Control.Exception as Exn
import qualified Control.Monad.Except as M
import qualified Control.Monad.Identity as M
import qualified Control.Monad.Reader as M
import qualified Control.Monad.State.Lazy as ML
import qualified Control.Monad.State.Strict as MS
import qualified Control.Monad.Writer.Lazy as ML
import qualified Control.Monad.Writer.Strict as MS
import           Data.Functor ( (<&>) )
import qualified Data.IORef as IO

class Monad m => MonadFork m where
  fork :: (IO.ThreadId -> m ()) -> m IO.ThreadId

class Monad m => MonadForkJoin m where
  forkJoin :: (IO.ThreadId -> m a) -> m (Either Exn.SomeException a)

instance MonadFork IO where
  fork action = mdo
    thread <- IO.forkIO $ action thread
    pure thread

instance MonadForkJoin IO where
  forkJoin action = do
    eValue <- IO.newIORef $ Left $ Exn.SomeException Exn.NonTermination
    done <- Event.new

    let errorHandler e = do
          IO.writeIORef eValue $ Left e
          Event.set done

    mdo
      thread <- IO.forkIO $ do
        Exn.handle errorHandler $ do
          x <- action thread
          IO.writeIORef eValue $ Right x
          Event.set done
      pure ()

    Event.wait done
    IO.readIORef eValue

instance MonadFork m => MonadFork (M.IdentityT m) where
  fork action = M.IdentityT $ do
    fork $ M.runIdentityT . action

instance MonadForkJoin m => MonadForkJoin (M.IdentityT m) where
  forkJoin action = M.IdentityT $ do
    forkJoin $ M.runIdentityT . action

instance MonadFork m => MonadFork (M.ExceptT e m) where
  fork action = M.ExceptT $ do
    fmap Right $ fork $ \thread -> case action thread of
      M.ExceptT e -> e <&> \e' -> case e' of
        Left {} -> ()
        Right () -> ()

instance MonadForkJoin m => MonadForkJoin (M.ExceptT e m) where
  forkJoin action = M.ExceptT $ do
    eeResult <- forkJoin $ \thread -> case action thread of
      M.ExceptT m -> m
    pure $ case eeResult of
      Left e -> Right $ Left e
      Right eResult -> case eResult of
        Left e -> Left e
        Right x -> Right $ Right x

instance MonadFork m => MonadFork (ML.StateT s m) where
  fork action = ML.StateT $ \s -> do
    thread <- fork $ \thread -> case action thread of
      ML.StateT m -> fst <$> m s
    pure (thread, s)

instance MonadFork m => MonadFork (MS.StateT s m) where
  fork action = MS.StateT $ \s -> do
    thread <- fork $ \thread -> case action thread of
      MS.StateT m -> fst <$> m s
    pure (thread, s)

instance MonadForkJoin m => MonadForkJoin (ML.StateT s m) where
  forkJoin action = ML.StateT $ \s -> do
    eResult <- forkJoin $ \thread -> case action thread of
      ML.StateT m -> m s
    pure $ case eResult of
      Left e -> (Left e, s)
      Right (x, s') -> (Right x, s')

instance MonadForkJoin m => MonadForkJoin (MS.StateT s m) where
  forkJoin action = MS.StateT $ \s -> do
    eResult <- forkJoin $ \thread -> case action thread of
      MS.StateT m -> m s
    pure $ case eResult of
      Left e -> (Left e, s)
      Right (x, s') -> (Right x, s')

instance (Monoid w, MonadFork m) => MonadFork (ML.WriterT w m) where
  fork action = ML.WriterT $ do
    thread <- fork $ \thread -> case action thread of
      ML.WriterT m -> fst <$> m
    pure (thread, mempty)

instance (Monoid w, MonadFork m) => MonadFork (MS.WriterT w m) where
  fork action = MS.WriterT $ do
    thread <- fork $ \thread -> case action thread of
      MS.WriterT m -> fst <$> m
    pure (thread, mempty)

instance (Monoid w, MonadForkJoin m) => MonadForkJoin (ML.WriterT w m) where
  forkJoin action = ML.WriterT $ do
    eResult <- forkJoin $ \thread -> case action thread of
      ML.WriterT m -> m
    pure $ case eResult of
      Left e -> (Left e, mempty)
      Right (x, w) -> (Right x, w)

instance (Monoid w, MonadForkJoin m) => MonadForkJoin (MS.WriterT w m) where
  forkJoin action = MS.WriterT $ do
    eResult <- forkJoin $ \thread -> case action thread of
      MS.WriterT m -> m
    pure $ case eResult of
      Left e -> (Left e, mempty)
      Right (x, w) -> (Right x, w)

instance MonadFork m => MonadFork (M.ReaderT r m) where
  fork action = M.ReaderT $ \env -> do
    fork $ \thread -> M.runReaderT (action thread) env

instance MonadForkJoin m => MonadForkJoin (M.ReaderT r m) where
  forkJoin action = M.ReaderT $ \env -> do
    forkJoin $ \thread -> M.runReaderT (action thread) env

