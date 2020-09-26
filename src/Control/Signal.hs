{-# LANGUAGE BangPatterns #-}
module Control.Signal where

import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow
import Control.Monad.IO.Class (liftIO, MonadIO)
import GHC.Clock (getMonotonicTimeNSec)
import Control.Concurrent (threadDelay)

newtype Signal m a b = Signal {stepSignal :: a -> m (b, Signal m a b)}

instance Functor m => Functor (Signal m a) where
  fmap f (Signal step) = Signal $ fmap (\(b, cont) -> (f b, fmap f cont)) . step

instance Applicative m => Applicative (Signal m a) where
  pure a = Signal $ \_ -> pure (a, pure a)
  (Signal step1) <*> (Signal step2) = Signal $ \a -> combine <$> (step1 a) <*> (step2 a)
    where
      combine (b1, cont1) (b2, cont2) = (b1 b2, cont1 <*> cont2)

instance Monad m => Category (Signal m) where
  id = Signal $ \a -> pure (a, id)
  (Signal step2) . (Signal step1) = Signal $ \a -> do
    (b1, cont1) <- step1 a
    (b2, cont2) <- step2 b1
    pure (b2, cont2 . cont1)

instance Monad m => Arrow (Signal m) where
  arr f = Signal $ \a -> pure (f a, arr f)
  (Signal step1) *** (Signal step2) = Signal $ \(a1, a2) ->
    combine <$> step1 a1 <*> step2 a2
    where
      combine (b1, cont1) (b2, cont2) = ((b1, b2), cont1 *** cont2)
  first (Signal step) = Signal $ \(a, d) -> (\(b, cont) -> ((b, d), first cont)) <$> step a

morphSignal :: Functor m2 => (forall c. (a1 -> m1 (b1,c)) -> a2 -> m2 (b2,c)) -> Signal m1 a1 b1 -> Signal m2 a2 b2
morphSignal f (Signal step) = Signal $ fmap (\(b,cont) -> (b, morphSignal f cont)) . f step

morphContext :: Functor g => (forall x. f x -> g x) -> Signal f a b -> Signal g a b
morphContext f = morphSignal (f .)

feedback :: Functor m => s -> Signal m (a,s) (b,s) -> Signal m a b
feedback s (Signal step) = Signal $ \a -> (\((b,newS),cont) -> (b,feedback newS cont)) <$> step (a,s)

simpleFeedback :: Functor m => b -> Signal m (a,b) b -> Signal m a b
simpleFeedback initial (Signal step) = Signal $ \a -> (\(b,cont) -> (b,simpleFeedback b cont)) <$> step (a,initial)

switch :: Monad m => Signal m a (Either c b) -> (c -> Signal m a b) -> Signal m a b
switch (Signal step) nextSignal = Signal $ \a -> do
  (res, cont) <- step a
  case res of 
    Right b -> pure (b,switch cont nextSignal)
    Left c -> stepSignal (nextSignal c) a

arrM :: Functor m => (a -> m b) -> Signal m a b
arrM f = Signal $ fmap (\b -> (b, arrM f)) . f

arrM_ :: Functor m => m b -> Signal m a b
arrM_ = arrM . const

limitExecutionRate :: MonadIO m => Int -> Signal m a b -> Signal m a b
limitExecutionRate fps signal =  Signal $ \a -> do
  t <- liftIO getMonotonicTimeNSec
  d <- pure $ toEnum (1000000000 `quot` fps)
  stepSignal (makeSignal d (t-d) signal) a
  where makeSignal d t1 s = Signal $ \a -> do
          (!b, cont) <- stepSignal s a
          t2 <- liftIO getMonotonicTimeNSec
          let diff = t2 - t1
          let waitTime = if diff >= d
                then 0
                else d - diff
          liftIO $ threadDelay $ fromEnum waitTime `quot` 1000
          pure (b, makeSignal d (t2 + waitTime) cont)

reactimate :: Monad m => Signal m () (Maybe a) -> m a
reactimate signal = stepSignal signal () >>= \case
  (Nothing, cont) -> reactimate cont
  (Just a, _) -> pure a

once :: Monad m => m x -> (x -> Signal m a b) -> Signal m a b
once m f = Signal $ \a -> m >>= flip stepSignal a . f