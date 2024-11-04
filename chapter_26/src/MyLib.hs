{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE UndecidableInstances #-}
module MyLib where
import           Control.Monad              (guard, liftM)
import           Control.Monad.IO.Class     (MonadIO (liftIO))
import           Control.Monad.Trans.Class  (MonadTrans (lift))
import           Control.Monad.Trans.Maybe  (MaybeT (runMaybeT))
import           Control.Monad.Trans.Reader (Reader,
                                             ReaderT (ReaderT, runReaderT),
                                             runReader, runReaderT)
import           Data.Functor.Identity      (Identity)

newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap :: Functor m => (a -> b) -> EitherT e m a -> EitherT e m b
  fmap f (EitherT ema) = EitherT $ (fmap . fmap) f ema

instance Applicative m => Applicative (EitherT e m) where
  pure :: Applicative m => a -> EitherT e m a
  pure x = EitherT (pure (pure x))

  (<*>) :: Applicative m =>
    EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
  (EitherT emab ) <*> (EitherT ema) = EitherT $ (<*>) <$> emab <*> ema

instance Monad m
  => Monad (EitherT e m) where
  return = pure

  (>>=) :: Monad m => EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
  (EitherT mea) >>= f = EitherT $ do
    ea <- mea
    case ea of
      Left e  -> return (Left e)
      Right a -> runEitherT (f a)


swapEitherT :: (Functor m)
            => EitherT e m a
            -> EitherT a m e
swapEitherT (EitherT ema) = EitherT $ swapEither <$> ema

swapEither :: Either a e -> Either e a
swapEither (Left a)  = Right a
swapEither (Right e) = Left e


eitherT :: Monad m =>
           (a -> m c)
        -> (b -> m c)
        -> EitherT a m b
        -> m c
eitherT f g (EitherT amb)= do
  ab <- amb
  case ab of
    Left a  -> f a
    Right b -> g b


newtype MyReaderT r m a =
  MyReaderT { runMyReaderT :: r -> m a }

instance (Functor m) => Functor (MyReaderT r m) where
  fmap :: Functor m => (a -> b) -> MyReaderT r m a -> MyReaderT r m b
  fmap f (MyReaderT rma) = MyReaderT $ (fmap. fmap) f rma

instance (Applicative m) => Applicative (MyReaderT r m) where
  pure :: Applicative m => a -> MyReaderT r m a
  pure x = MyReaderT (pure (pure x))

  (<*>) :: Applicative m =>
    MyReaderT r m (a -> b) -> MyReaderT r m a -> MyReaderT r m b
  (MyReaderT rmab) <*> (MyReaderT rma) = MyReaderT $ (<*>) <$> rmab <*> rma

instance (Monad m) => Monad (MyReaderT r m) where
  (>>=) :: Monad m => MyReaderT r m a -> (a -> MyReaderT r m b) -> MyReaderT r m b
  (MyReaderT rma) >>= f
    = MyReaderT $ \r -> do
         a <- rma r
         runMyReaderT (f a) r


newtype StateT s m a =
  StateT { runStateT :: s -> m (a , s) }

instance (Functor m) => Functor (StateT s m) where
  fmap :: Functor m => (a -> b) -> StateT s m a -> StateT s m b
  fmap f (StateT sma) = StateT $ \s ->
                                   let ma = sma s
                                    in fmap applier ma
                                    where applier (a, s) = (f a, s)

instance (Monad m) => Applicative (StateT s m) where
  pure :: Monad m => a -> StateT s m a
  pure x = StateT $ \s -> return (x, s)

  (<*>) :: Monad m => StateT s m (a -> b) -> StateT s m a -> StateT s m b
  (StateT smab) <*> (StateT sma) = StateT $ \s -> do
                                                (f, s') <- smab s
                                                (a, s'') <- sma s'
                                                return (f a, s'')


instance (Monad m) => Monad (StateT s m ) where
  (>>=) :: Monad m => StateT s m a -> (a -> StateT s m b) -> StateT s m b
  (StateT sma) >>= f = StateT $ \s -> do
                              (a, s') <- sma s
                              runStateT (f a) s'

instance MonadTrans (EitherT e) where
  lift = EitherT . liftM Right

instance MonadTrans (StateT s) where
  lift :: Monad m => m a -> StateT s m a
  lift m = StateT $ \s ->  do
                    a <- m
                    return (a , s)


newtype MyMaybeT m a = MyMaybeT { runMyMaybeT :: m (Maybe a) }

instance (MonadIO m)
  => MonadIO (StateT s m) where
  liftIO = lift . liftIO


rDec :: Num a => Reader a a
rDec = ReaderT $ return . subtract 1

result1 = fmap (runReader rDec) [1..10]


rShow :: Show a
      => ReaderT a Identity String
rShow = ReaderT $ \r -> return $ show r

result2 = runReaderT rShow 1


rShow' :: Show a
      => ReaderT a Identity String
rShow' = ReaderT $ return . show

rPrintAndInc :: (Num a, Show a)
             => ReaderT a IO a
rPrintAndInc = ReaderT $ \r -> do
                 print $ "Hi: " ++ show r
                 return (r + 1)

result3 = runReaderT rPrintAndInc 1

sPrintIncAccum :: (Num a, Show a)
               => StateT a IO String
sPrintIncAccum = StateT $ \s -> do
                 print $ "Hi: " ++ show s
                 return (show s, s + 1)


result4 = runStateT sPrintIncAccum 10

isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = do
  v <- lift getLine
  guard $ isValid v
  return v

doExcite :: IO ()
doExcite = do
  putStrLn "say something excite!"
  excite <- runMaybeT maybeExcite

  case excite of
    Nothing -> putStrLn "MORE EXCITE"
    Just e ->
      putStrLn
       ("Good, was very excite: " ++ e)
