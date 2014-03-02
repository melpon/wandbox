module Cache
  ( Cache
  , newCache
  , cacheWith
  ) where

-- cache on memory with expiration

import Import
import qualified Data.IORef                             as IORef
import qualified Data.Map                               as Map
import qualified Control.Concurrent                     as Concurrent
import qualified Control.Monad                          as Monad
import qualified Control.Monad.IO.Class                 as MonadIO

data Cache k a = Cache (IORef.IORef (Map.Map k a))

newCache :: IO (Cache k a)
newCache = Cache <$> IORef.newIORef Map.empty

cacheWith :: Ord k => Maybe Int -> k -> IO a -> Cache k a -> IO a
cacheWith mSeconds key value c@(Cache ref) = do
    cm <- MonadIO.liftIO $ IORef.readIORef ref
    case Map.lookup key cm of
        (Just value') -> return $ value'
        Nothing  -> do
            value' <- value
            _ <- MonadIO.liftIO $ IORef.atomicModifyIORef ref (\m -> (Map.insert key value' m, ()))
            case mSeconds of
                (Just seconds) -> do
                    _ <- MonadIO.liftIO $ Concurrent.forkIO $ delayedExpireLoop seconds c key value
                    return ()
                Nothing        -> return ()
            return value'

-- delayedExpire :: Ord k => Int -> Cache k a -> k -> IO ()
-- delayedExpire seconds (Cache ref) k = do
--     Concurrent.threadDelay (seconds*1000*1000)
--     _ <- IORef.atomicModifyIORef ref (\m -> (Map.delete k m, ()))
--     return ()

delayedExpireLoop :: Ord k => Int -> Cache k a -> k -> IO a -> IO ()
delayedExpireLoop seconds (Cache ref) key value =
    Monad.forever $ do
        Concurrent.threadDelay (seconds*1000*1000)
        value' <- value
        _ <- IORef.atomicModifyIORef ref (\m -> (Map.insert key value' m, ()))
        return ()
