module Data.Conduit.Serialize
  ( sinkGet
  , conduitGet
  , scarySinkGet
  , scaryConduitGet
  , sourcePut
  , conduitPut
  , GetFailureMessage
  , GetException
  , sinkGet'
  , sinkGet''
  , conduitGet'
  , conduitGet''
  ) where

import Control.Exception (Exception, throw)
import Control.Monad ((>=>))
import Control.Monad.Catch (MonadThrow, throwM)
import Data.ByteString (ByteString)
import Data.Conduit
import Data.Conduit.Combinators (awaitNonNull, sourceLazy)
import Data.NonNull (toNullable)
import Data.Serialize
import Data.Typeable (Typeable)

type GetFailureMessage = String

sinkGet :: Monad m => Get r -> ConduitT ByteString o m (Either GetFailureMessage r)
sinkGet = go . runGetPartial
  where
    go parse = do
        chunk <- maybe mempty toNullable <$> awaitNonNull
        case parse chunk of
            Done r rest    -> Right r <$ leftover rest
            Fail msg _     -> return (Left msg)
            Partial parse' -> go parse'

conduitGet :: Monad m => Get r -> ConduitT ByteString r m (Maybe GetFailureMessage)
conduitGet get = go
  where
    go = do
        mchunk <- awaitNonNull
        case mchunk of
            Nothing -> return Nothing
            Just b -> do
                leftover (toNullable b)
                e <- sinkGet get
                case e of
                    Left msg -> return (Just msg)
                    Right r  -> yield r >> go

scarySinkGet :: Monad m => Get r -> ConduitT ByteString o m (Either GetFailureMessage r)
scarySinkGet = go mempty . runGetPartial
  where
    go acc parse = do
        mchunk <- awaitNonNull
        let (chunk, acc') = case mchunk of
                Nothing -> (mempty, acc)
                Just b -> ((toNullable b), acc <> putByteString (toNullable b))
        case parse chunk of
            Done r   rest  -> Right r <$ leftover rest
            Fail msg rest  -> Left msg <$ leftover (runPut (acc <> putByteString rest))
            Partial parse' -> go acc' parse'

scaryConduitGet :: Monad m => Get r -> ConduitT ByteString r m (Maybe GetFailureMessage)
scaryConduitGet get = go
  where
    go = do
        mchunk <- awaitNonNull
        case mchunk of
            Nothing -> return Nothing
            Just b -> do
                leftover (toNullable b)
                e <- scarySinkGet get
                case e of
                    Left msg -> return (Just msg)
                    Right r  -> yield r >> go

sourcePut :: Monad m => Put -> ConduitT i ByteString m ()
sourcePut = sourceLazy . runPutLazy

conduitPut :: Monad m => Putter a -> ConduitT a ByteString m ()
conduitPut f = awaitForever (sourcePut . f)

-- Exceptions

data GetException = GetException GetFailureMessage deriving (Show, Typeable)

instance Exception GetException

sinkGet' :: MonadThrow m => Get r -> ConduitT ByteString o m r
sinkGet' = sinkGet >=> either (throwM . GetException) return

sinkGet'' :: MonadThrow m => Get r -> ConduitT ByteString o m r
sinkGet'' = sinkGet >=> either (throw . GetException) return

conduitGet' :: MonadThrow m => Monad m => Get r -> ConduitT ByteString r m ()
conduitGet' = conduitGet >=> maybe (return ()) (throwM . GetException)

conduitGet'' :: MonadThrow m => Monad m => Get r -> ConduitT ByteString r m ()
conduitGet'' = conduitGet >=> maybe (return ()) (throw . GetException)
