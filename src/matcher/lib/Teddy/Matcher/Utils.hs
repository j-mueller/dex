{-# LANGUAGE LambdaCase #-}
module Teddy.Matcher.Utils(
  liftResult,
  liftEither,
  mapError,
  failOnLeft,
  failOnLeftLog
) where

import           Control.Monad.Error.Class  (MonadError (..))
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.Result       (ResultT)
import qualified Control.Monad.Result       as Result
import           Control.Monad.Trans.Except (ExceptT, runExceptT)
import           Convex.MonadLog            (MonadLog, logWarnS)
import           System.Exit                (exitFailure)

liftResult :: (MonadError e m) => (String -> e) -> ResultT m a -> m a
liftResult f action = Result.runResultT action >>= either (throwError . f) pure . Result.toEither

liftEither :: (MonadError e m) => (ee -> e) -> m (Either ee a) -> m a
liftEither f action = action >>= either (throwError . f) pure

mapError :: (MonadError e m) => (ee -> e) -> ExceptT ee m a -> m a
mapError f action = runExceptT action >>= either (throwError . f) pure

failOnLeft :: MonadIO m => (e -> String) -> Either e a -> m a
failOnLeft f = \case
  Left err -> liftIO $ do
    putStrLn (f err)
    exitFailure
  Right x -> pure x

failOnLeftLog :: (MonadLog m, MonadIO m) => (e -> String) -> Either e a -> m a
failOnLeftLog f = \case
  Left err -> do
    logWarnS (f err)
    liftIO exitFailure
  Right x -> pure x
