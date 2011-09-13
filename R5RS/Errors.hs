module R5RS.Errors where

import Control.Monad.Error
import R5RS.LispTypes

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action (return . show)

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue
  where extractValue :: Either LispError a -> a
        extractValue (Right val) = val
        extractValue (Left _) = error "extractValue: Error"