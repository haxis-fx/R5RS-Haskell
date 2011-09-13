module R5RS.ModifiedEnvs where

import Control.Monad
import Control.Monad.Error
import Data.IORef

import R5RS.LispTypes

nullEnv = newIORef []

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var  =  do env <- liftIO $ readIORef envRef
                         maybe (throwError $ UnboundVar "Getting an unbound variable: " var)
                               (liftIO . readIORef)
                               (lookup var $ env)

getVarNeverFail :: Env -> String -> LispVal -> IOThrowsError LispVal
getVarNeverFail envRef var valIfNofound =  do env <- liftIO $ readIORef envRef
                                              maybe (return valIfNofound)
                                                    (liftIO . readIORef)
                                                    (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Setting an unbound variable: " var) 
                                   (liftIO . (flip writeIORef value))
                                   (lookup var env)
                             return value
                 
defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do 
    alreadyDefined <- liftIO $ isBound envRef var 
    if alreadyDefined 
       then setVar envRef var value
       else bindVar envRef var value
  where bindVar envRef var value = liftIO $ do valueRef <- newIORef value
                                               env <- readIORef envRef
                                               writeIORef envRef ((var, valueRef) : env)
                                               return value
      
bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings >>= return . reverse)
          addBinding (var, value) = do ref <- newIORef value
                                       return (var, ref)                       


-----------------------------------------------------------------------------

push :: Stack -> LispVal -> IOThrowsError LispVal
push stackRef value = liftIO $ do stack <- readIORef stackRef
                                  writeIORef stackRef (value:stack)
                                  return value

pop :: Stack -> IOThrowsError LispVal
pop stackRef = do stack <- liftIO $ readIORef stackRef
                  case stack of [] -> throwError $ Default "Pop empty stack"
                                a:as -> liftIO $ writeIORef stackRef as >> return a

first :: Stack -> IOThrowsError LispVal
first stackRef = ( liftIO $ readIORef stackRef ) >>= return . head

second :: Stack -> IOThrowsError LispVal
second stackRef = ( liftIO $ readIORef stackRef ) >>= return . head . tail

empty :: Stack -> IOThrowsError Bool
empty stackRef = (liftIO $ readIORef stackRef) >>= \x -> return $ length x <= 0 

size :: Stack -> IOThrowsError Int
size stackRef = (liftIO $ readIORef stackRef) >>= return . length

copy :: Stack -> IOThrowsError Stack
copy stackRef = liftIO (readIORef stackRef >>= newIORef)


