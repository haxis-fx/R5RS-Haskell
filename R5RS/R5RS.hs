{-# LANGUAGE ExistentialQuantification, Rank2Types #-}

module R5RS.R5RS( runOne, runRepl ) where

import Control.Monad
import IO

import R5RS.LispTypes
import R5RS.Errors
import R5RS.ModifiedEnvs
import R5RS.SyntaxTree
import R5RS.MacroEval
import R5RS.CPSTransfer
import R5RS.Primitives

--------------------------------------------------------------------------------
-- Evaluation
--------------------------------------------------------------------------------
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalAndPrint :: Env -> Env -> String -> IO ()
evalAndPrint macroEnv env expr = evalString macroEnv env expr >>= putStrLn

evalString :: Env -> Env -> String -> IO String
evalString macroEnv env expr = do fenv <- nullEnv
                                  stack <- nullEnv
                                  runIOThrows . liftM show $ (readExpr expr)
                                      >>= evalMacro macroEnv True
                                      >>= transCPS (makeValList "v")
                                      >>= \ (_, val) -> let cont = Continuation "x" [CFreshVar "x"] env fenv stack
                                                          in eval env fenv cont $
                                                                  List [ val, cont ]

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do 
  result <- prompt
  if pred result 
     then return ()
     else action result >> until_ pred prompt action

runOne :: [String] -> IO ()
runOne args = do env <- primitiveBindings >>= flip bindVars [("__args__", List $ map String $ drop 1 args)]
                 fenv <- nullEnv
                 stack <- nullEnv
                 (runIOThrows $ liftM show $ eval env fenv (Continuation "x" [CFreshVar "x"] env fenv stack)
                                                      (List [Atom "load", 
                                                             String (args !! 0)])) >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = do macroEnv <- nullEnv
             env <- primitiveBindings
             until_ (== "quit") (readPrompt "Lisp>>> ") (evalAndPrint macroEnv env)

