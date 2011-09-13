module Main where

import System.Environment
import R5RS.R5RS

--------------------------------------------------------------------------------
--  main
--------------------------------------------------------------------------------
main :: IO ()
main = do args <- getArgs
          if null args then runRepl else runOne $ args

--------------------------------------------------------------------------------