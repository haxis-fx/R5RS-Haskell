{-# LANGUAGE ExistentialQuantification, Rank2Types #-}

module R5RS.LispTypes (  
  LispNumber(..),
  LispVal(..),
  LispError(..),
  IOThrowsError,
  Env,
  Stack
) where

import Complex
import Control.Monad.Error
import IO
import Data.IORef
import Data.Ratio
import Text.ParserCombinators.Parsec 

data LispNumber = Integer Integer
                | Float Double
                | Complex (Complex Double)
                | Rational Rational
        deriving Eq

data LispVal = Atom String
             | Bool Bool
             | Number LispNumber Bool -- True: exact, False: inexact
             | Char Char
             | String String
             | List [LispVal]
             | DottedPair [LispVal] LispVal
             | Vector LispVal -- Vector List or Vector EllipsisList
             | PrimitiveFunc ([LispVal] -> IOThrowsError LispVal)
             | Func { params :: [String], vararg :: (Maybe String), 
                      bodies :: [LispVal], closure :: Env, fclosure :: Env }
             | Port Handle
             | Void
             -- for Macro transfermation
             | Transformers { ids :: [String], syntaxRules :: [LispVal] }
             | SyntaxRule { pattern :: LispVal, template :: LispVal, closure :: Env }
             | EllipsisList [LispVal]
             | EllipsisVar LispVal [LispVal]
             -- for CPS transfermation
             | PrimitiveCFunc ( LispVal -> LispVal -> [LispVal] -> IOThrowsError LispVal )
             | CFreshVar String
             | CList [LispVal] LispVal  -- for functions with continuation
             | Continuation { param :: String, bodies :: [LispVal], closure :: Env, fclosure :: Env, wind :: Stack }   {- ToDo: stack for dynamic wind -}
             | ContinuationCC LispVal -- for call/cc
             -- for quasiquote
             | CVector [LispVal] LispVal  
             | CDottedPair [LispVal] LispVal LispVal 
             | UnquoteList [LispVal]  -- for unquote-splicing


unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

instance Show LispVal where show = debugShow
 
{-- 
valShow :: LispVal -> String
valShow (Atom name) = name
valShow (Number (Integer content) _) = show content
valShow (Number (Float content) _) = show content
valShow (Number (Complex (r :+ i)) _) | i == 0 = show r
                                      | r == 0 = show i ++ "i"
                                      | otherwise = show r ++ (if i >= 0 then "+" else "") ++ show i ++ "i"
valShow (Number (Rational content) _) =
    let n = numerator content
        d = denominator content
        sign = if n == 0 then 1 else (n `div` abs n) * (d `div` abs d) -- ’x‰„•]‰¿‚Ì‚¨‰A‚Ådiv 0‚É‚È‚ç‚È‚¢A‚Ä‚©infinity‚ÆnotANumber‚Í‚»‚à‚»‚àŽg‚¦‚È‚¢B
      in case abs d of 1 -> (if sign < 0 then "-" else "") ++ show (abs n)
                       _ -> (if sign < 0 then "-" else "") ++ show (abs n) ++ "/" ++ show (abs d)
valShow (Bool True) = "#t"
valShow (Bool False) = "#f"
valShow (Char c) = [c]
valShow (String contents) = "\"" ++ contents ++ "\""  
valShow (PrimitiveFunc _) = "<primitive>"
valShow (List contents) = "(" ++ unwordsList contents ++ ")"
valShow (EllipsisList contents) = "(" ++ unwordsList contents ++ " ...)"
valShow (DottedPair head tail) = "(" ++ unwordsList head ++ " . " ++ show tail ++ ")"
valShow (Vector (List contents)) = "#(" ++ unwordsList contents ++ ")"
valShow (Func {params = args, vararg = varargs, body = _, closure = _}) = 
    "(lambda (" ++ unwords (map show args) ++ (case varargs of 
                                                    Nothing -> ""
                                                    Just arg -> " . " ++ arg) ++ ") ...)"
valShow (Port _) = "<IO port>"
valShow (Transformers _ _) = "<Transformers>"
valShow (SyntaxRule _ _ _) = "<SyntaxRule>"
valShow (EllipsisVar _ _) = "<EllipsisVar>"
valShow (Continuation _) = "<Continuation>"
valShow _ = "<Unknown type>"
--}


debugShow :: LispVal -> String
debugShow (Atom name) = name
debugShow (CFreshVar name) = name
debugShow (Number (Integer content) _) = show content
debugShow (Number (Float content) _) = show content
debugShow (Number (Complex (r :+ i)) _) | i == 0 = show r
                                        | r == 0 = show i ++ "i"
                                        | otherwise = show r ++ (if i >= 0 then "+" else "") ++ show i ++ "i"
debugShow (Number (Rational content) _) =
    let n = numerator content
        d = denominator content
        sign = if n == 0 then 1 else (n `div` abs n) * (d `div` abs d) -- ’x‰„•]‰¿‚Ì‚¨‰A‚Ådiv 0‚É‚È‚ç‚È‚¢A‚Ä‚©infinity‚ÆnotANumber‚Í‚»‚à‚»‚àŽg‚¦‚È‚¢B
      in case abs d of 1 -> (if sign < 0 then "-" else "") ++ show (abs n)
                       _ -> (if sign < 0 then "-" else "") ++ show (abs n) ++ "/" ++ show (abs d)
debugShow (Bool True) = "#t"
debugShow (Bool False) = "#f"
debugShow (Char c) = [c]
debugShow (String contents) = "\"" ++ contents ++ "\""
debugShow (List contents) = "(" ++ unwordsList contents ++ ")"
debugShow (EllipsisList contents) = "(" ++ unwordsList contents ++ " ...)"
debugShow (DottedPair head tail) = "(" ++ unwordsList head ++ " . " ++ show tail ++ ")"
debugShow (Vector (List contents)) = "#(" ++ unwordsList contents ++ ")"
debugShow (CList contents cont) = "(" ++ unwordsList contents ++ ")" ++ "[" ++ show cont ++ "]"
debugShow (Func args varargs _ _ _) = 
    "(lambda (" ++ unwords (map show args) ++ (case varargs of 
                                                    Nothing -> ""
                                                    Just arg -> " . " ++ arg) ++ ") ...)"
debugShow (Continuation arg body _ _ _) = "Cont(" ++ "(" ++ arg ++ ") . " ++ show body ++ ")"
debugShow Void = ""
  
--------------------------------------------------------------------------------
-- Error Handler
--------------------------------------------------------------------------------
data LispError = NumArgs String String
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String
               
type IOThrowsError = ErrorT LispError IO

instance Error LispError where
  noMsg = Default "An error has occurred"
  strMsg = Default
  
instance Show LispError where 
  show (UnboundVar message varname) = message ++ ": " ++ varname
  show (BadSpecialForm message form) = message ++ ": " ++ show form
  show (NotFunction message func) = message ++ ": " ++ show func
  show (NumArgs expected found) = "Expected " ++ expected 
                                  ++ " args; found " ++ found ++ " args"
  show (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
  show (Parser parseErr) = "Parse error at " ++ show parseErr
  show (Default msg) = msg
  
--------------------------------------------------------------------------------
-- Modifiable State
--------------------------------------------------------------------------------

type Env = IORef [(String, IORef LispVal)]
type Stack = IORef [LispVal]

