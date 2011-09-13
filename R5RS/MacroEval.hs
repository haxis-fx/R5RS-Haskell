module R5RS.MacroEval ( evalMacro ) where

import Control.Monad
import Control.Monad.Error

import R5RS.LispTypes
import R5RS.ModifiedEnvs

matched :: [String] -> LispVal -> LispVal -> Bool
matched _ (Number n1 e1) (Number n2 e2) = n1 == n2 && e1 == e2
matched _ (Bool b1) (Bool b2) = b1 == b2
matched _ (Char c1) (Char c2) = c1 == c2
matched _ (String s1) (String s2) = s1 == s2
matched literals (Atom a1) (Atom a2) = not (a1 `elem` literals && a1 /= a2)
matched literals (Atom a1) _ = a1 `notElem` literals
matched literals (List as)
                 (List args) = length args == length as 
                            && all (\ (p, a) -> matched literals p a) (zip as args)
matched literals (EllipsisList as) 
                 (List args) = length as <= length args 
                            && all (\ (p, a) -> matched literals p a) (zip as args)
matched literals (DottedPair as left) 
                 (List args) = let las = length as
                                 in las <= length args &&
                                    all (\ (p, a) -> matched literals p a) (zip as args) && 
                                    matched literals left (List $ drop las args)
matched literals (DottedPair as left) 
                 (DottedPair args argsLeft) = let las = length as; largs = length args
                                                in las <= largs &&
                                                   all (\ (p, a) -> matched literals p a) (zip as args) &&
                                                   if las == largs then matched literals left argsLeft
                                                                   else matched literals left (DottedPair (drop las args) argsLeft)
matched literals (Vector as)
                 (Vector args) = matched literals as args
matched _ _ _ = False


bindAll c _ [] = return c
bindAll c l (b:bs) = (uncurry $ bindPatternVar c l) b >>= \closure -> bindAll closure l bs

bindPatternVar :: Env -> [String] -> LispVal -> LispVal -> IOThrowsError Env
bindPatternVar closure literals (Atom a) arg = if a `notElem` literals
                                                 then liftIO $ bindVars closure [(a, arg)]
                                                 else return closure
bindPatternVar closure 
               literals 
               (List as) 
               (List args) = bindAll closure literals (zip as args) 

bindPatternVar closure 
               literals 
               (EllipsisList as) 
               (List args) = bindAll closure literals (zip (take (length as - 1) as) args) 
                          >>= \closure -> bindPatternVar closure 
                                                         literals 
                                                         (last as) 
                                                         (let argsLeft = drop (length as - 1) args
                                                           in EllipsisVar (head argsLeft) (tail argsLeft))
bindPatternVar closure 
               literals 
               (DottedPair as left) 
               (List args) = bindAll closure literals (zip as args)
                          >>= \closure -> bindPatternVar closure 
                                            literals 
                                            left
                                            (List $ drop (length as) args)
bindPatternVar closure 
               literals 
               (DottedPair as left)
               (DottedPair args argsLeft) = let las = length as; largs = length args
                                              in bindAll closure literals (zip as args) >>= \closure ->
                                                 if las == largs 
                                                   then bindPatternVar closure literals left argsLeft
                                                   else bindPatternVar closure literals left (DottedPair (drop las args) argsLeft)
bindPatternVar closure 
               literals
               (Vector as)
               (Vector args) = bindPatternVar closure literals as args
bindPatternVar closure _ _ _ = return closure


expandEllipsisVarList :: Env -> [LispVal] -> [LispVal] -> IOThrowsError [LispVal]
expandEllipsisVarList _ heads [] = return heads
expandEllipsisVarList closure heads (val@(EllipsisVar _ _):as) = do List r <- expandEllipsisVar closure val
                                                                    expandEllipsisVarList closure (heads ++ r) as
expandEllipsisVarList closure heads (val:as) = do r <- expandEllipsisVar closure val
                                                  expandEllipsisVarList closure (heads ++ [r]) as

expandEllipsisVar :: Env -> LispVal -> IOThrowsError LispVal
expandEllipsisVar closure (List as) = expandEllipsisVarList closure [] as >>= return . List
expandEllipsisVar closure (Vector (List as)) = expandEllipsisVarList closure [] as >>= return . Vector . List
expandEllipsisVar closure (DottedPair as tmp) = expandEllipsisVarList closure [] as >>= return . flip DottedPair tmp
expandEllipsisVar closure (EllipsisVar (Atom a) _) = do v <- getVar closure a
                                                        case v of EllipsisVar h t -> return $ List (h:t)
                                                                  _ -> throwError $ Default (a ++ " is not a variable pattern-var")
expandEllipsisVar closure val@(Atom a) = do v <- getVarNeverFail closure a val 
                                            case v of EllipsisVar _ _ -> throwError $ Default (a ++ " is a variable pattern-var")
                                                      _ -> return val
expandEllipsisVar _ other = return other

makeSyntaxRule :: Env -> LispVal -> IOThrowsError LispVal
makeSyntaxRule macroEnv (List [List (_:pattern), template]) = return $ SyntaxRule (List pattern) template macroEnv
makeSyntaxRule macroEnv (List [EllipsisList (_:pattern), template]) = return $ SyntaxRule (EllipsisList pattern) template macroEnv
makeSyntaxRule macroEnv (List [Vector (List (_:pattern)), template]) = return $ SyntaxRule (Vector $ List pattern) template macroEnv
makeSyntaxRule macroEnv (List [Vector (EllipsisList (_:pattern)), template]) = return $ SyntaxRule (Vector $ EllipsisList pattern) template macroEnv
makeSyntaxRule macroEnv (List [DottedPair (_:p1) p2, template]) = return $ SyntaxRule (DottedPair p1 p2) template macroEnv
makeSyntaxRule _ badRules = throwError $ BadSpecialForm "Unrecognized syntax rules" badRules

toEllipsis :: LispVal -> IOThrowsError LispVal
toEllipsis val@(List []) = return val
toEllipsis (List args) = case last args of EllipsisVar atom _ -> do args <- mapM toEllipsis $ take (length args - 1) args
                                                                    return $ EllipsisList $ args ++ [atom]
                                           _ -> liftM List $ mapM toEllipsis args
toEllipsis (DottedPair args arg) = do args <- mapM toEllipsis args
                                      arg <- toEllipsis arg
                                      return (DottedPair args arg)
toEllipsis (Vector v) = toEllipsis v >>= return . Vector
toEllipsis (EllipsisVar _ _) = throwError $ Default "EllipsisVar not at the end of pattern"
toEllipsis other = return other

transferEllipsisPattern :: LispVal -> IOThrowsError LispVal
transferEllipsisPattern (List [pattern, template]) = do p <- toEllipsis pattern
                                                        return (List [p, template])
transferEllipsisPattern _ = throwError $ Default "transferEllipsisPattern: Not a pattern"

patternMatch :: [String] -> [LispVal] -> LispVal -> IOThrowsError LispVal
patternMatch literals 
             ((SyntaxRule pattern template closure):as) 
             args = if matched literals pattern args
                      then bindPatternVar closure literals pattern args >>= \closure ->
                           expandEllipsisVar closure template >>=
                           evalMacro closure False
                      else patternMatch literals as args
patternMatch _ _ _ = throwError $ Default "Availble pattrn not exist"


applymacro :: LispVal -> LispVal -> IOThrowsError LispVal
applymacro (Transformers literals rules) args = patternMatch literals rules args
applymacro a (DottedPair as t) = return $ DottedPair (a:as) t
applymacro a (List as) = return $ List (a:as)
applymacro a (Vector (List as)) = return $ Vector $ List (a:as)
applymacro _ _ = throwError $ Default "Unrecognized marco format"


------------
--  export
------------

evalMacro :: Env -> Bool -> LispVal -> IOThrowsError LispVal
evalMacro macroEnv _ val@(Atom id) = getVarNeverFail macroEnv id val 
evalMacro macroEnv _ (List (Atom "syntax-rules" : List params : body)) = mapM transferEllipsisPattern body 
                                                                     >>= mapM (makeSyntaxRule macroEnv)
                                                                     >>= return . Transformers (map show params)
evalMacro macroEnv topLevel (List [val@(Atom "define-syntax"), Atom var, form]) | topLevel = evalMacro macroEnv False form >>= defineVar macroEnv var >> return (Bool True)
                                                                                | otherwise = throwError $ BadSpecialForm "Not a toplevel define-syntax: " val
evalMacro macroEnv _ (List (function : args)) = do func <- evalMacro macroEnv False function
                                                   argVals <- mapM (evalMacro macroEnv False) args
                                                   applymacro func (List argVals)
evalMacro macroEnv _ (DottedPair (function : args) argTail) = do func <- evalMacro macroEnv False function
                                                                 argVals <- mapM (evalMacro macroEnv False) args
                                                                 tailVal <- evalMacro macroEnv False argTail
                                                                 applymacro func $ DottedPair argVals tailVal
evalMacro macroEnv _ (Vector (List (function : args))) = do func <- evalMacro macroEnv False function
                                                            argVals <- mapM (evalMacro macroEnv False) args
                                                            applymacro func $ Vector $ List argVals
evalMacro _ _ others = return others

