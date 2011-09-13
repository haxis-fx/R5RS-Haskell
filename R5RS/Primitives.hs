{-# LANGUAGE ExistentialQuantification, Rank2Types #-}

module R5RS.Primitives (primitiveBindings, load, apply, eval) where

import Complex
import Control.Monad
import Control.Monad.Error
import IO
import Data.Ratio

import R5RS.LispTypes
import R5RS.ModifiedEnvs
import R5RS.SyntaxTree
import R5RS.MacroEval
import R5RS.CPSTransfer

--Todo: finish the error msg

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc PrimitiveFunc) primitives
                                              ++ map (makeFunc PrimitiveCFunc) primitivesC)
    where makeFunc constructor (var, func) = (var, constructor func)

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= readExprList

primitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
primitives = [( "+", applyBinOp (binOpNum (+)) binOpNumG0 binOpNumG1 ),
              ( "-", applyBinOp (binOpNum (-)) binOpNum0E binOpNumG1 ),
              ( "*", applyBinOp (binOpNum (*)) binOpNumR0 binOpNumR1 ),
              ( "/", applyBinOp binOpDiv binOpNum0E binOpNumR1 ),
              ( "=", funcNumericOrder eqNumber ),
              ( "<", funcNumericOrder (orderNum (<)) ),
              ( ">", funcNumericOrder (orderNum (>)) ),
              ( "<=", funcNumericOrder (orderNum (<=)) ),
              ( ">=", funcNumericOrder (orderNum (>=)) ),
              ( "abs", funcAbs ),
              ( "acos", floatingNum acos ),
              ( "asin", floatingNum asin ),
              -- ( "atan", floatingNum atan ),
              ( "cos", floatingNum cos ),
              ( "sin", floatingNum sin ),
              ( "tan", floatingNum sin ),
              ( "log", floatingNum log ),
              ( "exp", floatingNum exp ),
              ( "append", funcAppend ),
              ( "begin", return . last ),
              ( "car", car ),
              ( "cdr", cdr ),
              ( "close-input-port", closePort ),
              ( "close-output-port", closePort ),
              ( "cons", cons ),
              ( "display", funcDisplay ),
              ( "newline", funcNewline ),
              ( "null?", funcNull ),
              ( "zero?", funcZero ),
              ( "open-input-file", makePort ReadMode ),
              ( "open-output-file", makePort WriteMode ),
              ( "read", readProc ),
              ( "read-all", readAll ),
              ( "read-contents", readContents ),
              ( "write", writeProc ),
              ( "quasiquote", funcQuasiquote ),
              ( "unquote-splicing", funcUnquoteSplicing )
             ]

primitivesC :: [(String, LispVal -> LispVal -> [LispVal] -> IOThrowsError LispVal)]
primitivesC = [ ( "apply", funcApply ),
                ( "call-with-current-continuation", funcCallCC ),
                ( "if", funcIf ),
                ( "dynamic-wind", funcDynamicWind )
              ]

              -- these functions or syntax are not here
              -- "define"
              -- "if"
              -- "lambda"
              -- "quote"
              -- "load"
              -- "set!"
              -- "unquote"


funcCallCC :: LispVal -> LispVal -> [LispVal] -> IOThrowsError LispVal
funcCallCC cont@(Continuation param body env fenv _)
           currentCont@(Continuation _ _ _ _ wind)
           [proc] = applyWithCont cont currentCont proc [ContinuationCC (Continuation param body env fenv wind)]

funcIf :: LispVal -> LispVal -> [LispVal] -> IOThrowsError LispVal
funcIf cont currentCont [test, p1, p2] = case test of 
                               Bool False -> applyWithCont cont currentCont p2 []
                               _ -> applyWithCont cont currentCont p1 []

funcApply :: LispVal -> LispVal -> [LispVal] -> IOThrowsError LispVal
funcApply cont currentCont (func : args) = makelist [] args >>= applyWithCont cont currentCont func 
  where makelist heads [List args] = return $ heads ++ args
        makelist heads (a:as) = makelist (heads ++ [a]) as

--------------------------------------------------------------------------------


makeDynamicWind = 
    List [Atom "lambda",
          List [Atom "before", Atom "trunk", Atom "after"],
          List [Atom "lambda",
                List [CFreshVar "v1"],
                List [List [Atom "lambda",
                            List [CFreshVar "v7"],
                            CList [CFreshVar "v7"]
                            (List [Atom "lambda",
                                   List [CFreshVar "v2"],
                                   List [List [Atom "lambda",
                                               List [CFreshVar "v6"],
                                               CList [CFreshVar "v6"]
                                               (List [Atom "lambda",
                                                      List [CFreshVar "v3"],
                                                      List [List [Atom "lambda",
                                                                  List [CFreshVar "v5"],
                                                                  CList [CFreshVar "v5"]
                                                                  (List [Atom "lambda",
                                                                         List [CFreshVar "v4"],
                                                                         List [CFreshVar "v1", CFreshVar "v3"]
                                                                        ]
                                                                      )
                                                                 ],
                                                            Atom "after"
                                                           ]
                                                     ])
                                              ],
                                         Atom "trunk"
                                        ]
                                  ])
                           ],
                      Atom "before"
                     ]
               ]
         ]
--------------------------------------------------------------------------------

funcDynamicWind :: LispVal -> LispVal -> [LispVal] -> IOThrowsError LispVal
funcDynamicWind cont currentCont@(Continuation _ _ _ _ wind)
                [before, trunk, after] = do env <- liftIO nullEnv 
                                            fenv <- liftIO nullEnv
                                            lambda <- eval env fenv currentCont makeDynamicWind
                                            push wind after
                                            push wind before
                                            applyWithCont cont currentCont lambda [before, trunk, after]


-------------------------------------------------------------------------------


{--
forEach :: LispVal -> [LispVal] -> IOThrowsError LispVal
forEach cont [func, List args] = mapM (applyWithCont cont func . replicate 1) args >> apply cont [Bool True]
--}

--

eqNumber :: LispVal -> LispVal -> IOThrowsError Bool
eqNumber (Number (Integer a1) b1) (Number (Integer a2) b2) = return $ a1 == a2
eqNumber (Number (Float a1) b1) (Number (Float a2) b2) = return $ a1 == a2
eqNumber (Number (Rational a1) b1) (Number (Rational a2) b2) = return $ a1 == a2
eqNumber (Number (Complex a1) b1) (Number (Complex a2) b2) = return $ a1 == a2
eqNumber (Number (Float a1) b1) (Number (Integer a2) b2) = return $ a1 == fromInteger a2
eqNumber (Number (Integer a1) b1) (Number (Float a2) b2) = return $ fromInteger a1 == a2
eqNumber (Number (Integer a1) b1) (Number (Rational a2) b2) = return $ (a1 % 1) == a2
eqNumber (Number (Rational a1) b1) (Number (Integer a2) b2) = return $ a1 == (a2 % 1)
eqNumber (Number (Float a1) b1) (Number (Rational a2) b2) = return $ a1 == fromRational a2
eqNumber (Number (Rational a1) b1) (Number (Float a2) b2) = return $ fromRational a1 == a2
eqNumber (Number (Integer a1) b1) (Number (Complex a2) b2) = return $ (fromInteger a1 :+ 0) == a2
eqNumber (Number (Complex a1) b1) (Number (Integer a2) b2) = return $ a1 == (fromInteger a2 :+ 0)
eqNumber (Number (Float a1) b1) (Number (Complex a2) b2) = return $ (a1 :+ 0) == a2
eqNumber (Number (Complex a1) b1) (Number (Float a2) b2) = return $ a1 == (a2 :+ 0)
eqNumber (Number (Rational a1) b1) (Number (Complex a2) b2) = return $ (fromRational a1 :+ 0) == a2
eqNumber (Number (Complex a1) b1) (Number (Rational a2) b2) = return $ a1 == (fromRational a2 :+ 0)
eqNumber (Number _ _) a2 = throwError $ TypeMismatch "Number" a2
eqNumber a1 _ = throwError $ TypeMismatch "Number" a1

orderNum :: (forall a. Ord a => a -> a -> Bool) -> LispVal -> LispVal -> IOThrowsError Bool
orderNum op (Number (Integer a1) b1) (Number (Integer a2) b2) = return (a1 `op` a2)
orderNum op (Number (Float a1) b1) (Number (Float a2) b2) = return (a1 `op` a2)
orderNum op (Number (Rational a1) b1) (Number (Rational a2) b2) = return (a1 `op` a2)
orderNum op (Number (Float a1) b1) (Number (Integer a2) b2) = return (a1 `op` fromInteger a2)
orderNum op (Number (Integer a1) b1) (Number (Float a2) b2) = return (fromInteger a1 `op` a2)
orderNum op (Number (Integer a1) b1) (Number (Rational a2) b2) = return ((a1 % 1) `op` a2)
orderNum op (Number (Rational a1) b1) (Number (Integer a2) b2) = return (a1 `op` (a2 % 1))
orderNum op (Number (Float a1) b1) (Number (Rational a2) b2) = return (a1 `op` fromRational a2)
orderNum op (Number (Rational a1) b1) (Number (Float a2) b2) = return (fromRational a1 `op` a2)
orderNum op (Number (Complex (a1r :+ a1i)) b1) (Number (Complex (a2r :+ a2i)) b2) = 
    if a1i == 0 && a2i == 0
      then return (a1r `op` a2r)
      else throwError $ Default "Order operator requires <Real Number>"
orderNum op (Number (Integer a1) b1) (Number (Complex (a2r :+ a2i)) b2) = 
    if a2i == 0 
      then return (fromInteger a1 `op` a2r)
      else throwError $ Default "Order operator requires <Real Number>"
orderNum op (Number (Complex (a1r :+ a1i)) b1) (Number (Integer a2) b2) = 
    if a1i == 0
      then return (a1r `op` fromInteger a2)
      else throwError $ Default "Order operator requires <Real Number>"
orderNum op (Number (Float a1) b1) (Number (Complex (a2r :+ a2i)) b2) = 
    if a2i == 0 
      then return (a1 `op` a2r)
      else throwError $ Default "Order operator requires <Real Number>"
orderNum op (Number (Complex (a1r :+ a1i)) b1) (Number (Float a2) b2) = 
    if a1i == 0
      then return (a1r `op` a2)
      else throwError $ Default "Order operator requires <Real Number>"
orderNum op (Number (Rational a1) b1) (Number (Complex (a2r :+ a2i)) b2) = 
    if a2i == 0 
      then return (fromRational a1 `op` a2r)
      else throwError $ Default "Order operator requires <Real Number>"
orderNum op (Number (Complex (a1r :+ a1i)) b1) (Number (Rational a2) b2) = 
    if a1i == 0
      then return (a1r `op` fromRational a2)
      else throwError $ Default "Order operator requires <Real Number>"
orderNum _ (Number _ _) a2 = throwError $ TypeMismatch "Number" a2
orderNum _ a1 _ = throwError $ TypeMismatch "Number" a1


funcNumericOrder :: (LispVal -> LispVal -> IOThrowsError Bool) -> [LispVal] -> IOThrowsError LispVal
funcNumericOrder f [a, b] = f a b >>= return . Bool
funcNumericOrder f (a0:a1:as) = do eqHead <- f a0 a1
                                   Bool eqRest <- funcNumericOrder f (a1:as)
                                   return $ Bool (eqHead && eqRest)
funcNumericOrder f badArgList = throwError $ NumArgs "1" (show $ length badArgList)




funcDisplay :: [LispVal] -> IOThrowsError LispVal
funcDisplay [String x] = liftIO (putStr x >> hFlush stdout) >> return Void
funcDisplay [x] = liftIO ((putStr $ show x) >> hFlush stdout) >> return Void

funcNewline :: [LispVal] -> IOThrowsError LispVal
funcNewline [] = liftIO (putStrLn "" >> hFlush stdout) >> return Void

funcUnquoteSplicing :: [LispVal] -> IOThrowsError LispVal
funcUnquoteSplicing [List as] = return $ UnquoteList as
funcUnquoteSplicing [expr] = throwError $ Default $ "unquote-splicing fail: " ++ show expr ++ " is not a list."

flattenSplicing :: [LispVal] -> [LispVal] -> [LispVal]
flattenSplicing heads [] = heads
flattenSplicing heads (UnquoteList a:as) = flattenSplicing (heads ++ a) as
flattenSplicing heads (other:as) = flattenSplicing (heads ++ [other]) as

funcQuasiquote :: [LispVal] -> IOThrowsError LispVal
funcQuasiquote [Vector (List args)] = return . Vector . List $ flattenSplicing [] args
funcQuasiquote [DottedPair args (UnquoteList _)] = throwError $ Default "The second member of pair cannot be an unquote-splicing expression"
funcQuasiquote [DottedPair args argt] = return $ DottedPair (flattenSplicing [] args) argt
funcQuasiquote args = return . List $ flattenSplicing [] args

car :: [LispVal] -> IOThrowsError LispVal
car [List (x : _)] = return x
car [DottedPair (x : _) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs "1" (show $ length badArgList)

cdr :: [LispVal] -> IOThrowsError LispVal
cdr [List (_ : xs)] = return $ List xs
cdr [DottedPair [_] x] = return x
cdr [DottedPair (_ : xs) x] = return $ DottedPair xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs "1" (show $ length badArgList)

cons :: [LispVal] -> IOThrowsError LispVal
cons [x, List []] = return $ List [x]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedPair xs xlast] = return $ DottedPair (x : xs) xlast
cons [x1, x2] = return $ DottedPair [x1] x2
cons badArgList = throwError $ NumArgs "2" (show $ length badArgList)

funcZero :: [LispVal] -> IOThrowsError LispVal
funcZero [Number (Integer a1) b1] = return $ Bool (a1 == 0)
funcZero [Number (Float a1) b1] = return $ Bool (a1 == 0)
funcZero [Number (Rational a1) b1] = return $ Bool (numerator a1 == 0)
funcZero [Number (Complex (r :+ i) ) b1] = return $ Bool (r == 0 && i == 0)
funcZero [a] = throwError $ TypeMismatch "Number" a
funcZero badArgList = throwError $ NumArgs "1" (show $ length badArgList)

funcAbs :: [LispVal] -> IOThrowsError LispVal
funcAbs [Number (Integer a1) b1] = return $ Number (Integer $ abs a1) b1
funcAbs [Number (Float a1) b1] = return $ Number (Float $ abs a1) b1
funcAbs [Number (Rational a1) b1] = return $ Number (Rational $ abs a1) b1
funcAbs [Number (Complex a1) b1] = return $ Number (Complex $ abs a1) b1
funcAbs [a] = throwError $ TypeMismatch "Number" a
funcAbs badArgList = throwError $ NumArgs "1" (show $ length badArgList)

floatingNum :: (forall a. Floating a => a -> a) -> [LispVal] -> IOThrowsError LispVal
floatingNum f [Number (Integer a1) b1] = return $ Number (Float $ f $ fromInteger a1) b1
floatingNum f [Number (Float a1) b1] = return $ Number (Float $ f a1) b1
floatingNum f [Number (Rational a1) b1] = return $ Number (Float $ f $ fromRational a1) b1
floatingNum f [Number (Complex a1) b1] = return $ Number (Complex $ f a1) b1
floatingNum f [a] = throwError $ TypeMismatch "Number" a
floatingNum f badArgList = throwError $ NumArgs "1" (show $ length badArgList)


binOpNum :: (forall a. Num a => a -> a -> a) -> LispVal -> LispVal -> IOThrowsError LispVal
binOpNum op (Number (Integer a1) b1) (Number (Integer a2) b2) = return $ Number (Integer (a1 `op` a2)) (b1 && b2)
binOpNum op (Number (Float a1) b1) (Number (Float a2) b2) = return $ Number (Float (a1 `op` a2)) (b1 && b2)
binOpNum op (Number (Rational a1) b1) (Number (Rational a2) b2) = return $ Number (Rational (a1 `op` a2)) (b1 && b2)
binOpNum op (Number (Complex a1) b1) (Number (Complex a2) b2) = return $ Number (Complex (a1 `op` a2)) (b1 && b2)
binOpNum op (Number (Float a1) b1) (Number (Integer a2) b2) = return $ Number (Float (a1 `op` fromInteger a2)) (b1 && b2)
binOpNum op (Number (Integer a1) b1) (Number (Float a2) b2) = return $ Number (Float (fromInteger a1 `op` a2)) (b1 && b2)
binOpNum op (Number (Integer a1) b1) (Number (Rational a2) b2) = return $ Number (Rational ((a1 % 1) `op` a2)) (b1 && b2)
binOpNum op (Number (Rational a1) b1) (Number (Integer a2) b2) = return $ Number (Rational (a1 `op` (a2 % 1))) (b1 && b2)  
binOpNum op (Number (Float a1) b1) (Number (Rational a2) b2) = return $ Number (Float (a1 `op` fromRational a2)) (b1 && b2)
binOpNum op (Number (Rational a1) b1) (Number (Float a2) b2) = return $ Number (Float (fromRational a1 `op` a2)) (b1 && b2) 
binOpNum op (Number (Integer a1) b1) (Number (Complex a2) b2) = return $ Number (Complex ((fromInteger a1 :+ 0) `op` a2)) (b1 && b2)
binOpNum op (Number (Complex a1) b1) (Number (Integer a2) b2) = return $ Number (Complex (a1 `op` (fromInteger a2 :+ 0))) (b1 && b2)
binOpNum op (Number (Float a1) b1) (Number (Complex a2) b2) = return $ Number (Complex ((a1 :+ 0) `op` a2)) (b1 && b2)
binOpNum op (Number (Complex a1) b1) (Number (Float a2) b2) = return $ Number (Complex (a1 `op` (a2 :+ 0))) (b1 && b2)
binOpNum op (Number (Rational a1) b1) (Number (Complex a2) b2) = return $ Number (Complex ((fromRational a1 :+ 0) `op` a2)) (b1 && b2)
binOpNum op (Number (Complex a1) b1) (Number (Rational a2) b2) = return $ Number (Complex (a1 `op` (fromRational a2 :+ 0))) (b1 && b2)
binOpNum _ (Number _ _) a2 = throwError $ TypeMismatch "Number" a2
binOpNum _ a1 _ = throwError $ TypeMismatch "Number" a1

binOpDiv :: LispVal -> LispVal -> IOThrowsError LispVal
binOpDiv (Number (Integer a1) b1) (Number (Integer a2) b2) = return $ Number (Rational (a1 % a2)) (b1 && b2)
binOpDiv (Number (Float a1) b1) (Number (Float a2) b2) = return $ Number (Float (a1 / a2)) (b1 && b2)
binOpDiv (Number (Rational a1) b1) (Number (Rational a2) b2) = return $ Number (Rational (a1 / a2)) (b1 && b2)
binOpDiv (Number (Complex a1) b1) (Number (Complex a2) b2) = return $ Number (Complex (a1 / a2)) (b1 && b2)
binOpDiv (Number (Float a1) b1) (Number (Integer a2) b2) = return $ Number (Float (a1 / fromInteger a2)) (b1 && b2)
binOpDiv (Number (Integer a1) b1) (Number (Float a2) b2) = return $ Number (Float (fromInteger a1 / a2)) (b1 && b2)
binOpDiv (Number (Integer a1) b1) (Number (Rational a2) b2) = return $ Number (Rational ((a1 % 1) / a2)) (b1 && b2)
binOpDiv (Number (Rational a1) b1) (Number (Integer a2) b2) = return $ Number (Rational (a1 / (a2 % 1))) (b1 && b2)  
binOpDiv (Number (Float a1) b1) (Number (Rational a2) b2) = return $ Number (Float (a1 / fromRational a2)) (b1 && b2)
binOpDiv (Number (Rational a1) b1) (Number (Float a2) b2) = return $ Number (Float (fromRational a1 / a2)) (b1 && b2) 
binOpDiv (Number (Integer a1) b1) (Number (Complex a2) b2) = return $ Number (Complex ((fromInteger a1 :+ 0) / a2)) (b1 && b2)
binOpDiv (Number (Complex a1) b1) (Number (Integer a2) b2) = return $ Number (Complex (a1 / (fromInteger a2 :+ 0))) (b1 && b2)
binOpDiv (Number (Float a1) b1) (Number (Complex a2) b2) = return $ Number (Complex ((a1 :+ 0) / a2)) (b1 && b2)
binOpDiv (Number (Complex a1) b1) (Number (Float a2) b2) = return $ Number (Complex (a1 / (a2 :+ 0))) (b1 && b2)
binOpDiv (Number (Rational a1) b1) (Number (Complex a2) b2) = return $ Number (Complex ((fromRational a1 :+ 0) / a2)) (b1 && b2)
binOpDiv (Number (Complex a1) b1) (Number (Rational a2) b2) = return $ Number (Complex (a1 / (fromRational a2 :+ 0))) (b1 && b2)
binOpDiv (Number _ _) a2 = throwError $ TypeMismatch "Number" a2
binOpDiv a1 _ = throwError $ TypeMismatch "Number" a1

applyBinOp :: (LispVal -> LispVal -> IOThrowsError LispVal) -> 
              IOThrowsError LispVal ->
              ((LispVal -> LispVal -> IOThrowsError LispVal) -> LispVal -> IOThrowsError LispVal) ->
              [LispVal] -> IOThrowsError LispVal
applyBinOp _ f0 _ [] = f0
applyBinOp op _ f1 [a] = f1 op a
applyBinOp op _ _ (a:as) = foldM op a as

binOpNum0E :: IOThrowsError LispVal
binOpNum0E = throwError $ NumArgs "1~n" "0"

binOpNumG0 :: IOThrowsError LispVal
binOpNumG0 = return $ Number (Integer 0) True

binOpNumR0 :: IOThrowsError LispVal
binOpNumR0 = return $ Number (Integer 1) True

binOpNumG1 :: (LispVal -> LispVal -> IOThrowsError LispVal) -> LispVal -> IOThrowsError LispVal
binOpNumG1 op param = binOpNumG0 >>= flip op param

binOpNumR1 :: (LispVal -> LispVal -> IOThrowsError LispVal) -> LispVal -> IOThrowsError LispVal
binOpNumR1 op param = binOpNumR0 >>= flip op param

funcNull :: [LispVal] -> IOThrowsError LispVal
funcNull [List []] = return $ Bool True
funcNull [_] = return $ Bool False
funcNull as = throwError $ NumArgs "1" (show $ length as)

funcAppend :: [LispVal] -> IOThrowsError LispVal
funcAppend [] = throwError $ NumArgs "1~n" "0"
funcAppend [val@(List _)] = return val
funcAppend ((List as):(List bs):cs)  = funcAppend (List (as ++ bs):cs)
funcAppend _ = throwError $ Default "Not a list in func : append"

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode
makePort _ _ = throwError $ Default "Not a filename"

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _ = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= readExpr
readProc _ = throwError $ Default "Not a port"

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)
writeProc _ = throwError $ Default "Not a port"

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename
readContents _ = throwError $ Default "IO func error"

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename
readAll _ = throwError $ Default "IO func error"


--------------------------------------------------------------------------------
          
         
applyWithCont :: LispVal -> LispVal -> LispVal -> [LispVal] -> IOThrowsError LispVal
applyWithCont cont currentCont (PrimitiveCFunc func) args = func cont currentCont args
applyWithCont cont currentCont func@(Func _ _ _ _ _) args = 
    do ret <- apply currentCont func args
       apply currentCont ret [cont]

applyWithCont cont currentCont (ContinuationCC realCont@(Continuation param body closure fclosure wind)) args = 
    do isEmpty <- empty wind
       if isEmpty
        then apply currentCont realCont args
        else do env <- liftIO nullEnv
                fenv <- liftIO nullEnv
                stack <- liftIO nullEnv
                lambda <- eval env fenv currentCont makeDynamicWind
                before <- first wind
                after <- second wind
                applyWithCont realCont currentCont lambda [before,
                                                           Func [] Nothing [Continuation "v1" [head args] env fenv stack]
                                                                env fenv,
                                                           after]


applyWithCont cont currentCont func args = apply currentCont func args >>= apply currentCont cont . return

apply :: LispVal -> LispVal -> [LispVal] -> IOThrowsError LispVal
apply _ (PrimitiveFunc func) args = func args
apply currentCont (Func params varargs body closure fclosure)
      args = if num params /= num args && varargs == Nothing
               then throwError $ NumArgs (show $ num params) (show $ num args)
               else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= flip evalBody fclosure
        where remainingArgs = drop (length params) args
              num = toInteger . length
              evalBody env fenv = liftM last $ mapM (eval env fenv currentCont) body 
              bindVarArgs arg env = case arg of
                                      Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
                                      Nothing -> return env
apply currentCont@(Continuation _ _ _ _ wind)
      cont@(Continuation param body closure fclosure _)
      [arg] = do wind' <- copy wind
                 (liftIO $ bindVars fclosure [(param, arg)]) >>= \fclosure -> (liftM last $ mapM (eval closure fclosure (Continuation param body closure fclosure wind')) body )
apply currentCont badFunc _ = throwError $ BadSpecialForm "Unrecognized appliable function" badFunc

makeFunc varargs env fenv params body = return $ Func (map show params) varargs body env fenv
makeNormalFunc = makeFunc Nothing
makeVarargs = makeFunc . Just . show

---------------------------
-- evaluation
---------------------------

eval :: Env -> Env -> LispVal -> LispVal -> IOThrowsError LispVal
eval _   _    _ Void = return Void
eval _   _    _ val@(Number _ _) = return val
eval _   _    _ val@(Bool _) = return val
eval _   _    _ val@(Char _) = return val
eval _   _    _ val@(String _) = return val
eval env _    _ (Atom id) = getVar env id
eval _   fenv _ (CFreshVar id) = getVar fenv id
eval _   _    _ (List [Atom "quote", val]) = return val
eval _   _    _ val@(Continuation _ _ _ _ _) = return val

eval env fenv currentCont (List [Atom "load", String filename]) = 
       do marcoEnv <- liftIO nullEnv
          fenv' <- liftIO nullEnv
          as' <- load filename >>= mapM (evalMacro marcoEnv True) >>= mapM (transCPS $ makeValList "v")
          liftM last (mapM (\ (y, x) -> eval env fenv currentCont $
                                             List [ x,
                                                    currentCont
                                                  ])
                           as')

eval env fenv _ (List (Atom "lambda" : List [CFreshVar param] : body)) 
    = do stack <- liftIO $ nullEnv
         return $ Continuation param body env fenv stack
eval env fenv _ (List (Atom "lambda" : List params : body)) = makeNormalFunc env fenv params body --need to modify
eval env fenv _ (List (Atom "lambda" : DottedPair params varargs : body)) = makeVarargs varargs env fenv params body
eval env fenv _ (List (Atom "lambda" : varargs@(Atom _) : body)) = makeVarargs varargs env fenv [] body

eval env fenv currentCont (List (function : args)) = -- (liftIO $ putStrLn $ (show function) ++ "|||" ++ (show args)) >>
                                         do func <- eval env fenv currentCont function
                                            argVals <- mapM (eval env fenv currentCont) args
                                            apply currentCont func argVals

eval env fenv currentCont (CList (function : args) conti) = -- (liftIO $ putStrLn $ (show function) ++ "|||" ++ (show args)) >>
    do func <- eval env fenv currentCont function
       argVals <- mapM (eval env fenv currentCont) args
       cont <- eval env fenv currentCont conti
       case func:argVals of
         [Atom "define", Atom var, form] -> defineVar env var form >> apply currentCont cont [Void]
         [Atom "set!", Atom var, form] -> setVar env var form >> apply currentCont cont [Void]
         _ -> applyWithCont cont currentCont func argVals

eval env fenv currentCont (CVector (function : args) conti) = -- (liftIO $ putStrLn $ (show function) ++ "|||" ++ (show args)) >>
    do func <- eval env fenv currentCont function
       argVals <- mapM (eval env fenv currentCont) args
       cont <- eval env fenv currentCont conti
       applyWithCont cont currentCont func [Vector $ List argVals]

eval env fenv currentCont (CDottedPair (function : args) argt conti) = -- (liftIO $ putStrLn $ (show function) ++ "|||" ++ (show args)) >>
    do func <- eval env fenv currentCont function
       argVals <- mapM (eval env fenv currentCont) args
       argTail <- eval env fenv currentCont argt
       cont <- eval env fenv currentCont conti
       applyWithCont cont currentCont func [DottedPair argVals argTail]

eval _ _ _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm
