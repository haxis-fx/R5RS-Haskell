{-# LANGUAGE ExistentialQuantification, Rank2Types #-}

module R5RS.CPSTransfer (transCPS, makeValList) where

import Control.Monad
import Control.Monad.Error

import R5RS.LispTypes
import R5RS.ModifiedEnvs
import R5RS.SyntaxTree

transCPSApplyQQPair :: [String] -> [String] -> LispVal -> [LispVal] -> IOThrowsError ([String], LispVal)
transCPSApplyQQPair heads vs cont [a] = 
    transCPSUnquoteExpr (tail vs)
                        (List [
                                Atom "lambda",
                                List [CFreshVar $ head vs],
                                CDottedPair (map CFreshVar heads) (CFreshVar $ head vs) cont
                              ])
                        a
transCPSApplyQQPair heads vs cont (a:as) = 
    do (vs', val) <- transCPSApplyQQPair (heads ++ [head vs]) (tail vs) cont as 
       transCPSUnquoteExpr vs'
                           (List [
                                   Atom "lambda", 
                                   List [CFreshVar $ head vs],
                                   val
                                 ])
                           a


transCPSApply :: ([String] -> LispVal -> LispVal -> IOThrowsError ([String], LispVal)) ->
                 ([LispVal] -> LispVal -> LispVal) ->
                 [String] -> [String] -> LispVal -> [LispVal] -> IOThrowsError ([String], LispVal)
transCPSApply f cons heads vs cont [a] = 
    f (tail vs)
      (List [
              Atom "lambda", 
              List [CFreshVar $ head vs],
              cons (map CFreshVar (heads ++ [head vs])) cont
            ])
      a
transCPSApply f cons heads vs cont (a:as) = 
    do (vs', val) <- transCPSApply f cons (heads ++ [head vs]) (tail vs) cont as 
       f vs'
         (List [
                 Atom "lambda", 
                 List [CFreshVar $ head vs],
                 val
               ])
         a

transCPSClosure :: [String] -> LispVal -> [LispVal] -> IOThrowsError ([String], LispVal)
transCPSClosure vs cont [a] = transCPSExpr (tail vs)
                                                 (List (Atom "lambda" :
                                                        List [CFreshVar $ head vs] :
                                                        [List [cont, CFreshVar $ head vs]]
                                                       ))
                                                 a
transCPSClosure vs cont (a:as) = do (vs', val) <- transCPSClosure (tail vs) cont as
                                    transCPSExpr vs'
                                                       (List [Atom "lambda", 
                                                              List [CFreshVar $ head vs],
                                                              val
                                                             ])
                                                       a

--------------------------------------------------------------------------------
-- Left-to-right call-by-value CPS transfermation.
--------------------------------------------------------------------------------

unzipList :: [LispVal] -> [LispVal] -> [LispVal] -> IOThrowsError ([LispVal], [LispVal])
unzipList a b [] = return (a, b)
unzipList a b (List [a0, b0]:as) = unzipList (a ++ [a0]) (b ++ [b0]) as
unzipList a b _ = throwError $ Default "Unknown bind form."


transCPSUnquoteExpr :: [String] -> LispVal -> LispVal -> IOThrowsError ([String], LispVal)
transCPSUnquoteExpr vs cont val@(Atom "quasiquote") = return (vs, List [cont, val])
transCPSUnquoteExpr vs cont (List [Atom "unquote", expr]) = transCPSExpr vs cont expr
transCPSUnquoteExpr vs cont (List exprs@[Atom "unquote-splicing", _]) = transCPSApply transCPSExpr CList [] vs cont exprs
transCPSUnquoteExpr vs cont other = return (vs, List [cont, List [Atom "quote", other]])


transCPSExpr ::  [String] -> LispVal -> LispVal -> IOThrowsError ([String], LispVal)
transCPSExpr vs cont Void = return (vs, List [cont, Void])
transCPSExpr vs cont val@(Number _ _) = return (vs, List [cont, val])
transCPSExpr vs cont val@(Bool _) = return (vs, List [cont, val])
transCPSExpr vs cont val@(Char _) = return (vs, List [cont, val])
transCPSExpr vs cont val@(String _) = return (vs, List [cont, val])
transCPSExpr vs cont val@(Atom _) = return (vs, List [cont, val])
transCPSExpr vs cont (List []) = return (vs, List [cont, List []])

transCPSExpr vs cont (List (Atom "lambda" : params : body)) 
                     = do (vs', expr) <- transCPSClosure (tail vs) (CFreshVar $ head vs) body
                          return (vs', List [cont, 
                                         List [Atom "lambda", 
                                               params,
                                               List [Atom "lambda", List [(CFreshVar $ head vs)], expr]
                                              ]
                                        ])

transCPSExpr vs cont (List (Atom "let" : List binds : body)) = 
    do (args, vals) <- unzipList [] [] binds
       transCPSExpr vs cont (List (List (Atom "lambda" : List args : body) : vals))

transCPSExpr vs cont (List (Atom "let*" : List binds : body)) = 
    do (args, vals) <- unzipList [] [] binds
       buildLet args vals body >>= transCPSExpr vs cont
    where buildLet [] [] body = return $ List (Atom "let" : List [] : body)
          buildLet [a] [b] body = return $ List (Atom "let" : List [List [a, b]] : body)
          buildLet (a:as) (b:bs) body = buildLet as bs body >>= \body -> return $ List (Atom "let" : List [List [a, b]] : [body])

transCPSExpr vs cont (List (Atom "letrec" : List binds : body)) = 
    do (args, vals) <- unzipList [] [] binds
       buildLetrec args vals body >>= transCPSExpr vs cont
    where buildLetrec [] [] body = return $ List (Atom "let" : List [] : body)
          buildLetrec as bs body = buildLetrecBody (makeValList "__letrec_proc__") as bs body >>= \body ->
                                   return $ List [Atom "let", 
                                                  List (map (\x -> List [x, Void]) as),
                                                  body
                                                 ]
          buildLetrecBody procs as bs body = let pab = zip procs (zip as bs) in
                                               return $ List (Atom "let" :
                                                              List (map (\ (p, (_, b)) -> List [Atom p, b]) pab) :
                                                              (map (\ (p, (a, _)) -> List[Atom "set!", a, Atom p]) pab ++ body)
                                                             )

transCPSExpr vs cont expr@(List [Atom "quote", _]) = return (vs, List [cont, expr])
transCPSExpr vs cont (List [Atom "quasiquote", Vector (List v)]) = transCPSApply transCPSUnquoteExpr CVector [] vs cont (Atom "quasiquote" : v)
transCPSExpr vs cont (List [Atom "quasiquote", DottedPair h t ]) = transCPSApplyQQPair [] vs cont (Atom "quasiquote" : (h ++ [t]))
transCPSExpr vs cont (List [Atom "quasiquote", List [Atom "unquote-splicing", _]]) = throwError $ Default "unquote-splicing expression cannot appear in outside of a list, a pair or a vector."
transCPSExpr vs cont (List [Atom "quasiquote", List [Atom "unquote", expr]]) = transCPSExpr vs cont expr
transCPSExpr vs cont (List [Atom "quasiquote", List v]) = transCPSApply transCPSUnquoteExpr CList [] vs cont (Atom "quasiquote" : v)
transCPSExpr vs cont (List [Atom "quasiquote", other]) = return (vs, List [cont, other])

transCPSExpr vs cont (List [Atom "define", var@(Atom _), form]) = 
    transCPSApply transCPSExpr CList [] vs cont
                  [ 
                    List [Atom "quote", Atom "define"],
                    List [Atom "quote", var],
                    form 
                  ]
transCPSExpr vs cont (List (Atom "define" : List (var@(Atom _) : params) : body)) = 
    transCPSApply transCPSExpr CList [] vs cont
                  [ 
                    List [Atom "quote", Atom "define"],
                    List [Atom "quote", var],
                    List (Atom "lambda" : List params : body)
                  ]
transCPSExpr vs cont (List (Atom "define" : DottedPair [var@(Atom _)] varargs@(Atom _) : body)) = 
    transCPSApply transCPSExpr CList [] vs cont
                  [ 
                    List [Atom "quote", Atom "define"],
                    List [Atom "quote", var],
                    List (Atom "lambda" : varargs : body)
                  ]

transCPSExpr vs cont (List [Atom "set!", var, expr]) = 
    transCPSApply transCPSExpr CList [] vs cont
                  [
                    List [Atom "quote", Atom "set!"],
                    List [Atom "quote", var], 
                    expr
                  ] 
          
transCPSExpr vs cont (List [Atom "if", cond, p1]) = transCPSExpr vs cont (List [Atom "if", cond, p1, Void])
transCPSExpr vs cont (List [Atom "if", cond, p1, p2]) = 
    transCPSApply transCPSExpr CList [] vs cont
                  [
                    Atom "if", 
                    cond, 
                    List [ Atom "lambda", List [], p1 ], 
                    List [ Atom "lambda", List [], p2 ]
                  ] 

transCPSExpr _ _ (List [Atom "unquote", _]) = throwError $ Default "CPS transfer fail: unquote outside quasiquote"
transCPSExpr _ _ (List [Atom "unquote-splicing", _]) = throwError $ Default "CPS transfer fail: unquote-splicing outside quasiquote"
transCPSExpr vs cont (List as) = transCPSApply transCPSExpr CList [] vs cont as
transCPSExpr _ _ e = throwError $ Default $ "CPS transfer fail: " ++ show e


--------------------------------------------------------------------------------
-- Export
--------------------------------------------------------------------------------
transCPS :: [String] -> LispVal -> IOThrowsError ([String], LispVal)
transCPS vs expr = do (vs', val) <- transCPSExpr (tail vs) (CFreshVar $ head vs) expr
                      return (vs', List [Atom "lambda", List [CFreshVar $ head vs], val])

makeValList pre = map (\x -> pre ++ show x) [0,1..]
