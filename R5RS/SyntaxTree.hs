module R5RS.SyntaxTree (
  readExpr,
  readExprList
) where

import Char
import Complex
import Control.Monad
import Control.Monad.Error
import Data.Ratio
import Numeric

import Text.ParserCombinators.Parsec 

import R5RS.LispTypes

--------------------

readExpr = readOrThrow parseExpr'
    where parseExpr' = do e <- spaces >> parseExpr
                          spaces >> eof
                          return e

readExprList = readOrThrow parseExprs
  where parseExprs = do es <- spaces >> parseExpr `sepEndBy` spaces1
                        eof
                        return es

---------------------

empty :: GenParser tok st [a]
empty = return []

{--
zero1 :: GenParser tok st a -> GenParser tok st [a]
zero1 p = count 1 p <|> empty
--}

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces1 :: Parser ()
spaces1 = skipMany1 space

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               return $ Atom (first:rest)

parseBool :: Parser LispVal
parseBool = do try $ char '#'
               x <- oneOf "tTfF"
               return $ case toLower x of 't' -> Bool True
                                          'f' -> Bool False
                                          _ -> error "Will not come here"



--------------------------------------------------------------------------------
-- <character> -> #\ <any character>
--              | #\ <any character name>
-- <character name> -> space | newline
--------------------------------------------------------------------------------
parseChar :: Parser LispVal
parseChar = do string "#\\"
               (try (string "space" >> (return $ Char ' ')) <|>
                try (string "newline" >> (return $ Char '\n')) <|>
                (anyChar >>= return . Char))

--------------------------------------------------------------------------------
-- <string> -> " <string element>* "
-- <string element> -> <any character other than " or \> | \" | \\
--------------------------------------------------------------------------------
escapedChars :: Parser Char
escapedChars = do char '\\' 
                  x <- oneOf "\\\"nrt" 
                  return $ case x of 
                    '\\' -> x
                    '"'  -> x
                    'n'  -> '\n'
                    'r'  -> '\r'
                    't'  -> '\t'
                    _ -> error ("Not a supported backslash char: " ++ [x])

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many $ escapedChars <|> noneOf "\"\\"
                 char '"'
                 return $ String x

--------------------------------------------------------------------------------
-- <prefix R> -> <radix R> <exactness>
--             | <exactness> <radix R>
-- <exactness> -> <empty> | #i | #e
-- <radix 2> -> #b
-- <radix 8> -> #o
-- <radix 10> -> <empty> | #d
-- <radix 16> -> #x
--------------------------------------------------------------------------------
exactness :: String -> Parser String
exactness s = try (do s0 <- do e0 <- char '#'
                               e1 <- oneOf exactStr
                               return $ [e0, toLower e1]
                      s1 <- do r0 <- char '#'
                               r1 <- oneOf s
                               return $ [r0, toLower r1]
                      return $ s0 ++ s1)
            <|>
              try (do s0 <- do r0 <- char '#'
                               r1 <- oneOf s
                               return $ [r0, toLower r1]
                      s1 <- do e0 <- char '#'
                               e1 <- oneOf exactStr
                               return $ [e0, toLower e1]
                      return $ s0 ++ s1)
            <|>
              try (do s0 <- do r0 <- char '#'
                               r1 <- oneOf s
                               return $ [r0, toLower r1]
                      return s0)
            <|>              
              try (do s1 <- do e0 <- char '#'
                               e1 <- oneOf exactStr
                               return $ [e0, toLower e1]
                      return s1)
            <|> empty
        where exactStr = "iIeE"


--------------------------------------------------------------------------------
-- <sign> -> <empty> | + | -
--------------------------------------------------------------------------------
parseSign :: Num a => Parser a
parseSign = do sign <- string "+" <|> string "-" <|> empty
               return (if '-' `elem` sign then -1 else 1)

parseUIntDigitNE :: Num a => (ReadS a, Parser Char) -> Parser a
parseUIntDigitNE (reader, digits) = do x0 <- many1 digits
                                       return $ fst $ reader x0 !! 0

{--
parseIntDigitNE :: Num a => (ReadS a, Parser Char) -> Parser a
parseIntDigitNE arg = do sign <- parseSign
                         v <- parseUIntDigitNE arg
                         return $ sign * v
--}

parseUIntDigit :: Num a => (ReadS a, Parser Char) -> Parser (a, Bool)
parseUIntDigit arg = do v <- parseUIntDigitNE arg
                        x <- many (char '#')
                        let len = length x
                          in return (v * 10 ^ len, len == 0)

parseIntDigit :: Num a => (ReadS a, Parser Char) -> Parser (a, Bool)
parseIntDigit arg = do sign <- parseSign
                       (v, isExact) <- parseUIntDigit arg
                       return (sign * v, isExact)

--------------------------------------------------------------------------------
-- <decimal 10> -> <uinteger 10> <suffix>
--               | . <digit 10>+ #* <suffix>
--               | <digit 10>+ . <digit 10>* #* <suffix>
--               | <digit 10>+ #+ . #* <suffix>
--------------------------------------------------------------------------------
parseDecimal10 :: RealFrac a => Parser (a, Bool)
parseDecimal10 = do char '.'
                    x0 <- many1 digit
                    x1 <- many (char '#')
                    (e, _) <- parseExponent
                    let l0 = length x0
                        l1 = length x1
                        v = fst $ readDec x0 !! 0 
                      in return (v * 10 ^ l1 * 10 ^^ (-(l0 + l1)) * e, False)
             <|> try (do (x, isExact1) <- parseUIntDigit (readDec, digit)
                         (e, isExact2) <- parseExponentNE
                         return (x * e, isExact1 && isExact2))
             <|> try (do x0 <- many1 digit
                         char '.'
                         x1 <- many digit
                         x2 <- many (char '#')
                         (e, _) <- parseExponent
                         let v0 = fst $ readDec x0 !! 0
                             l1 = length x1
                             l2 = length x2
                             v1 = if l1 > 0 then fst $ readDec x1 !! 0 else if l2 > 0 then 1 else 0
                           in return ((v0 + v1 * 10 ^ l2 * 10 ^^ (-(l1 + l2))) * e, False))
             <|> do x0 <- many1 digit
                    x1 <- many1 (char '#')
                    char '.'
                    many (char '#')
                    (e, _) <- parseExponent
                    let v0 = fst $ readDec x0 !! 0
                        l1 = length x1
                      in return ((v0 * 10 ^ l1) * e, False)

parseSignedDecimal10 :: RealFrac a => Parser (a, Bool)
parseSignedDecimal10 = do sign <- parseSign
                          (v, isExact) <- parseDecimal10
                          return (sign * v, isExact)
                          
--------------------------------------------------------------------------------
-- <complex R> -> <real R> | <real R> @ <real R>
--              | <real R> + <ureal R> i | <real R> - <ureal R> i
--              | <real R> + i | <real R> - i
--              | + <ureal R> i | - <ureal R> i | + i | - i
--------------------------------------------------------------------------------
parseUReal :: RealFrac a => Parser (a, Bool)
parseUReal = try parseDecimal10 <|> parseUIntDigit (readDec, digit)

parseReal :: RealFrac a => Parser (a, Bool)
parseReal = do sign <- parseSign
               (v, isExact) <- parseUReal
               return (sign * v, isExact)

parseComplex10 :: RealFloat a => Parser (Complex a, Bool)
parseComplex10 = try (do (rel, isExact1) <- parseReal
                         char '+'
                         (img, isExact2) <- parseUReal
                         char 'i'
                         return (rel :+ img, isExact1 && isExact2))
             <|> try (do (rel, isExact1) <- parseReal
                         char '-'
                         (img, isExact2) <- parseUReal
                         char 'i'
                         return (rel :+ (-img), isExact1 && isExact2))
             <|> try (do (rel, isExact) <- parseReal
                         string "+i"
                         return (rel :+ 1, isExact))
             <|> try (do (rel, isExact) <- parseReal
                         string "-i"
                         return (rel :+ (-1), isExact))
             <|> try (do (len, isExact1) <- parseReal
                         char '@'
                         (rad, isExact2) <- parseReal
                         return ((len * cos rad) :+ (len * sin rad), isExact1 && isExact2))             
             <|> try (do char '+'
                         (img, isExact) <- parseUReal
                         char 'i'
                         return (0 :+ img, isExact))
             <|> try (do char '-'
                         (img, isExact) <- parseUReal
                         char 'i'
                         return (0 :+ (-img), isExact))
             <|> do string "+i"
                    return (0 :+ 1, True)
             <|> do string "-i"
                    return (0 :+ (-1), True)
             

--------------------------------------------------------------------------------
-- <suffix> -> <empty>
--           | <exponent marker> <sign> <digit 10>+
-- <exponent marker> -> e | s | f | d | l
--------------------------------------------------------------------------------
parseExponentNE :: RealFrac a => Parser (a, Bool)
parseExponentNE = do oneOf "eEsSfFdDlL"
                     sign <- parseSign
                     v <- parseUIntDigitNE (readDec, digit)
                     return ((10 ^^ (sign * v)), False)

parseExponent :: RealFrac a => Parser (a, Bool)
parseExponent = parseExponentNE <|> do empty
                                       return (1, True)

isBinary :: Char -> Bool
isBinary x | x == '0' = True
           | x == '1' = True
           | otherwise = False

binDigit = satisfy isBinary <?> "binary digit"

getParser :: Num a => String -> (ReadS a, Parser Char)
getParser s = if 'b' `elem` s then (readInt 2 isBinary (\x -> case x of '0' -> 0
                                                                        '1' -> 1
                                                                        _ -> error "Will not come here" ), binDigit)
                else if 'o' `elem` s then (readOct, octDigit)
                       else if 'x' `elem` s then (readHex, hexDigit)
                              else (readDec, digit)
          

radixStr = "dDbBoOxX"

parseInt :: Parser LispVal
parseInt = do prefix <- exactness radixStr
              (ret, isExact) <- parseIntDigit (getParser prefix)
              return $ Number (Integer ret)
                              ('e' `elem` prefix || 'i' `notElem` prefix && isExact)

parseRatio :: Parser LispVal
parseRatio = do prefix <- exactness radixStr
                let r = getParser prefix
                (ret1, isExact1) <- parseIntDigit r
                char '/'
                (ret2, isExact2) <- parseUIntDigit r
                return $ Number (Rational (ret1 % ret2))
                                ('e' `elem` prefix || 'i' `notElem` prefix && isExact1 && isExact2)  

          
parseFloat :: Parser LispVal
parseFloat = do prefix <- exactness "dD"
                (ret, isExact) <- parseSignedDecimal10
                return $ Number (Float ret)
                                ('e' `elem` prefix || 'i' `notElem` prefix && isExact)


parseComplex :: Parser LispVal
parseComplex = do prefix <- exactness "dD"
                  (ret, isExact) <- parseComplex10
                  return $ Number (Complex ret)
                                  ('e' `elem` prefix || 'i' `notElem` prefix && isExact)

parseNumber :: Parser LispVal
parseNumber = try parseComplex
          <|> try parseFloat
          <|> try parseRatio
          <|> parseInt

parseList :: Parser LispVal
parseList = liftM List $ parseExpr `sepEndBy` spaces1


parseDottedPair :: Parser LispVal
parseDottedPair = do head <- parseExpr `endBy` spaces1
                     tail <- char '.' >> spaces1 >> parseExpr
                     spaces
                     case tail of List [Atom "unquote", _ ] -> return $ DottedPair head tail
                                  List [Atom "unquote-splicing", _ ] -> return $ DottedPair head tail
                                  List as -> return $ List (head ++ as)
                                  DottedPair [Atom "unquote", _ ] _ -> return $ DottedPair head tail
                                  DottedPair [Atom "unquote-splicing", _ ] _ -> return $ DottedPair head tail
                                  DottedPair as tail' -> return $ DottedPair (head ++ as) tail'
                                  _ -> return $ DottedPair head tail

parseVector :: Parser LispVal
parseVector = do string "#(" >> spaces
                 es <- parseList
                 char ')'
                 return $ Vector es

parseSExpr :: Parser LispVal
parseSExpr = do char '(' >> spaces
                es <- try parseDottedPair <|> parseList
                char ')'
                return es
        
parseEllipsisVar :: Parser LispVal
parseEllipsisVar = do s <- parseAtom
                      spaces1 >> string "..."
                      return $ EllipsisVar s []


parseQuoted :: Parser LispVal
parseQuoted = do char '\''
                 x <- parseExpr
                 return $ List [Atom "quote", x]

parseQuasiquoted :: Parser LispVal
parseQuasiquoted = do char '`'
                      x <- parseExpr
                      return $ List [Atom "quasiquote", x]

parseUnquoted :: Parser LispVal
parseUnquoted = do char ','
                   x <- parseExpr
                   return $ List [Atom "unquote", x]

parseUnquoteSplicing :: Parser LispVal
parseUnquoteSplicing = do string ",@"
                          x <- parseExpr
                          return $ List [Atom "unquote-splicing", x]

parseExpr :: Parser LispVal
parseExpr = try parseBool
        <|> try parseChar
        <|> try parseNumber
        <|> try parseEllipsisVar
        <|> try parseUnquoteSplicing
        <|> parseUnquoted
        <|> parseQuasiquoted
        <|> parseQuoted    
        <|> parseAtom
        <|> parseString
        <|> parseSExpr
        <|> parseVector

readOrThrow :: Parser a -> String -> IOThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val
