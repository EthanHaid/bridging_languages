import System.Process
import System.Environment
import System.IO (stdout,stderr,hPutStr,hPutStrLn)
import Data.List
import Data.Char
import Text.Read

-- Parser for algebraic expressions

{-  ALGEBRA GRAMMAR
    <expression> ::= <signed-term> | <expression> "+" <term> | <expression> "-" <term>
    <signed-term> ::= "-" <term> | <term>
    <term> ::= <element> | <term> * <element> 
    <element> ::= <variable> | <numeral> | "(" <expression> ")"
    <variable> ::= [a-z]
    <numeral> ::= [0-9]+
-}

data ME = Num Double
          | Group ME
          | Add ME ME
          | Sub ME ME
          | Mul ME ME
          | Neg ME
          deriving (Show, Ord, Eq)

parseME :: [Char] -> Maybe ME
parseExpression :: [Char] -> Maybe (ME, [Char])
parseSignedTerm :: [Char] -> Maybe (ME, [Char])
parseTerm :: [Char] -> Maybe (ME, [Char])
parseElement :: [Char] -> Maybe (ME, [Char])
parseNum :: [Char] -> Maybe (ME, [Char])

parseME s =
  case parseExpression(s) of
    Just (me, []) -> Just me
    _ -> Nothing

extendExpression :: (ME, [Char]) -> Maybe (ME, [Char])
extendExpression (me, []) = Just (me, [])
extendExpression (me1, '+':more) =
  case parseTerm(more) of
    Just (me2, and_more) -> extendExpression (Add me1 me2, and_more)
    _ -> Nothing
extendExpression (me1, '-':more) =
  case parseTerm(more) of
    Just (me2, and_more) -> extendExpression (Sub me1 me2, and_more)
    _ -> Nothing
extendExpression (me, c:more) = Just (me, c:more)

parseExpression s =
  case parseSignedTerm(s) of
    Just (me, and_more) -> extendExpression (me, and_more)
    _ -> Nothing

parseSignedTerm ('-':more) =
  case parseTerm(more) of
    Just (me, and_more) -> Just (Neg me, and_more)
    _ -> Nothing
parseSignedTerm s =
  case parseTerm(s) of
    Just (me, and_more) -> Just (me, and_more)
    _ -> Nothing

extendTerm :: (ME, [Char]) -> Maybe (ME, [Char])
extendTerm (me, []) = Just (me, [])
extendTerm (me1, '*':after_mult) =
  case parseElement(after_mult) of
    Just (me2, more) -> extendTerm (Mul me1 me2, more)
    _ -> Nothing
extendTerm (me, c:more) = Just (me, c:more)

parseTerm s =
  case parseElement(s) of
    Just (me, more) -> extendTerm (me, more)
    _ -> Nothing

maybeNum c = (elem c (['0'..'9']++['.']))
maybeDecimal c = (elem c ['0'..'9'])

parseElement ('(':more) =
  case parseExpression(more) of
    Just (me, ')':and_more) -> Just (Group me, and_more)
    _ -> Nothing
parseElement (c:more)
  | maybeNum c = parseNum (c:more)
  | otherwise  = Nothing

parseDecimal :: [Char] -> [Char] -> Maybe ([Char], [Char])
parseDecimal numstring [] = Just (numstring, [])
parseDecimal numstring (c:rest)
  | (elem c ['0'..'9']) = parseDecimal (numstring ++ [c]) rest
  | otherwise         = Just (numstring, c:rest)

parseNumeral :: [Char] -> [Char] -> Maybe ([Char], [Char])
parseNumeral numstring [] = Just (numstring, [])
parseNumeral numstring (c:rest)
  | (elem c ['0'..'9']) = parseNumeral (numstring ++ [c]) rest
  | (elem c ['.']) = parseDecimal (numstring ++ [c]) rest
  | otherwise         = Just (numstring, c:rest)

parseNum s = 
  case parseNumeral [] s of
    Just (numeral, more) -> Just (Num (read numeral), more)
    _ -> Nothing

-- Simplifier

makeNum  :: Double -> ME
makeGroup :: ME -> ME
makeAdd :: ME -> ME -> ME
makeSub :: ME -> ME -> ME
makeMul :: ME -> ME -> ME
makeNeg :: ME -> ME

-- Basic
makeNum numeral = Num numeral
makeGroup me = Group me

-- Addition
makeAdd me1 (Num 0) = me1
makeAdd (Num m) (Num n) = makeNum (m*n)
makeAdd me1 (Add (Num m) (Num n)) = makeAdd me1 (makeNum (m+n))
makeAdd (Mul (Num m) me1) (Mul (Num n) me2)
  | me1 == me2 = makeMul (makeNum (m+n)) me1
  | otherwise = Add (makeMul (makeNum m) me1) (makeMul (makeNum n) me2)

makeAdd (Num n) me = makeAdd me (makeNum n)
makeAdd (Add me1 (Num n)) me2 = makeAdd me1 (makeAdd me2 (makeNum n))
makeAdd f g = Add f g

-- Subtraction
makeSub (Num 0) me = makeNeg me
makeSub me (Num 0) = me
makeSub me (Sub (Num m) (Num n)) = makeSub me (makeNum (m+n))
makeSub (Num m) (Num n) = makeNum (m - n)

makeSub (Num m) f = makeAdd (makeNeg (f)) (makeNum m)
makeSub f g = Sub f g

-- Multiplication
makeMul (Num 0) me = makeNum 0
makeMul (Num 1) me = me
makeMul (Num m) (Num n) = makeNum (m*n)
makeMul (Num m) (Mul (Num n) f) = makeMul (makeNum (m*n)) f

makeMul f (Num n) = makeMul (makeNum n) f
makeMul f (Group (Mul g h)) = makeMul f (makeMul g h)

makeMul f g = Mul f g

-- Negative
makeNeg (Num m) = makeNum (-m)
makeNeg f = Neg f



simplifyME :: ME -> ME
simplifyME (Num a) = makeNum a
simplifyME (Group me1) = makeGroup (simplifyME me1)
simplifyME (Add me1 me2) = makeAdd (simplifyME me1) (simplifyME me2)
simplifyME (Sub me1 me2) = makeSub (simplifyME me1) (simplifyME me2)
simplifyME (Mul me1 me2) = makeMul (simplifyME me1) (simplifyME me2)
simplifyME (Neg me1) = makeNeg (simplifyME me1)


unparseME :: ME -> [Char]
unparseME (Num i) = show i
unparseME (Group a) = "(" ++ (unparseME a) ++ ")"
unparseME (Add a b) = (unparseME a) ++ "+" ++ (unparseME b)
unparseME (Sub a b) = (unparseME a) ++ "-" ++ (unparseME b)
unparseME (Mul a b) = (unparseME a) ++ "*" ++ (unparseME b)
unparseME (Neg a) = "-" ++ (unparseME a)

stringify :: ME -> [Char]
stringify (Num i) = show i
stringify (Group a) = " Group " ++ (stringify a)
stringify (Add a b) = " Add " ++ (stringify a) ++ " " ++ (stringify b)
stringify (Sub a b) = " Sub " ++ (stringify a) ++ " " ++ (stringify b)
stringify (Mul a b) = " Mul " ++ (stringify a) ++ " " ++ (stringify b)
stringify (Neg a) = "-" ++ (stringify a)


-- Command line interface
main = do
  [exp] <- getArgs
  case parseME exp of 
    Just (me) -> hPutStr stdout (unparseME (simplifyME me))
    _ -> hPutStr stdout ("invalid input\n")
