module Parser (
    smithParser
  ) where

import Prelude ()
import Prelude'

import Tokens

import ApplicativeParsec

import Data.Char
import Data.List hiding ((++))

import Control.Monad

-----------------------------------------------------------

infixl 4 <?, <^>, <&>

-----------------------------------------------------------

smithParser :: CharParser st [Operator]
smithParser = do
  whitespaces
  xs <- many $ (Left . operator <|> Right . macro) <* whitespaces1
  eof
  let ops = foldr (
        \x xs -> (++ xs) $ case x of
          Left op -> [op]
          Right (REP n op) -> genericReplicate (max 0 n) op
        ) [] xs
  return $ snd $ foldr (
    \op (n, ops) -> (n - 1, (iterate (expandLine $ Immed n) op !! 3) : ops) 
    ) (genericLength ops - 1, []) ops
  where
    expandLine n op = case op of
      MOV r (Left CurrLine) -> MOV r $ Left n
      SUB r CurrLine -> SUB r n
      MUL r CurrLine -> MUL r n
      COR CurrLine o1 o2 -> COR n o1 o2
      COR o1 CurrLine o2 -> COR o1 n o2
      COR o1 o2 CurrLine -> COR o1 o2 n
      BLA CurrLine opr o -> BLA n opr o
      BLA o opr CurrLine -> BLA o opr n
      _ -> op

operator :: CharParser st Operator
operator = choice [mov, sub, mul, not, cor, bla, nop, stop]
  where
    mov = MOV <? istring "MOV" <^> register <&> (Left . operand <|> Right . stringL)
    sub = SUB <? istring "SUB" <^> register <&> operand
    mul = MUL <? istring "MUL" <^> register <&> operand
    not = NOT <? istring "NOT" <^> register
    cor = COR <? istring "COR" <^> operand <&> operand <&> operand
    bla = BLA <? istring "BLA" <^> operand <&> choice [nop, stop] <&> operand
    nop = NOP <? istring "NOP"
    stop = STOP <? istring "STOP"

macro :: CharParser st Macro
macro = REP <? istring "REP" <^> decimal <^> operator

-----------------------------------------------------------

whitespace :: CharParser st ()
whitespace = () <$ space <|> () <$ comment
  where
    comment = char ';' *> manyTill anyChar (oneOf "\r\n")

whitespaces :: CharParser st ()
whitespaces = () <$ many whitespace

whitespaces1 :: CharParser st ()
whitespaces1 = whitespace *> whitespaces <|> eof

(<^>) :: CharParser st (a -> b) -> CharParser st a -> CharParser st b
f <^> p = f <*> (whitespaces1 *> p)

(<&>) :: CharParser st (a -> b) -> CharParser st a -> CharParser st b
f <&> p = f <*> (whitespaces *> char ',' *> whitespaces *> p)

(<?) :: a -> CharParser st b -> CharParser st a
x <? p = x <$ try p

-----------------------------------------------------------

decimal :: CharParser st Integer
decimal = read . many1 digit

ichar :: Char -> CharParser st Char
ichar c = oneOf [toUpper c, toLower c]

istring :: String -> CharParser st String
istring = mapM ichar

register :: CharParser st Register
register = choice [
    TTY <? istring "TTY"
  , PC <? istring "PC"
  , ichar 'R' >> REG . decimal
             <|> IREG . (whitespaces *> char '[' *> whitespaces *> register <* whitespaces <* char ']')
  ]

stringL :: CharParser st String
stringL = quote *> manyTill anyChar quote
  where
    quote = char '"'

numL :: CharParser st Integer
numL = sign . option '+' (oneOf "+-") <*> decimal
  where
    sign '+' = id
    sign '-' = negate

operand :: CharParser st Operand
operand = choice [
    Immed . numL
  , Reg . register
  , CurrLine <$ char '*'
  ]
