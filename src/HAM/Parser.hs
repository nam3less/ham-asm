module HAM.Parser where

import Control.Applicative hiding (some, many)
import Control.Monad
import Data.Text(Text)
import Data.Void
import qualified Data.Text as T
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

import HAM.Asm

type Parser = Parsec Void Text

asm :: Parser [Expression]
asm = do
  exprs <- count' 1 4096 line
  return (eliminateNoOp exprs)

line :: Parser Expression
line = do
  void whitespace
  expr <- expression
  void $ many (noneOf eolChars)
  void $ many (oneOf eolChars)
  return expr

expression :: Parser Expression
expression = try labeledValue
       <|> try labeledInstr
       <|> try instruction
       <|> try value
       <|> do skip; return (NoOp)

eolChars :: String
eolChars = "\n\r"

skip :: Parser ()
skip = void $ many (noneOf eolChars)


instruction :: Parser Expression
instruction = try $ do
  instr <- T.pack <$> some alphaNumChar
  void whitespace
  makeInstr (T.toLower instr) <$> getAlphaNum

value :: Parser Expression
value = do
  val <- count' 1 4 hexDigitChar
  return (Value "" (T.pack val))

labeledInstr :: Parser Expression
labeledInstr = do
  labelName <- getAlphaNum
  matchChar ':'
  rest <- instruction <?> "Not a valid Instruction"
  return (labelInstr labelName rest)

labeledValue :: Parser Expression
labeledValue  = try $ do
  labelName <- getAlphaNum
  matchChar '='
  labelValue labelName <$> getHex

getHex :: Parser Text
getHex = T.pack <$> some hexDigitChar

getAlphaNum :: Parser Text
getAlphaNum = T.pack <$> some alphaNumChar

matchChar :: Char -> Parser ()
matchChar x = do
  void whitespace
  void $ char x
  void whitespace

whitespace :: Parser Text
whitespace = T.pack <$> many (oneOf space')

space' :: String
space' = " \t"
