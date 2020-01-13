module HAM.Asm ( Token
               , Data
               , Label
               , Expression(..)
               , makeInstr
               , labelInstr
               , labelValue
               , getToken
               , getLabel
               , getOpCode
               , getValueOrToken
               , updateToken
               , eliminateNoOp
               , exprToText
               , unlabel
               ) where

import Data.Text(Text)
import qualified Data.Text as T

type Token = Text
type Data  = Text
type Label = Text

data Expression = LabeledInstr Label Expression
                | Value Label Data
                | Load Token
                | Store Token
                | Add Token
                | And Token
                | Jump Token
                | JumpZ Token
                | Inv Token
                | RShift Token
                | AddI Token
                | NoOp
                 deriving (Show, Eq)

getToken :: Expression -> Text
getToken (LabeledInstr _ expr) = getToken expr
getToken (Load tok)            = tok
getToken (Store tok)           = tok
getToken (Add tok)             = tok
getToken (And tok)             = tok
getToken (Jump tok)            = tok
getToken (JumpZ tok)           = tok
getToken (Inv tok)             = tok
getToken (RShift tok)          = tok
getToken (AddI tok)            = tok
getToken _                     = ""

getValueOrToken :: Expression -> Text
getValueOrToken (Value _ val) = val
getValueOrToken rest = getToken rest

getOpCode :: Expression -> Text
getOpCode (LabeledInstr _ expr) = getOpCode expr
getOpCode (Value _ _)           = ""
getOpCode (Load _)              = "0"
getOpCode (Store _)             = "1"
getOpCode (Add _)               = "2"
getOpCode (And _)               = "3"
getOpCode (Jump _)              = "4"
getOpCode (JumpZ _)             = "5"
getOpCode (Inv _)               = "6"
getOpCode (RShift _)            = "7"
getOpCode (AddI _)              = "8"
getOpCode NoOp                  = ""

unlabel :: Expression -> Expression
unlabel (LabeledInstr _ instr) = instr
unlabel expr = expr

getLabel :: Expression -> Maybe Label
getLabel (LabeledInstr label _) = Just label
getLabel (Value "" _)           = Nothing
getLabel (Value label _)        = Just label
getLabel _                      = Nothing

eliminateNoOp :: [Expression] -> [Expression]
eliminateNoOp (NoOp : xs) = eliminateNoOp xs
eliminateNoOp (x : xs) = x : eliminateNoOp xs
eliminateNoOp [] = []

updateToken :: Expression -> Token -> Expression
updateToken (LabeledInstr _ expr) tok = updateToken expr tok
updateToken (Load _) tok              = Load tok
updateToken (Store _) tok             = Store tok
updateToken (Add _) tok               = Add tok
updateToken (And _) tok               = And tok
updateToken (Jump _) tok              = Jump tok
updateToken (JumpZ _) tok             = JumpZ tok
updateToken (Inv _) tok               = Inv tok
updateToken (RShift _) tok            = RShift tok
updateToken (AddI _) tok              = AddI tok
updateToken expr _                    = expr

makeInstr :: Label -> Token -> Expression
makeInstr "load" x   = Load x
makeInstr "store" x  = Store x
makeInstr "add" x    = Add x
makeInstr "and" x    = And x
makeInstr "jump" x   = Jump x
makeInstr "jumpz" x  = JumpZ x
makeInstr "inv" x    = Inv x
makeInstr "rshift" x = RShift x
makeInstr "addi" x   = AddI x
makeInstr _ _        = NoOp

exprToText :: Expression -> Text
exprToText expr =
  case getOpCode expr of
    "" -> T.justifyRight 4 '0' (getValueOrToken expr)
    _  -> getOpCode expr `T.append` T.justifyRight 3 '0' (getValueOrToken expr)

labelInstr :: Token -> Expression -> Expression
labelInstr = LabeledInstr

labelValue :: Token -> Data -> Expression
labelValue = Value
