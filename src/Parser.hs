-- |

module Parser where

import Text.Parsec

asmFile = endBy expr eol

expr = label <|> instruction

instruction = choice [load, ]
