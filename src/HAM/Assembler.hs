module HAM.Assembler where

import Numeric (showHex)
import Data.Map(Map)
import qualified Data.Map as Map

import Data.Text(Text)
import qualified Data.Text as T

import HAM.Asm

type NumberedExpression = (Int, Expression)

assemble :: [Expression] -> [Text]
assemble exprs = (exprsToText . unLabel) exprs
  where labelMap = (buildMap . enumerateExprs) exprs
        unLabel  = replaceLabels labelMap

enumerateExprs :: [Expression] -> [NumberedExpression]
enumerateExprs = zip [0..]

buildMap :: [(Int, Expression)] -> Map Text Int
buildMap = buildLabelMap Map.empty

replaceLabels :: Map Text Int -> [Expression] -> [Expression]
replaceLabels labels = map replace
  where replace = replaceLabel labels

replaceLabel :: Map Text Int -> Expression -> Expression
replaceLabel labels expr =
  case fetchToken of
    Just tok -> updateToken expr (intToHex tok)
    Nothing -> expr
  where
    label = getToken expr
    fetchToken = Map.lookup label labels

intToHex :: Int -> Text
intToHex = T.justifyRight 3 '0' . toHex
  where toHex tok = T.pack $ showHex tok ""

exprsToText :: [Expression] -> [Text]
exprsToText = map exprToText

buildLabelMap :: Map Text Int -> [NumberedExpression] -> Map Text Int
buildLabelMap  = foldl insertLabel
  where insertLabel labels (line, expr) =
          case getLabel expr of
            Just label -> Map.insert label line labels
            Nothing    -> labels
