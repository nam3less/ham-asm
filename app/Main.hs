module Main where

import HAM.Parser (asm)
import HAM.Assembler (assemble)
import HAM.Asm (Expression)

import Data.Semigroup ((<>))
import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Options.Applicative
import Numeric (showHex)

import Text.Megaparsec

data Options = Options
  { debug :: Bool
  , file :: String }

options :: Parser Options
options = Options
        <$> switch
           (long "debug"
           <> short 'd'
           <> help "Whether to additionally create a .debug file showing bytecode and assembly language.")
        <*> argument str (metavar "FILE")


main :: IO ()
main = assembler =<< execParser opts
  where opts :: ParserInfo Options
        opts = info (options <**> helper)
          ( fullDesc
          <> progDesc "Assembles the HAM assembly language"
          <> header "ham-asm - assembler for ham")

assembler :: Options -> IO ()
assembler (Options True path) = do
  input <- T.readFile path
  let (Right ast) = parse asm "" input
      (Just outPath) = changeEnding "hex" path
      (Just debugPath) = changeEnding "txt" path
      assembled = assemble ast
      debugInfo = getDebug assembled ast
  T.writeFile debugPath $ unline debugInfo
  T.writeFile outPath $ unline (T.pack "v2.0 raw" : assembled)
assembler (Options False path) = do
  input <- T.readFile path
  let (Right ast) = parse asm "" input
      (Just outPath) = changeEnding "hex" path
      assembled = assemble ast
  T.writeFile outPath $ unline (T.pack "v2.0 raw" : assembled)


unline :: [Text] -> Text
unline = T.intercalate "\n"

getDebug :: [Text] -> [Expression] -> [Text]
getDebug hexList exprs = map debugLine (zip3 [0..] hexList exprs)
  where debugLine :: (Int, Text, Expression) -> Text
        debugLine (lineNum, hex, expr) =
          T.intercalate " " [hex, T.justifyRight 3 '0' $ T.pack $ showHex lineNum "", T.pack $ show expr]

changeEnding :: Text -> String -> Maybe String
changeEnding newEnd inPath =  T.unpack <$> outPath (T.splitOn "." (T.pack inPath))
  where outPath :: [Text] -> Maybe Text
        outPath ("asm" : _) = Just newEnd
        outPath (x : xs)    = T.append <$> Just (T.append x ".") <*> outPath xs
        outPath []          = Nothing
