{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Format (run, format) where

import Control.Lens
import Control.Monad
import Data.Void (Void)
import System.IO (readFile')
import Text.Megaparsec
import Text.Megaparsec.Char
import WithCli

run :: IO ()
run = withCli $ \(files :: [FilePath]) -> do
  forM_ files $ \file -> do
    content <- readFile' file
    writeFile file $ format file content

format :: FilePath -> String -> String
format file =
  render
    . either (error . errorBundlePretty) id
    . parse snippets file

type Parser = Parsec Void String

data Snippet
  = Line String
  | IString
      { baseIndentation :: Int,
        content :: [IStringLine]
      }
  deriving stock (Show)

data IStringLine = IStringLine
  { _lineIndentation :: Int,
    lineContents :: String
  }
  deriving stock (Show)

lineIndentation :: Lens' IStringLine Int
lineIndentation = lens _lineIndentation (\l n -> l {_lineIndentation = n})

snippets :: Parser [Snippet]
snippets = manyTill (iString <|> (Line <$> line)) eof

line :: Parser String
line =
  manyTill anySingle eol

iString :: Parser Snippet
iString = do
  baseIndentation <-
    length
      <$> try
        (manyTill (char ' ') (chunk "[i|\n"))
  content <-
    manyTill
      iStringLine
      (try (manyTill (char ' ') (chunk "|]\n")))
  return $ IString baseIndentation (normalizeIndentation content)

iStringLine :: Parser IStringLine
iStringLine = do
  indentation <- length <$> takeWhileP Nothing (== ' ')
  IStringLine indentation <$> line

normalizeIndentation :: [IStringLine] -> [IStringLine]
normalizeIndentation lines =
  map
    (lineIndentation %~ subtract minimalIndentation)
    lines
  where
    minimalIndentation =
      let indentations =
            map (^. lineIndentation) $
              filter ((/= "") . lineContents) $
                lines
       in case indentations of
            [] -> 0
            _ -> minimum indentations

render :: [Snippet] -> String
render = concatMap $ \case
  Line line -> line <> "\n"
  IString baseIndentation content ->
    unlines $
      fmap
        (nTimes baseIndentation indentLine)
        ( ["[i|"]
            ++ map
              (nTimes 2 indentLine . renderIStringLine)
              content
            ++ ["|]"]
        )

renderIStringLine :: IStringLine -> String
renderIStringLine (IStringLine indentation s) =
  nTimes indentation indentLine s

indentLine :: String -> String
indentLine = \case
  "" -> ""
  l -> " " <> l

nTimes :: Int -> (a -> a) -> a -> a
nTimes n f x =
  if n <= 0
    then x
    else nTimes (n - 1) f (f x)
