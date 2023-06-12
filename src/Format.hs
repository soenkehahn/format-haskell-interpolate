{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Format (run, format) where

import Control.Monad
import Data.Void (Void)
import Debug.Trace (traceShowId)
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
    . traceShowId
    . either (error . errorBundlePretty) id
    . parse snippets file

type Parser = Parsec Void String

data Snippet
  = Line String
  | IString
      { baseIndentation :: Int,
        content :: [String]
      }
  deriving stock (Show)

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
      (skipMany (char ' ') >> line)
      (try (manyTill (char ' ') (chunk "|]\n")))
  return $ IString baseIndentation content

render :: [Snippet] -> String
render = concatMap $ \case
  Line line -> line <> "\n"
  IString baseIndentation content ->
    unlines $
      fmap
        (nTimes baseIndentation indent)
        ( ["[i|"]
            ++ map (nTimes 2 indent) content
            ++ ["|]"]
        )

indent :: String -> String
indent = (" " <>)

nTimes :: Int -> (a -> a) -> a -> a
nTimes n f x = case n of
  0 -> x
  n -> nTimes (n - 1) f (f x)
