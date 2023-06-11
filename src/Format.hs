module Format (format) where

format :: String -> String
format = unlines . go False . lines

go :: Bool -> [String] -> [String]
go insideInterpolate (line : rest) = case line of
  "[i|" -> line : go True rest
  "|]" -> line : go False rest
  _ | insideInterpolate -> ("  " <> line) : go True rest
  _ -> line : go insideInterpolate rest
go _ [] = []
