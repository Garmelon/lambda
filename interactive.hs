import qualified Lambda as L
import Data.Maybe

-- Helper type for using arbitrary strings as symbols
newtype StrSymbol = StrSymbol String

instance Show StrSymbol where
  show (StrSymbol s) = s

instance Eq StrSymbol where
  (StrSymbol a) == (StrSymbol b) = a == b

linewise :: (String -> String) -> String -> String
linewise f = unlines . map f . lines

evaluate :: String -> String
evaluate s =
  let result = return
             . map (L.display True)
             . L.evaluate
             . fmap StrSymbol
      l = fromMaybe ["Error: Could not parse expression."]
        $ L.parseMaybe s >>= result
  in  unlines l

main = interact $ linewise evaluate
