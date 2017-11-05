
import Data.List

-- A symbol denoting a variable; consists of a textual name and some (or no) apostrophes to ensure uniqueness
data Symbol = Symbol { symBase :: String -- lowercase a to z
                     , symLen  :: Int    -- nonnegative
                     }

instance Show Symbol where
  show (Symbol s n) = s ++ (replicate n '\'')

-- Present symbols (not including symbol to change) -> symbol to change ->
--   unique symbol with same base and minimal amount of apostrophes
findName :: [Symbol] -> Symbol -> Symbol
findName other (Symbol base n) =
  let sameBase = filter ((base ==) . symBase) other
      lengths = map symLen sameBase
      freeLengths = [0..] \\ (nub lengths)
  in Symbol base (head freeLengths) -- [0..] is infinite

-- An expression. Can be a mere symbol, a lambda application, or a lambda abstraction.
data Expression s = ESymbol s
                  | EExpr   (Expression s) (Expression s)
                  | ELambda s (Expression s)

instance (Show s) => Show (Expression s) where
  show (ESymbol s) = show s
  -- ((a b) c) is equivalent to (a b c)
  show (EExpr a@(EExpr _ _) b) = (init $ show a) ++ " " ++ show b ++ ")"
  show (EExpr a b) = "(" ++ show a ++ " " ++ show b ++ ")"
  show (ELambda s e) = "\\" ++ show s ++ "." ++ show e

_s :: String -> Expression Symbol
_s s = ESymbol $ Symbol s 0

main = do
  print (EExpr (_s "a") (EExpr (_s "b") (_s "c")))
  print (EExpr (_s "a") (EExpr (_s "b") (EExpr (_s "c") (_s "d"))))
  print (EExpr (EExpr (_s "a") (_s "b")) (_s "c"))
  print (EExpr (EExpr (EExpr (_s "a") (_s "b")) (_s "c")) (_s "d"))
  print (EExpr (EExpr (_s "a") (EExpr (_s "b") (_s "c"))) (_s "d"))
  print (ELambda (Symbol "a" 0) (_s "a"))
  print (ELambda (Symbol "a" 0) (EExpr (EExpr (_s "a") (EExpr (_s "b") (_s "c"))) (_s "d")))
  
  -- test of findName (seems to be working) ~G
  print $ findName [(Symbol "a" 0), (Symbol "b" 0), (Symbol "a" 1)] (Symbol "a" 4)
  print $ findName [(Symbol "a" 0), (Symbol "b" 0), (Symbol "a" 1)] (Symbol "b" 3)
  print $ findName [(Symbol "a" 0), (Symbol "b" 0), (Symbol "a" 1)] (Symbol "c" 2)
  print $ findName [(Symbol "a" 1), (Symbol "a" 3), (Symbol "a" 0)] (Symbol "a" 1)

