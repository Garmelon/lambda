import Data.List

data StrSymbol = StrSymbol { symBase :: String -- lowercase a to z
                           , symLen :: Int
                           }

instance Show StrSymbol where
  show (StrSymbol s n) = s ++ (replicate n '\'')

class Symbol a where
  -- Should we use a set instead of a list here? ~G
  --   Hardly five minutes away and you're already overengineering. ~X
  --   Overengineering is fun! ~G
  -- Also, what is the second parameter? ~X
  --   The original symbol for which a new name should be found ~G (grammer is herd)
  --     Do we need that or can we just let it return a new symbol? ~X
  --     If we just let it return a new symbol, we would lose the thing where something called a will be renamed to a'
  --     and later back to a (keep the same name) ~G
  --     How about we define a findNewName that just finds a new unique symbol that is not in the list, so we can try out both
  --     things later on. ~G
  findName :: [a] -> a -> a
  findNewName :: [a] -> a

instance Symbol StrSymbol where
  findName other (StrSymbol base n) =
    let sameBase = filter ((base ==) . symBase) other
        lengths = map symLen sameBase
        freeLengths = [0..] \\ (nub lengths)
    in  StrSymbol base (head freeLengths) -- [0..] is infinite
  
  -- findNewName basically finds a new unique base ~G
  -- try to find a name in the sequence
  -- a..z, aa..zz, aaa........
  {-
  findNewName other =
    let bases = nub $ map symBase other
  -}

data Expression s = ESymbol s
                  | EExpr   (Expression s) (Expression s)
                  | ELambda s (Expression s)

instance (Show s) => Show (Expression s) where
  show (ESymbol s) = show s
  -- show (EExpr a e@(EExpr _ _)) = "(" ++ show a ++ " " ++ (drop 1 $ show e) -- hack hack hack
  show (EExpr a@(EExpr _ _) b) = (init $ show a) ++ " " ++ show b ++ ")" -- hack hack hack
  show (EExpr a b) = "(" ++ show a ++ " " ++ show b ++ ")"
  show (ELambda s e) = "\\" ++ show s ++ "." ++ show e

_s :: String -> Expression StrSymbol
_s s = ESymbol (StrSymbol s 0)

_ss :: String -> StrSymbol
_ss s = (StrSymbol s 0)

main = do
  print (EExpr (_s "a") (EExpr (_s "b") (_s "c")))
  print (EExpr (_s "a") (EExpr (_s "b") (EExpr (_s "c") (_s "d"))))
  print (EExpr (EExpr (_s "a") (_s "b")) (_s "c"))
  print (EExpr (EExpr (EExpr (_s "a") (_s "b")) (_s "c")) (_s "d"))
  print (EExpr (EExpr (_s "a") (EExpr (_s "b") (_s "c"))) (_s "d"))
  print (ELambda (StrSymbol "a" 0) (_s "a"))
  print (ELambda (StrSymbol "a" 0) (EExpr (EExpr (_s "a") (EExpr (_s "b") (_s "c"))) (_s "d")))
  
  -- test of findName (seems to be working) ~G
  print $ findName [(StrSymbol "a" 0), (StrSymbol "b" 0), (StrSymbol "a" 1)] (StrSymbol "a" 4)
  print $ findName [(StrSymbol "a" 0), (StrSymbol "b" 0), (StrSymbol "a" 1)] (StrSymbol "b" 3)
  print $ findName [(StrSymbol "a" 0), (StrSymbol "b" 0), (StrSymbol "a" 1)] (StrSymbol "c" 2)
  print $ findName [(StrSymbol "a" 1), (StrSymbol "a" 3), (StrSymbol "a" 0)] (StrSymbol "a" 1)
