import Data.List
import Data.Maybe

-- A symbol denoting a variable; consists of a textual name and some (or no) apostrophes to ensure uniqueness
class (Eq s) => Symbol s where
  -- Present symbols (not including symbol to change) -> symbol to change ->
  --   simplest unique symbol
  findName :: [s] -> s -> s

-- Symbol that appends apostrophes to become unique
data SymApostrophe = SymApostrophe { symApoBase :: String -- lowercase a to z
                                   , symApoLen  :: Int    -- nonnegative
                                   }
  deriving (Eq)

instance Show SymApostrophe where
  show (SymApostrophe s n) = s ++ (replicate n '\'')

instance Symbol SymApostrophe where
  findName other (SymApostrophe base n) =
    let sameBase = filter ((base ==) . symApoBase) other
        lengths = map symApoLen sameBase
        freeLengths = [0..] \\ (nub lengths)
    in SymApostrophe base (head freeLengths) -- [0..] is infinite

-- Symbol that changes to another letter in the alphabet to become unique
data SymLetter = SymLetter {symLetBase :: String } -- lowercase a to z, aa to zz, ...
  deriving (Eq)

instance Show SymLetter where
  show (SymLetter l) = l

instance Symbol SymLetter where
  findName other _ = 
    let bases = map symLetBase other
        freeBases = names \\ (nub bases)
    in  SymLetter (head freeBases)
      where letters = ['a'..'z']
            namesN 0 = [""]
            namesN n = [n ++ [l] | n <- namesN (n - 1), l <- letters]
            names = concatMap namesN [1..]


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

showTopLevel :: (Show s) => Expression s -> String
showTopLevel e@(EExpr _ _) = tail . init . show $ e
showTopLevel e = show e

printTopLevel :: (Show s) => Expression s -> IO ()
printTopLevel = putStrLn . showTopLevel

{-
Apply a function
Insert an expression into another expression:
  replace all symbols s with an expression
  change all symbols within that expression to be unique in the expression's new context
  simplify all symbols
-}
simplifyExpr :: (Symbol s) => [(s, s)] -> [s] -> Expression s -> Expression s
simplifyExpr mapping context (ESymbol s) = ESymbol $ fromMaybe (findName context s) $ lookup s mapping
simplifyExpr mapping context (EExpr a b) = EExpr (simplifyExpr mapping context a) (simplifyExpr mapping context b)
simplifyExpr mapping context (ELambda l e) =
  let newl = findName context l
      newmapping = (l, newl) : mapping
      newcontext = newl : context
  in  ELambda newl (simplifyExpr newmapping newcontext e)

makeUnique :: (Symbol s) => [s] -> Expression s -> Expression s
makeUnique = simplifyExpr []

simplify :: (Symbol s) => Expression s -> Expression s
simplify = makeUnique []

insertExpr :: (Symbol s) => s -> Expression s -> [s] -> Expression s -> Expression s
insertExpr r new context old@(ESymbol s)
  | r == s    = makeUnique context new
  | otherwise = old
insertExpr r new context (EExpr a b) = EExpr (insertExpr r new context a) (insertExpr r new context b)
insertExpr r new context (ELambda l e) = ELambda l (insertExpr r new (l : context) e)

apply :: (Symbol s) => Expression s -> Expression s
apply (EExpr (ELambda s l) b) = insertExpr s b [] l -- [], not [s]
apply (EExpr a@(EExpr _ _) b) = EExpr (apply a) b
apply e = e

step :: (Symbol s) => Expression s -> Expression s
step = simplify . apply

_sa = flip SymApostrophe 0
_sl = SymLetter
_ea s = ESymbol $ _sa s
_el s = ESymbol $ _sl s

main = do
  putStrLn "Testing: Showing expressions"
  print (EExpr (_ea "a") (EExpr (_ea "b") (_ea "c")))
  print (EExpr (_ea "a") (EExpr (_ea "b") (EExpr (_ea "c") (_ea "d"))))
  print (EExpr (EExpr (_ea "a") (_ea "b")) (_ea "c"))
  print (EExpr (EExpr (EExpr (_ea "a") (_ea "b")) (_ea "c")) (_ea "d"))
  print (EExpr (EExpr (_ea "a") (EExpr (_ea "b") (_ea "c"))) (_ea "d"))
  print (ELambda (_sa "a") (_ea "a"))
  print (ELambda (_sa "a") (EExpr (EExpr (_ea "a") (EExpr (_ea "b") (_ea "c"))) (_ea "d")))
  putStrLn ""
  
  -- test of findName (seems to be working) ~G
  putStrLn "Testing: Finding new symbols"
  putStrLn "SymApostrophe"
  print $ findName [(SymApostrophe "a" 0), (SymApostrophe "b" 0), (SymApostrophe "a" 1)] (SymApostrophe "a" 4)
  print $ findName [(SymApostrophe "a" 0), (SymApostrophe "b" 0), (SymApostrophe "a" 1)] (SymApostrophe "b" 3)
  print $ findName [(SymApostrophe "a" 0), (SymApostrophe "b" 0), (SymApostrophe "a" 1)] (SymApostrophe "c" 2)
  print $ findName [(SymApostrophe "a" 1), (SymApostrophe "a" 3), (SymApostrophe "a" 0)] (SymApostrophe "a" 1)
  putStrLn "SymLetter"
  print $ findName [(_sl "a"), (_sl "b"), (_sl "a")] (_sl "a")
  print $ findName [(_sl "a"), (_sl "b"), (_sl "d")] (_sl "b")
  print $ findName [(_sl "a"), (_sl "d"), (_sl "c")] (_sl "c")
  print $ findName [(_sl "b"), (_sl "a"), (_sl "c")] (_sl "d")
  putStrLn ""
  
  putStrLn "Testing: Applying expressions"
  let ta = (ELambda (_sa "a") (ELambda (_sa "b") (_ea "a")))
      fa = (ELambda (_sa "a") (ELambda (_sa "b") (_ea "b")))
      na = (ELambda (_sa "a") (EExpr (EExpr (_ea "a") (makeUnique [(_sa "a")] fa)) (makeUnique [(_sa "a")] ta)))
      tl = (ELambda (_sl "a") (ELambda (_sl "b") (_el "a")))
      fl = (ELambda (_sl "a") (ELambda (_sl "b") (_el "b")))
      nl = (ELambda (_sl "a") (EExpr (EExpr (_el "a") (makeUnique [(_sl "a")] fl)) (makeUnique [(_sl "a")] tl)))
  print ta
  print fa
  print na
  print tl
  print fl
  print nl
  print $ simplify $ (ELambda (SymApostrophe "a" 1) (ESymbol (SymApostrophe "a" 1)))
  print $ simplify $ (ELambda (_sl "b") (ESymbol (_sl "b")))
  putStrLn $ showTopLevel (EExpr na fa)
  putStrLn $ showTopLevel (EExpr nl fl)
  putStrLn ""
  
  putStrLn "Replacing symbols: apostrophe"
  putStrLn "Running: not false"
  printTopLevel (EExpr na fa)
  printTopLevel . step $ (EExpr na fa)
  printTopLevel . step . step $ (EExpr na fa)
  printTopLevel . step . step . step $ (EExpr na fa)
  
  putStrLn "Running: not true"
  printTopLevel (EExpr na ta)
  printTopLevel . step $ (EExpr na ta)
  printTopLevel . step . step $ (EExpr na ta)
  printTopLevel . step . step . step $ (EExpr na ta)
  putStrLn ""
  
  putStrLn "Replacing symbols: letter"
  putStrLn "Running: not false"
  printTopLevel (EExpr nl fl)
  printTopLevel . step $ (EExpr nl fl)
  printTopLevel . step . step $ (EExpr nl fl)
  printTopLevel . step . step . step $ (EExpr nl fl)
  
  putStrLn "Running: not true"
  printTopLevel (EExpr nl tl)
  printTopLevel . step $ (EExpr nl tl)
  printTopLevel . step . step $ (EExpr nl tl)
  printTopLevel . step . step . step $ (EExpr nl tl)
  putStrLn ""
