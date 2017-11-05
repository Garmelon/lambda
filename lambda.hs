-- I removed the line at the bottom, now we can also remove this line. ~G
import Data.List
import Data.Maybe

-- A symbol denoting a variable; consists of a textual name and some (or no) apostrophes to ensure uniqueness
class (Eq s) => Symbol s where
  -- Present symbols (not including symbol to change) -> symbol to change ->
  --   simplest unique symbol
  findName :: [s] -> s -> s
  
  numberUniquely :: [Int] -> s -> Int
  numberUniquely context _ = head $ [1..] \\ (nub context)

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
      where namesN 0 = [""]
            namesN n = [n ++ [l] | n <- namesN (n - 1), l <- ['a'..'z']]
            names = concatMap namesN [1..]


-- An expression. Can be a mere symbol, a lambda application, or a lambda abstraction.
-- re. "a lambda application": (a b) is also a valid expression, even though nothing is applied here ~G
data Expression s = ESymbol s
                  | EExpr   (Expression s) (Expression s)
                  | ELambda s (Expression s)

instance (Show s) => Show (Expression s) where
  show (ESymbol s) = show s
  -- ((a b) c) is equivalent to (a b c)
  show (EExpr a@(EExpr _ _) b) = (init $ show a) ++ " " ++ show b ++ ")"
  show (EExpr a b) = "(" ++ show a ++ " " ++ show b ++ ")"
  show (ELambda s e) = "\\" ++ show s ++ "." ++ show e

instance (Symbol s) => Eq (Expression s) where
  a == b = cmp (contextMap numberUniquely [] a) (contextMap numberUniquely [] b)
    where cmp (ESymbol s1) (ESymbol s2) = s1 == s2
          cmp (EExpr a1 b1) (EExpr a2 b2) = cmp a1 a2 && cmp b1 b2
          cmp (ELambda s1 e1) (ELambda s2 e2) = s1 == s2 && cmp e1 e2
          cmp _ _ = False

showTopLevel :: (Show s) => Expression s -> String
showTopLevel e@(EExpr _ _) = tail . init . show $ e
showTopLevel e = show e

printTopLevel :: (Show s) => Expression s -> IO ()
printTopLevel = putStrLn . showTopLevel

contextMap :: (Eq s, Eq t) => ([t] -> s -> t) -> [t] -> Expression s -> Expression t
contextMap f context e = helper f [] context e
  where helper f mapping context (ESymbol s) = ESymbol $ fromMaybe (f context s) $ lookup s mapping
        helper f mapping context (EExpr a b) = EExpr (helper f mapping context a) (helper f mapping context b)
        helper f mapping context (ELambda s e) =
          let news = f context s
              newmapping = (s, news) : mapping
              newcontext = news : context
          in  ELambda news (helper f newmapping newcontext e)

makeUnique :: (Symbol s) => [s] -> Expression s -> Expression s
makeUnique = contextMap findName

simplify :: (Symbol s) => Expression s -> Expression s
simplify = makeUnique []

-- symbol to replace -> expression to replace symbol with -> expression to replace in -> resulting epression
replaceIn :: (Symbol s) => s -> Expression s -> Expression s -> Expression s
replaceIn = helper []
  where helper context replace new old@(ESymbol s)
          | replace == s = makeUnique context new
          | otherwise    = old
        helper context replace new (EExpr a b) = EExpr (helper context replace new a) (helper context replace new b)
        helper context replace new (ELambda s e) = ELambda s (helper (s : context) replace new e)

apply :: (Symbol s) => Expression s -> Expression s
apply (EExpr (ELambda s a) b) = replaceIn s b a -- replace s with b in a
apply (EExpr a@(EExpr _ _) b) = EExpr (apply a) b
apply e = e

step :: (Symbol s) => Expression s -> Expression s
step = simplify . apply

takeWhileUnique :: (Eq a) => [a] -> [a]
takeWhileUnique l = map fst $ takeWhile (\a -> not $ fst a `elem` snd a) $ zip l (inits l)

evaluate :: (Symbol s) => Expression s -> [Expression s]
evaluate = takeWhileUnique . iterate apply

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
  
  putStrLn "-----"
  mapM_ printTopLevel $ evaluate (EExpr nl tl)
  putStrLn "-----"
