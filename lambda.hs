
import Data.List
import Data.Maybe

-- A symbol denoting a variable; consists of a textual name and some (or no) apostrophes to ensure uniqueness
data Symbol = Symbol { symBase :: String -- lowercase a to z
                     , symLen  :: Int    -- nonnegative
                     }
  deriving (Eq)

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

showTopLevel :: (Show s) => Expression s -> String
showTopLevel e@(EExpr _ _) = tail . init . show $ e
showTopLevel e = show e

{-
Apply a function
Insert an expression into another expression:
  replace all symbols s with an expression
  change all symbols within that expression to be unique in the expression's new context
  simplify all symbols
-}
simplifyExpr :: [(Symbol, Symbol)] -> [Symbol] -> Expression Symbol -> Expression Symbol
simplifyExpr mapping context (ESymbol s) = ESymbol $ fromMaybe (findName context s) $ lookup s mapping
simplifyExpr mapping context (EExpr a b) = EExpr (simplifyExpr mapping context a) (simplifyExpr mapping context b)
simplifyExpr mapping context (ELambda l e) =
  let newl = findName context l
      newmapping = (l, newl) : mapping
      newcontext = newl : context
  in  ELambda newl (simplifyExpr newmapping newcontext e)

makeUnique :: [Symbol] -> Expression Symbol -> Expression Symbol
makeUnique = simplifyExpr []

simplify :: Expression Symbol -> Expression Symbol
simplify = makeUnique []

insertExpr :: Symbol -> Expression Symbol -> [Symbol] -> Expression Symbol -> Expression Symbol
insertExpr r new context old@(ESymbol s)
  | r == s    = makeUnique context new
  | otherwise = old
insertExpr r new context (EExpr a b) = EExpr (insertExpr r new context a) (insertExpr r new context b)
insertExpr r new context (ELambda l e) = ELambda l (insertExpr r new (l : context) e)

apply :: Expression Symbol -> Expression Symbol
apply (EExpr (ELambda s l) b) = insertExpr s b [] l -- [], not [s]
apply (EExpr a@(EExpr _ _) b) = EExpr (apply a) b
apply e = e

step = simplify . apply

-- EXPERIMENT ZONE START --
letters = ['a'..'z']
namesN :: Int -> [String]
namesN 0 = [""]
namesN n = [n ++ [l] | n <- namesN (n-1), l <- letters]
names :: [String]
names = concatMap namesN [1..]
findNewName :: [Symbol] -> Symbol
findNewName other =
  let bases = map symBase other
      freeBases = names \\ (nub bases)
  in  Symbol (head freeBases) 0
simplifyExpr_ :: [(Symbol, Symbol)] -> [Symbol] -> Expression Symbol -> Expression Symbol
simplifyExpr_ mapping context (ESymbol s) = ESymbol $ fromMaybe (findNewName context) $ lookup s mapping
simplifyExpr_ mapping context (EExpr a b) = EExpr (simplifyExpr_ mapping context a) (simplifyExpr_ mapping context b)
simplifyExpr_ mapping context (ELambda l e) =
  let newl = findNewName context
      newmapping = (l, newl) : mapping
      newcontext = newl : context
  in  ELambda newl (simplifyExpr_ newmapping newcontext e)
makeUnique_ :: [Symbol] -> Expression Symbol -> Expression Symbol
makeUnique_ = simplifyExpr_ []
simplify_ :: Expression Symbol -> Expression Symbol
simplify_ = makeUnique_ []
insertExpr_ :: Symbol -> Expression Symbol -> [Symbol] -> Expression Symbol -> Expression Symbol
insertExpr_ r new context old@(ESymbol s)
  | r == s    = makeUnique_ context new
  | otherwise = old
insertExpr_ r new context (EExpr a b) = EExpr (insertExpr_ r new context a) (insertExpr_ r new context b)
insertExpr_ r new context (ELambda l e) = ELambda l (insertExpr_ r new (l : context) e)
apply_ :: Expression Symbol -> Expression Symbol
apply_ (EExpr (ELambda s l) b) = insertExpr_ s b [] l -- [], not [s]
apply_ (EExpr a@(EExpr _ _) b) = EExpr (apply_ a) b
apply_ e = e
step_ = simplify_ . apply_
-- EXPERIMENT ZONE END --

_s :: String -> Expression Symbol
_s s = ESymbol $ _ss s

_ss :: String -> Symbol
_ss s = Symbol s 0

main = do
  putStrLn "Testing: Showing expressions"
  print (EExpr (_s "a") (EExpr (_s "b") (_s "c")))
  print (EExpr (_s "a") (EExpr (_s "b") (EExpr (_s "c") (_s "d"))))
  print (EExpr (EExpr (_s "a") (_s "b")) (_s "c"))
  print (EExpr (EExpr (EExpr (_s "a") (_s "b")) (_s "c")) (_s "d"))
  print (EExpr (EExpr (_s "a") (EExpr (_s "b") (_s "c"))) (_s "d"))
  print (ELambda (Symbol "a" 0) (_s "a"))
  print (ELambda (Symbol "a" 0) (EExpr (EExpr (_s "a") (EExpr (_s "b") (_s "c"))) (_s "d")))
  putStrLn ""
  
  -- test of findName (seems to be working) ~G
  putStrLn "Testing: Finding new symbols"
  print $ findName [(Symbol "a" 0), (Symbol "b" 0), (Symbol "a" 1)] (Symbol "a" 4)
  print $ findName [(Symbol "a" 0), (Symbol "b" 0), (Symbol "a" 1)] (Symbol "b" 3)
  print $ findName [(Symbol "a" 0), (Symbol "b" 0), (Symbol "a" 1)] (Symbol "c" 2)
  print $ findName [(Symbol "a" 1), (Symbol "a" 3), (Symbol "a" 0)] (Symbol "a" 1)
-- EXPERIMENT ZONE START --
  print $ findNewName [(Symbol "a" 0), (Symbol "b" 0), (Symbol "a" 1)]
  print $ findNewName [(Symbol "a" 0), (Symbol "b" 0), (Symbol "a" 1)]
  print $ findNewName [(Symbol "a" 0), (Symbol "b" 0), (Symbol "a" 1)]
  print $ findNewName [(Symbol "a" 1), (Symbol "a" 3), (Symbol "a" 0)]
-- EXPERIMENT ZONE END --
  putStrLn ""
  
  putStrLn "Testing: Applying expressions"
  let t = (ELambda (_ss "a") (ELambda (_ss "b") (ESymbol (_ss "a"))))
      f = (ELambda (_ss "a") (ELambda (_ss "b") (ESymbol (_ss "b"))))
      n = (ELambda (_ss "a") (EExpr (EExpr (ESymbol (_ss "a")) (makeUnique [(_ss "a")] f)) (makeUnique [(_ss "a")] t)))
-- EXPERIMENT ZONE START --
      n_ = (ELambda (_ss "a") (EExpr (EExpr (ESymbol (_ss "a")) (makeUnique_ [(_ss "a")] f)) (makeUnique_ [(_ss "a")] t)))
-- EXPERIMENT ZONE END --
  print t
  print f
  print n
  print $ simplify $ (ELambda (Symbol "a" 1) (ESymbol (Symbol "a" 1)))
  print (EExpr n f)
  putStrLn $ showTopLevel (EExpr n f)
  putStrLn ""
  
  putStrLn "Replacing symbols: apostrophe"
  putStrLn "Running: not false"
  print (EExpr n f)
  print . step $ (EExpr n f)
  print . step . step $ (EExpr n f)
  print . step . step . step $ (EExpr n f)
  
  putStrLn "Running: not true"
  print (EExpr n t)
  print . step $ (EExpr n t)
  print . step . step $ (EExpr n t)
  print . step . step . step $ (EExpr n t)
  putStrLn ""
  
-- EXPERIMENT ZONE START --
  putStrLn "Replacing symbols: new base"
  putStrLn "Running: not false"
  print (EExpr n_ f)
  print . step_ $ (EExpr n_ f)
  print . step_ . step_ $ (EExpr n_ f)
  print . step_ . step_ . step_ $ (EExpr n_ f)
  
  putStrLn "Running: not true"
  print (EExpr n_ t)
  print . step_ $ (EExpr n_ t)
  print . step_ . step_ $ (EExpr n_ t)
  print . step_ . step_ . step_ $ (EExpr n_ t)
  putStrLn ""
-- EXPERIMENT ZONE END --
