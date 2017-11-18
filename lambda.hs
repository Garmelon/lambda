import Data.List

{-
 - The Expression type
 -}

data Expression s = ESymbol s
                  | EReference Int
                  | EExpr (Expression s) (Expression s)
                  | ELambda s (Expression s)

-- list of names -> name -> unique name
-- Find a unique name not in the list of names.
-- Appends apostrophes until name is unique.
makeUnique :: [String] -> String -> String
makeUnique context s =
  let apostrophes = iterate ('\'' :) ""
      modified = zipWith (++) (repeat s) apostrophes
      available = filter (not . (`elem` context)) modified
  in head available -- list of available names is infinite

-- Expression -> Expression with unique symbol names
renameUniquely :: (Show s) => Expression s -> Expression String
renameUniquely = rename_ []
  -- rename_ :: (Show s) => [String] -> Expression s -> Expression String
  where rename_ c (EReference r) = EReference r
        rename_ c (ESymbol s)    = ESymbol . makeUnique c . show $ s
        rename_ c (EExpr a b)    = EExpr (rename_ c a) (rename_ c b)
        rename_ c (ELambda s e)  = let name = makeUnique c . show $ s
                                   in  ELambda name (rename_ (name : c) e)

instance (Show s) => Show (Expression s) where
  show = show_ [] . renameUniquely
    -- show_ :: Expression String -> String
    where show_ c (EReference n)
            | n >= 0 && n < length c = c !! n
            | otherwise              = "ERR" -- TODO: Deal with errors properly?
          show_ c (ESymbol s)                             = s
          show_ c (ELambda s e)                           = "\\" ++ s ++ "."  ++ show_ (s : c) e
          show_ c (EExpr a@(ELambda _ _) b@(ELambda _ _)) = "(" ++ show_ c a ++ ") (" ++ show_ c b ++ ")"
          show_ c (EExpr a@(ELambda _ _) b@(EExpr   _ _)) = "(" ++ show_ c a ++ ") (" ++ show_ c b ++ ")"
          show_ c (EExpr a@(ELambda _ _) b              ) = "(" ++ show_ c a ++ ") "  ++ show_ c b
          show_ c (EExpr a               b@(ELambda _ _)) =        show_ c a ++  " (" ++ show_ c b ++ ")"
          show_ c (EExpr a               b@(EExpr   _ _)) =        show_ c a ++  " (" ++ show_ c b ++ ")"
          show_ c (EExpr a               b)               =        show_ c a ++  " "  ++ show_ c b

instance (Eq s) => Eq (Expression s) where
  (ESymbol a)    == (ESymbol b)    = a == b
  (EReference a) == (EReference b) = a == b
  (EExpr a b)    == (EExpr c d)    = a == c && b == d
  (ELambda _ a)  == (ELambda _ b)  = a == b
  _              == _              = False

{-
 - Evaluating expressions
 -}

-- Expression to be inserted -> Expression to be inserted into -> result
-- TODO: Make clearer what insert does - better description, ...
insertExpr :: Int -> Expression s -> Expression s -> Expression s
insertExpr level replace ref@(EReference n)
  | n == level = replace
  | otherwise  = ref
insertExpr level replace (EExpr a b)   = EExpr (insertExpr level replace a)
                                               (insertExpr level replace b)
insertExpr level replace (ELambda s e) = ELambda s (insertExpr (level + 1) replace e)
insertExpr _     _       symbol        = symbol

apply :: Expression s -> Expression s
apply (EExpr l@(ELambda s e) b) = insertExpr 0 b e
apply (EExpr e@(EExpr _ _) b) = EExpr (apply e) b
apply e = e

takeWhileUnique :: (Eq a) => [a] -> [a]
takeWhileUnique l = map fst
                  $ takeWhile (\a -> not $ fst a `elem` snd a)
                  $ zip l (inits l)

evaluate :: (Eq s) => Expression s -> [Expression s]
evaluate = takeWhileUnique . iterate apply

{-
 - Parsing expressions
 -}

-- TODO



-- Helper type for using arbitrary strings as symbols
newtype StrSymbol = StrSymbol String

instance Show StrSymbol where
  show (StrSymbol s) = s

instance Eq StrSymbol where
  (StrSymbol a) == (StrSymbol b) = a == b

--instance Read StrSymbol where
--  read s = StrSymbol s

_s = ESymbol
_e = EExpr
_r = EReference
_l = ELambda
_ss = StrSymbol

main = do
  putStrLn "Test nested expressions and parentheses"
  print (_e (_e (_s 1) (_s 2)) (_e (_s 3) (_s 4)))
  print (_e (_e (_s 1) (_e (_s 2) (_s 3))) (_s 4))
  print (_e (_e (_l 1 (_r 0)) (_e (_l 2 (_r 0)) (_l 3 (_r 0)))) (_l 4 (_r 0)))
  putStrLn "Test references and symbols in lambda expressions"
  print (_l 5 (_l 2 (_e (_s 3) (_r 0))))
  print (_l 5 (_l 2 (_e (_s 3) (_r 1))))
  print (_l 5 (_l 2 (_e (_s 3) (_r 2)))) -- should fail in some way
  putStrLn "More reference tests"
  print (_l 1 (_e (_l 2 (_r 0)) (_l 3 (_r 1))))
  print ((_r 0) :: Expression Int) -- should also fail in some way
  putStrLn "Test insertion"
  putStrLn "Testing Ints as symbols..."
  let t = (_l 1 (_l 2 (_r 1)))
      f = (_l 1 (_l 2 (_r 0)))
      n = (_l 1 (_e (_e (_r 0) f) t))
  print t
  print f
  print n
  putStrLn "Evaluating... N T"
  mapM_ print . evaluate $ (_e n t)
  putStrLn "Evaluating... N F"
  mapM_ print . evaluate $ (_e n f)
  putStrLn "Testing StrSymbols as symbols..."
  let st = (_l (_ss "a") (_l (_ss "b") (_r 1)))
      sf = (_l (_ss "a") (_l (_ss "b") (_r 0)))
      sn = (_l (_ss "n") (_e (_e (_r 0) sf) st))
  print st
  print sf
  print sn
  putStrLn "Evaluating... N T"
  mapM_ print . evaluate $ (_e sn st)
  putStrLn "Evaluating... N F"
  mapM_ print . evaluate $ (_e sn sf)
