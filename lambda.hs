import Data.List

data Expression s = ESymbol s
                  | EReference Int
                  | EExpr (Expression s) (Expression s)
                  | ELambda s (Expression s)

makeUnique :: [String] -> String -> String
makeUnique context s =
  let apostrophes = iterate ('\'' :) ""
      modified = zipWith (++) (repeat s) apostrophes
      available = filter (not . (`elem` context)) modified
  in head available

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
            | otherwise              = "ERR" -- TODO: Do this better?
          show_ c (ESymbol s)               = s
          show_ c (EExpr a b@(EExpr _ _))   = show_ c a ++ " (" ++ show_ c b ++ ")"
          show_ c (EExpr a b)               = show_ c a ++ " "  ++ show_ c b
          show_ c (ELambda s e@(EExpr _ _)) = "\\" ++ s ++ ".(" ++ show_ (s : c) e ++ ")"
          show_ c (ELambda s e)             = "\\" ++ s ++ "."  ++ show_ (s : c) e

instance Eq (Expression s) where
  (ESymbol _)    == (ESymbol _)    = True
  (EReference a) == (EReference b) = a == b
  (EExpr a b)    == (EExpr c d)    = a == c && b == d
  (ELambda _ a)  == (ELambda _ b)  = a == b
  _              == _              = False

insertExpr :: Expression s -> Expression s -> Expression s
insertExpr = insert_ 0
  where insert_ level replace ref@(EReference n)
          | n == level = replace
          | otherwise  = ref
        insert_ level replace (EExpr a b)   = EExpr (insert_ level replace a) (insert_ level replace b)
        insert_ level replace (ELambda s e) = ELambda s (insert_ (level + 1) replace e)
        insert_ _ _ symbol                  = symbol

apply :: Expression s -> Expression s
apply (EExpr l@(ELambda s e) b) = insertExpr b e
apply (EExpr e@(EExpr _ _) b) = EExpr (apply e) b
apply e = e

takeWhileUnique :: (Eq a) => [a] -> [a]
takeWhileUnique l = map fst $ takeWhile (\a -> not $ fst a `elem` snd a) $ zip l (inits l)

evaluate :: Expression s -> [Expression s]
evaluate = takeWhileUnique . iterate apply

_s = ESymbol
_e = EExpr
_r = EReference
_l = ELambda

main = do
  putStrLn "Test nested expressions and parentheses"
  print (_e (_e (_s 1) (_s 2)) (_e (_s 3) (_s 4)))
  print (_e (_e (_s 1) (_e (_s 2) (_s 3))) (_s 4))
  putStrLn "Test references and symbols in lambda expressions"
  print (_l 5 (_l 2 (_e (_s 3) (_r 0))))
  print (_l 5 (_l 2 (_e (_s 3) (_r 1))))
  print (_l 5 (_l 2 (_e (_s 3) (_r 2)))) -- should fail in some way
  putStrLn "More reference tests"
  print (_l 1 (_e (_l 2 (_r 0)) (_l 3 (_r 1))))
  print ((_r 0) :: Expression Int) -- should also fail in some way
  putStrLn "Test insertion"
  let t = (_l 1 (_l 2 (_r 1)))
      f = (_l 1 (_l 2 (_r 0)))
      n = (_l 1 (_e (_e (_r 0) f) t))
  print t
  print f
  print n
  putStrLn "Evaluating..."
  mapM_ print . evaluate $ (_e n t)
