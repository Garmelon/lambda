import Control.Monad
import Data.List
import Data.Maybe
import Text.ParserCombinators.ReadP

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

instance Functor Expression where
  fmap f (EReference n) = EReference n
  fmap f (ESymbol a)    = ESymbol (f a)
  fmap f (ELambda s e)  = ELambda (f s) (fmap f e)
  fmap f (EExpr a b)    = EExpr (fmap f a) (fmap f b)

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

-- "munchified" versions of many, many1 and chainl1 that try to match as much as possible.
many' :: ReadP a -> ReadP [a]
many' p = many1' p <++ return []

many1' :: ReadP a -> ReadP [a]
many1' p = liftM2 (:) p (many' p)

chainl1' :: ReadP a -> ReadP (a -> a -> a) -> ReadP a
chainl1' p f = foldl1 <$> f <*> many1' p

parens :: [(Char, Char)]
parens = [('(',')'),('[',']'),('{','}')]

isOpeningParen :: Char -> Bool
isOpeningParen a = isJust $ lookup a parens

isClosingParen :: Char -> Char -> Bool
isClosingParen a b = fromMaybe False $ (==b) <$> lookup a parens

parenthesize :: ReadP a -> ReadP a
parenthesize parser = do
  paren <- satisfy isOpeningParen
  result <- parser
  satisfy $ isClosingParen paren
  return result

parseSymbol :: ReadP (Expression String)
parseSymbol = do
  a <- munch1 (`elem` ['a'..'z'])
  b <- munch (=='\'')
  return $ ESymbol (a ++ b)

parseLambda :: ReadP (Expression String)
parseLambda = do
  char '\\' +++ char 'λ'
  (ESymbol s) <- parseSymbol
  char '.'
  e <- parseExpr
  return $ ELambda s e

parseExpr :: ReadP (Expression String)
parseExpr =
  let options =   parseSymbol
              +++ parseLambda
              +++ parenthesize parseExpr
      parse = between skipSpaces skipSpaces options
  in  chainl1' parse (return EExpr)

findReferences :: (Eq s) => Expression s -> Expression s
findReferences = find_ []
  where find_ context sym@(ESymbol s)    = fromMaybe sym $ EReference <$> elemIndex s context
        find_ context ref@(EReference _) = ref
        find_ context (ELambda s e)      = ELambda s $ find_ (s:context) e
        find_ context (EExpr a b)        = EExpr (find_ context a) (find_ context b)

removeApostrophes :: String -> String
removeApostrophes = reverse . dropWhile (=='\'') . reverse

maybeParseExpression :: String -> Maybe (Expression String)
maybeParseExpression s = do
  let results = readP_to_S parseExpr s
  (expr, _) <- safeLast results
  return . fmap removeApostrophes . findReferences $ expr
    where safeLast [] = Nothing
          safeLast xs = Just $ last xs

{-
 - Interactive evaluation
 -}

-- Helper type for using arbitrary strings as symbols
newtype StrSymbol = StrSymbol String

instance Show StrSymbol where
  show (StrSymbol s) = s

instance Eq StrSymbol where
  (StrSymbol a) == (StrSymbol b) = a == b

linewise :: (String -> String) -> String -> String
linewise f = unlines . map f . lines

evaluateExpression :: String -> String
evaluateExpression s =
  let result = return
             . map show
             . evaluate
             . fmap StrSymbol
      l = fromMaybe ["Error: Could not parse expression."]
        $ maybeParseExpression s >>= result
  in  unlines l

main = interact $ linewise evaluateExpression
