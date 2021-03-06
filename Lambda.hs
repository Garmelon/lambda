module Lambda
( Expression
, display
, parseMaybe
, evaluate
) where

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
  deriving (Show)

-- comparison checks for alpha-equivalency
instance (Eq s) => Eq (Expression s) where
  (ESymbol a)    == (ESymbol b)    = a == b
  (EReference a) == (EReference b) = a == b
  (EExpr a b)    == (EExpr c d)    = a == c && b == d
  (ELambda _ a)  == (ELambda _ b)  = a == b
  _              == _              = False

-- Map over all the symbols in the expression (including those in the ELambdas).
instance Functor Expression where
  fmap f (EReference r) = EReference r
  fmap f (ESymbol a)    = ESymbol (f a)
  fmap f (ELambda s e)  = ELambda (f s) (fmap f e)
  fmap f (EExpr a b)    = EExpr (fmap f a) (fmap f b)

{-
 - Displaying expressions
 -}

-- list of names -> name -> unique name
-- Find a unique name not in the list of names.
-- Appends apostrophes until name is unique.
makeUnique :: [String] -> String -> String
makeUnique context s =
  let apostrophes = iterate ('\'' :) ""
      modified    = zipWith (++) (repeat s) apostrophes
      available   = filter (not . (`elem` context)) modified
  in head available -- re. head: list of available names is infinite

-- Expression -> Expression with unique symbol names
-- Converts all the symbols in the expression to strings and
-- then makes them unique in their context.
renameUniquely :: (Show s) => Expression s -> Expression String
renameUniquely = rename_ []
  -- rename_ :: (Show s) => [String] -> Expression s -> Expression String
  where rename_ c (EReference r) = EReference r
        rename_ c (ESymbol s)    = ESymbol . makeUnique c . show $ s
        rename_ c (EExpr a b)    = EExpr (rename_ c a) (rename_ c b)
        rename_ c (ELambda s e)  = let name = makeUnique c . show $ s
                                   in  ELambda name (rename_ (name : c) e)

-- unicode? -> Expression -> Maybe String
-- Converts the expression to a string, if possible.
-- (Not possible if a reference is too high)
-- If unicode is True, uses λ instead of \.
display :: (Show s) => Bool -> Expression s -> String
display unicode = d [] . renameUniquely
  where l = if unicode then "λ" else "\\"
        d c (EReference n)
          | n >= 0 && n < length c = c !! n
          | otherwise              = "ERR"
        d c (ESymbol s) = s
        d c (ELambda s e@(ELambda _ _)) = l ++ s ++ " " ++ (tail $ d (s : c) e)
        d c (ELambda s e)               = l ++ s ++ "." ++         d (s : c) e
        d c (EExpr a@(ELambda _ _) b@(ELambda _ _)) = "(" ++ d c a ++ ") (" ++ d c b ++ ")"
        d c (EExpr a@(ELambda _ _) b@(EExpr _ _)  ) = "(" ++ d c a ++ ") (" ++ d c b ++ ")"
        d c (EExpr a               b@(ELambda _ _)) =        d c a ++  " (" ++ d c b ++ ")"
        d c (EExpr a               b@(EExpr _ _)  ) =        d c a ++  " (" ++ d c b ++ ")"
        d c (EExpr a               b              ) =        d c a ++  " "  ++ d c b

{-
 - Evaluating expressions
 -}

-- level to insert at -> Expression to be inserted
-- -> Expression to be inserted into -> result
-- Inserts an expression into another expression on a certain level.
-- The level points towards an ELambda, like an EReference.
insertExpr :: Int -> Expression s -> Expression s -> Expression s
insertExpr level replace ref@(EReference n)
  | n == level = replace
  | otherwise  = ref
insertExpr level replace (EExpr a b)   = EExpr (insertExpr level replace a)
                                               (insertExpr level replace b)
insertExpr level replace (ELambda s e) = ELambda s (insertExpr (level + 1) replace e)
insertExpr _     _       other         = other

-- Expression -> Expression applied once
-- Attempts beta-reduction, applying the leftmost expression (if there is one) to
-- the expression to its right (if there is one).
-- TODO: Apply leftmost function, not leftmost expression.
apply :: Expression s -> Expression s
apply (EExpr l@(ELambda s e) b) = insertExpr 0 b e
apply (EExpr e@(EExpr _ _) b) = EExpr (apply e) b
apply e = e

-- Takes elements as long as it hasn't seen them before.
takeWhileUnique :: (Eq a) => [a] -> [a]
takeWhileUnique l = map fst
                  $ takeWhile (\a -> not $ fst a `elem` snd a)
                  $ zip l (inits l)

-- Applies a function until there is a loop or it can't be applied any further.
evaluate :: (Eq s) => Expression s -> [Expression s]
evaluate = takeWhileUnique . iterate apply

{-
 - Parsing expressions
 -}

-- "munchified" versions of many, many1 and chainl1 that try to match as much as
-- possible. They only match the maximum amount, not all numbers in-between.
many' :: ReadP a -> ReadP [a]
many' p = many1' p <++ return []

many1' :: ReadP a -> ReadP [a]
many1' p = liftM2 (:) p (many' p)

chainl1' :: ReadP a -> ReadP (a -> a -> a) -> ReadP a
chainl1' p f = foldl1 <$> f <*> many1' p

-- List of (opening parenthesis, closing parenthesis) pairs.
parens :: [(Char, Char)]
parens = [ ('(', ')')
         , ('[', ']')
         , ('{', '}')
         ]

-- Is opening parenthesis?
isOpeningParen :: Char -> Bool
isOpeningParen a = isJust $ lookup a parens

-- Is closing parenthesis to this opening parenthesis?
isClosingParen :: Char -> Char -> Bool
isClosingParen a b = fromMaybe False $ (== b) <$> lookup a parens

-- Put a pair of matching parentheses around a parser
parenthesize :: ReadP a -> ReadP a
parenthesize parser = do
  paren <- satisfy isOpeningParen
  result <- parser
  satisfy $ isClosingParen paren
  return result

parseSymbolStr :: ReadP String
parseSymbolStr = do
  a <- munch1 (`elem` ['a'..'z'])
  b <- munch (== '\'')
  return $ a ++ b

parseSymbol :: ReadP (Expression String)
parseSymbol = parseSymbolStr >>= (return . ESymbol)

parseLambda :: ReadP (Expression String)
parseLambda = do
  char '\\' +++ char 'λ'
  symbols <- many1' $ between skipSpaces skipSpaces parseSymbolStr
  char '.'
  e <- parseExpr
  return $ foldr ELambda e symbols

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
        find_ context (ELambda s e)      = ELambda s $ find_ (s : context) e
        find_ context (EExpr a b)        = EExpr (find_ context a) (find_ context b)

removeApostrophes :: String -> String
removeApostrophes = reverse . dropWhile (== '\'') . reverse

parseMaybe :: String -> Maybe (Expression String)
parseMaybe s = do
  let results = readP_to_S parseExpr s
  (expr, _) <- safeLast results
  return . fmap removeApostrophes . findReferences $ expr
    where safeLast [] = Nothing
          safeLast xs = Just $ last xs
