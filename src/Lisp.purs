module Lisp where

import Prelude 

import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.Expr
import Text.Parsing.Parser.String

import Data.Show
import Data.Tuple
import Data.String as S
import Data.Foldable hiding (oneOf, length)
import Data.Either
import Data.Maybe
import Data.List hiding (span)
import Data.Int (fromString)
import Control.Alt ((<|>))

import Control.Lazy

import Math (abs)

type SParser a = Parser String a

symbol :: SParser Char
symbol = oneOf $ S.toCharArray "!#$%&|*+-/:<=>?@^_~"


digit :: SParser Char
digit = oneOf $ S.toCharArray "0123456789"

letter :: SParser Char
letter = oneOf $ S.toCharArray "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

readExpr :: String -> String
readExpr input = case runParser input parseExpr of
                      Left err -> "No match: " <> show err
                      Right thing -> "Found value: " <> showLispVal thing

listToString :: List Char -> String
listToString l = foldl (<>) "" $ S.singleton <$> l

parseString :: SParser LispVal
parseString = do
  _ <- char '"'
  x <- many (noneOf ['"'])
  _ <- char '"'
  pure $ String (listToString x)

parseAtom :: SParser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many $ letter <|> symbol <|> digit
  let atom = listToString $ first:rest
  pure $ case atom of
            "true" -> Bool true
            "false" -> Bool false
            _ -> Atom atom

str2num :: String -> Int
str2num s = fromStringOrZero $ fromString s

fromStringOrZero :: Maybe Int -> Int
fromStringOrZero (Just s) = s
fromStringOrZero _        = 0

parseInt :: SParser LispVal
parseInt = Int <<< str2num <<< listToString <$> many1 digit

many1 :: forall a. Parser String a -> Parser String (List a)
many1 par = do
  x <- par
  xs <- many par
  pure (x:xs)


parseList :: SParser LispVal -> SParser LispVal
parseList pars = List <$> pars `sepBy` whiteSpace

parseQuoted :: SParser LispVal -> SParser LispVal
parseQuoted pars = do
  _ <- string "'"
  x <- pars
  pure $ List $ (Atom "quote") : x : Nil

parseExpr :: SParser LispVal
parseExpr = fix $ \p -> (parseAtom
                     <|> parseString
                     <|> parseInt
                     <|> parseQuoted p
                     <|> (do
                         _ <- char '('
                         x <- try (parseList p) 
                         _ <- char ')'
                         pure x))


data LispVal = Atom String
             | List (List LispVal)
             | Int Int
             | String String
             | Bool Boolean
             | Meta LispMetaData LispVal

type LispMetaData = { 
                      block_type:: String,
                      shadowness :: Boolean,
                      x :: Int, 
                      y :: Int,
                      id :: String }



--This tuple stuff is confusing as fuck -SF

stepLispProgram_ :: Tuple Boolean LispVal -> Tuple Boolean LispVal
stepLispProgram_ (Tuple true  v) = Tuple true v
stepLispProgram_ (Tuple false v) = case v of
                                       (Meta d x) -> Tuple false $ stepLispProgram x
                                       (Atom s) -> Tuple false (Atom s)
                                       (Int i)  -> Tuple false (Int i)
                                       (String s)  -> Tuple false (String s)
                                       (Bool b)  -> Tuple false (Bool b)
                                       (List vals) -> case isSimpleList (List vals) of
                                                           true ->  Tuple true $ simpleStep $ List vals
                                                           false -> Tuple false $ List $ stepLispProgram <$> vals
                                       v         -> Tuple true v

stepLispProgram :: LispVal -> LispVal
stepLispProgram v = snd $ stepLispProgram_ $ Tuple false v

isSimpleList :: LispVal -> Boolean
isSimpleList (List xs)       = foldl (\f s-> f && (not isList s)) true xs
isSimpleList _ = false

isList (List x) = true
isList (Meta _ (List x)) = true
isList _ = false

simpleStep :: LispVal -> LispVal
simpleStep (Meta d v) = simpleStep v
simpleStep (List (Atom "plus" : other_things))  = foldOverLisp (liftOverMetas $ plusLisp2)  (Int 0) (List other_things)
simpleStep (List (Atom "minus" : x : xs))       = foldOverLisp (liftOverMetas $ minusLisp2) x (List xs)
simpleStep _ = String "Can only simple step a simple list.  And only if the function is defined..."

foldOverLisp :: (LispVal -> LispVal -> LispVal) -> LispVal -> LispVal -> LispVal
foldOverLisp f default (Meta d v)  = foldOverLisp f default v
foldOverLisp f default (List vals) = foldl f default vals
foldOverLisp f _ _= Atom "error"

liftOverMetas :: (LispVal -> LispVal -> LispVal) -> LispVal -> LispVal -> LispVal 
liftOverMetas f (Meta d1 i1) i2  = liftOverMetas f i1 i2
liftOverMetas f i1 (Meta d2 i2)  = liftOverMetas f i1 i2
liftOverMetas f l1 l2 = f l1 l2

plusLisp2 :: LispVal -> LispVal -> LispVal
plusLisp2 (Int i1) (Int i2)  = Int (i1 + i2)
plusLisp2 i1 i2              = Atom $ "Error: Couldn't add " <> (show i1) <> "+" <> (show i2)

minusLisp2 ::LispVal -> LispVal -> LispVal
minusLisp2 (Int i1) (Int i2)         = Int (i1 - i2)
minusLisp2 _ _                       = Atom "error"

example2 = parseLisp "(plus 2 (plus 2 4))" 
example3 = parseLisp "(minus 8 (minus 3 1))"


    




--diffStrings :: String -> String -> Int --0 is same.  Higher number mean more difference.
--diffStrings s1 s2 = abs $ (length s1) - (length s2) --Dumb temp algorithm.  Use levenstein... 


--lispSimilarity :: LispVal -> LispVal -> Int --0 is same.  Higher number mean more difference.
--lispSimilarity v1 v2     = diffStrings (show v1) (show v2) 

--lispSimilarity (Atom a1) (Atom a2)     = diffStrings a1 a2
--lispSimilarity (String a1) (String a2) = diffStrings a1 a2
--lispSimilarity (Int a1) (Int a2) = diffInts a1 a2
--lispSimilarity (Bool a1) (Bool a2) = if a1 == a2 then 0 else 1
--lispSimilarity (Meta a1) (Meta a2) = lispSimilarity a1 a2
--lispSimilarity (Meta a1) a2 = lispSimilarity a1 a2
--lispSimilarity a1 (Meta a2) = lispSimilarity a1 a2
--lispSimilarity (List (x1:xs1)) (List (x2:xs2)) = (lispSimilarity x1 x2) + (lispSimilarity xs1 xs2)
--lispSimilarity (List xs) (List Nil))  = length xs
--lispSimilarity (List Nil) (List xs))  = length xs
--lispSimilarity (List Nil) (List Nil)) = 0
--lispSimilarity (List xs) a2           = length xs
--lispSimilarity a2 (List xs)           = length xs





joinS Nil     = ""
joinS (x:Nil) = x
joinS (x:xs)  = x <> " " <> (joinS xs)

showLispVal :: LispVal -> String
showLispVal v = 
  case v of 
    Atom s -> s 
    List l -> "(" <> (joinS $ showLispVal <$> l) <> ")"
    Int i -> show i
    String s -> "\"" <> s <> "\""
    Bool b -> case b of
                true -> "true"
                false -> "false"
    Meta d b -> showLispVal b

lispDebugShow :: LispVal -> String
lispDebugShow v = 
  case v of 
    Atom s -> s 
    List l -> "(" <> (joinS $ lispDebugShow <$> l) <> ")"
    Int i -> show i
    String s -> "\"" <> s <> "\""
    Bool b -> case b of
                true -> "true"
                false -> "false"
    Meta d b -> "[" <> (showLispMeta d) <> "]<" <> (lispDebugShow b) <> ">"

showLispMeta { block_type:b, shadowness:s, x:x, y:y, id:id } = "{"<>(joinS values)<>"}"
  where values = (
    showMetaVal "block_type" (show b) :
    showMetaVal "shadowness" (show s) :
    showMetaVal "x" (show x) :
    showMetaVal "y" (show y) :
    showMetaVal "id" (show id) :
    Nil )

showMetaVal k v = k<>":"<>v

instance sShowLispVal :: Show LispVal where
  show l = showLispVal l

example = parseLisp "(above (circle 40 \"solid\" \"red\") (circle 40 \"solid\" \"red\"))" 


parseLisp :: String -> LispVal
parseLisp s = 
  let parsed = runParser s parseExpr in
    case parsed of
      Right v -> v 
      Left  e -> Atom (show e)

