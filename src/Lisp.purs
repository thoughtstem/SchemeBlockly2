module Lisp where

import Prelude 

import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.Expr
import Text.Parsing.Parser.String

import Data.Show
import Data.String as S
import Data.Foldable hiding (oneOf)
import Data.Either
import Data.Maybe
import Data.List hiding (span)
import Data.Int (fromString)
import Control.Alt ((<|>))

import Control.Lazy

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

