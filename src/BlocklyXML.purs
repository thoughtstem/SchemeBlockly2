module BlocklyXML where

import Prelude 

import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.Expr
import Text.Parsing.Parser.String

import Data.Show
import Data.String (toUpper)
import Data.List
import Data.String as S

import Control.Alt ((<|>))



data Blockly =
  Blockly (List Block)

data Block = 
  Block BlockAttributes (List Field) (List Value)

data BlockAttributes = 
  BlockType String 

data MutationAttributes =
  MutationItems Int 

data Field = 
  Field FieldAttributes String

data FieldAttributes =
  FieldName String

data Value = 
  Value ValueAttributes Block

data ValueAttributes = 
  ValueName String



type SParser a = Parser String a

symbol :: SParser Char
symbol = oneOf $ S.toCharArray "!#$%&|*+-/:=?@^_~"

digit :: SParser Char
digit = oneOf $ S.toCharArray "0123456789"

letter :: SParser Char
letter = oneOf $ S.toCharArray "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

listToString :: List Char -> String
listToString l = foldl (<>) "" $ S.singleton <$> l

beginOpenTag :: String -> SParser String
beginOpenTag s = do
  _ <- char '<'
  _ <- whiteSpace
  x <- string s
  _ <- whiteSpace
  pure $ x

endOpenTag :: SParser Char
endOpenTag = do
  _ <- char '>'
  pure '>'

closeTag :: String -> SParser String
closeTag s = do
  _ <- string "</"
  _ <- whiteSpace
  x <- string s
  _ <- whiteSpace
  _ <- string ">"
  pure $ x

attributeNamed :: String -> SParser String
attributeNamed s = do
  _ <- string s
  _ <- char '=' 
  quote <- char '"' <|> char '\''
  val <- many $ letter <|> digit <|> symbol
  _ <- char quote
  pure $ listToString val

padded :: forall a. SParser a -> SParser a
padded p = do
  _ <- whiteSpace
  x <- p
  _ <- whiteSpace
  pure x



mutationTag :: SParser String
mutationTag = do
  _ <- beginOpenTag "mutation"
  _ <- attributeNamed "items" 
  _ <- endOpenTag 
  _ <- closeTag "mutation"
  pure "mutation"

fieldTag :: SParser Field
fieldTag = do
  _ <- beginOpenTag "field"
  name <- attributeNamed "name" 
  _ <- endOpenTag 
  text <- many $ letter <|> digit <|> char ' ' <|> char '\n'
  _ <- closeTag "field"
  pure $ Field (FieldName name) (listToString text)

valueTag :: SParser Value
valueTag = do
  _ <- beginOpenTag "value"
  name <- attributeNamed "name" 
  _ <- endOpenTag 
  b <- padded blockTag 
  _ <- closeTag "value"
  pure $ Value (ValueName name) b

blockTag :: SParser Block
blockTag = do
  _ <- beginOpenTag "block"
  btype <- attributeNamed "type" 
  _ <- endOpenTag 
  _ <- padded mutationTag
  fs <- option Nil $ many $ try $ padded fieldTag 
  vs <- option Nil $ many $ try $ padded valueTag 
  _ <- padded $ closeTag "block"
  pure $ Block (BlockType btype) fs vs


blocklyParser :: SParser Blockly
blocklyParser = do
  _ <- beginOpenTag "xml"
  _ <- endOpenTag 
  bs <- padded $ many $ try $ padded blockTag
  _ <- padded $ closeTag "xml"
  pure $ Blockly bs





indentation = "  "

pad 0   = ""
pad n   = indentation <> (pad $ n-1)

joinN l = foldl (\f s -> f <> "\n" <> s) "" l

instance prettyShowBlocklyXML :: Show Blockly where
  show blockly = showBlocklyXML 0 blockly

instance prettyShowBlockXML :: Show Block where
  show block = showBlockXML 0 block

instance prettyShowFieldXML :: Show Field where
  show field = showFieldXML 0 field

instance prettyShowValueXML :: Show Value where
  show value = showValueXML 0 value

showBlocklyXML padding (Blockly children) = 
    let inner = joinN ((showBlockXML $ padding+1) <$> children) in
        "<xml>" <> inner <> "</xml>"

showBlockXML padding (Block (BlockType t) fields values) = 
  let mutationXML = (pad $ padding+1) <> "<mutation items=\"" <> (show (length values)) <> "\"></mutation>\n" in
    let fieldsXML = joinN ((showFieldXML $ padding+1) <$> fields) in
      let valuesXML = joinN ((showValueXML $ padding+1) <$> values) in
        (pad padding) <> "<block type=\"" <> t <> "\">\n" <> 
             mutationXML <> 
             fieldsXML <> 
             valuesXML <> 
        (pad padding) <> "</block>\n"

showFieldXML padding (Field (FieldName n) text) = 
      (pad padding) <> "<field name=\"" <> n <> "\">" <> text <> "</field>\n"

showValueXML padding (Value (ValueName n) block) = 
      (pad padding) <> "<value name=\"" <> n <> "\">\n" <> 
                           (showBlockXML $ padding+1) block <> 
      (pad padding) <> "</value>\n"




operand n b = Value (ValueName $ "OPERAND" <> (show n)) b

operands_ Nil n    = Nil
operands_ (b:bs) n = (operand n b) : (operands_ bs $ n+1)

operands bs = operands_ bs 0

block btype children = 
  let bs = operands children in
    Block (BlockType btype) Nil $ bs




number_block n =
   Block (BlockType "math_number") (Field (FieldName "NUM") (show n) : Nil) Nil

text_block t =
   Block (BlockType "text") (Field (FieldName "TEXT") t : Nil) Nil

bool_block b =
   Block (BlockType "logic_boolean") (Field (FieldName "BOOL") (toUpper $ show b) : Nil) Nil

circle size mode color =
  let bs = (number_block size : text_block mode : text_block color : Nil) in
    block "circle" bs




circle_example = circle 40 "solid" "red"

above_example = 
  let circles = (circle_example : circle_example : Nil) in
    Blockly $ (block "above" circles) : Nil
    


