module BlocklyXML where

import Prelude 

import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.Expr
import Text.Parsing.Parser.String

import Data.Show
import Data.Either
import Data.Maybe
import Data.String (toUpper)
import Data.List
import Data.String as S

import Control.Alt ((<|>))

import TypedLisp 



data Blockly =
  Blockly (List Block)

data Block = 
  Block BlockAttributes (List Field) (List Value) Shadowness

shadowize (Block as fs vs _) = (Block as fs vs true)

type Shadowness = Boolean

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
  val <- many (noneOf ['"'])
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
  _ <- padded $ attributeNamed "items" 
  btype <- padded $ attributeNamed "block_type" 
  _ <- padded $ attributeNamed "color" 
  _ <- padded $ attributeNamed "value" 
  _ <- endOpenTag 
  _ <- closeTag "mutation"
  pure btype

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
  b <- optionMaybe $ try $ padded blockTag  --Could be a shadow block
  b2 <- optionMaybe $ try $ padded blockTag --If so, this is the real block
  _ <- closeTag "value"
  case b of 
      Nothing -> case b2 of
                      Nothing ->  fail "Expected to find blocks in the value tag"
                      Just x2 ->  fail "This cannot happen.  If there is a second one, there must be a first."
      Just x1 -> case b2 of
                      Nothing ->  pure $ Value (ValueName name) x1
                      Just x2 ->  pure $ Value (ValueName name) x2


blockTag :: SParser Block
blockTag = do
  tag <- (try $ beginOpenTag "block") <|> (try $ beginOpenTag "shadow")
  _ <- padded $ attributeNamed "type" 
  _ <- option "" $ try $ padded $ attributeNamed "id" 
  _ <- option "" $ try $ padded $ attributeNamed "x" 
  _ <- option "" $ try $ padded $ attributeNamed "y" 
  _ <- endOpenTag 
  btype <- padded mutationTag
  fs <- option Nil $ many $ try $ padded fieldTag 
  vs <- option Nil $ many $ try $ padded valueTag 
  _ <- padded $ closeTag tag
  pure $ Block (BlockType btype) fs vs (tag == "shadow")


blocklyParser :: SParser Blockly
blocklyParser = do
  _ <- beginOpenTag "xml"
  _ <- option "" $ try $ attributeNamed "xmlns" 
  _ <- endOpenTag 
  bs <- padded $ many $ padded blockTag
  _ <- padded $ closeTag "xml"
  pure $ Blockly bs


parseBlockly s = runParser s blocklyParser



indentation = "  "

pad 0   = ""
pad n   = indentation <> (pad $ n-1)

join Nil     c = ""
join (x:Nil) c = x
join (x:xs)  c = x <> c <> (joinN xs)

joinN ls = join ls "\n"
joinS ls = join ls " "

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

showBlockXML padding (Block (BlockType t) fields values shadowness) = 
  let mutationXML = (pad $ padding+1) <> mutationXMLForBlock (Block (BlockType t) fields values shadowness) in
    let fieldsXML = joinN ((showFieldXML $ padding+1) <$> fields) in
      let valuesXML = joinN ((showValueXML $ padding+1) <$> values) in
          let tag = if shadowness then "shadow" else "block" in
            (pad padding) <> "<"<>tag<>" type=\"super_block\">\n" <> 
                 mutationXML <> 
                 fieldsXML <> 
                 valuesXML <> 
            (pad padding) <> "</"<>tag<>">\n"


mutationXMLForBlock (Block (BlockType t) fields values shadowness) = 
  "<mutation " <> (joinS attrs) <> ">" <> "</mutation>"  where
     attrs = (attrString "items"      $ show $ length values) :
             (attrString "block_type" $ t) :
             (attrString "color" $ show $ colorForBlockType defaultFunctionDefinitions t) :
             (attrString "value" $ valueFromBlock (Block (BlockType t) fields values shadowness) ) :
             Nil


colorForBlockType defs fname =
  case lookupDefinition defs fname of
    Nothing  -> 0
    Just def -> case getFunctionReturnType def of
      NumberType  -> 230 
      StringType  -> 160 
      BooleanType -> 210
      ImageType -> 290

valueFromBlock (Block (BlockType _) Nil _ _) = ""
valueFromBlock (Block (BlockType _) (f:fs) _ _) = valueFromField f

valueFromField (Field _ s) = s


  
attrString k v = k <> "=\"" <> v <> "\""

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
    Block (BlockType btype) Nil bs false




number_block n =
   Block (BlockType "math_number") (Field (FieldName "NUM") (show n) : Nil) Nil false

text_block t =
   Block (BlockType "text") (Field (FieldName "TEXT") t : Nil) Nil false 

bool_block b =
   Block (BlockType "logic_boolean") (Field (FieldName "BOOL") (toUpper $ show b) : Nil) Nil false

circle size mode color =
  let bs = (number_block size : text_block mode : text_block color : Nil) in
    block "circle" bs




circle_example = circle 40 "solid" "red"

above_example = 
  let circles = (circle_example : circle_example : Nil) in
    Blockly $ (block "above" circles) : Nil
    
example_shadow_xml1 = "<shadow type=\"super_block\" id=\"oR.vO!{FqD+RcZ8[#mmE\"><mutation items=\"0\" block_type=\"math_number\" color=\"230\" value=\"0\"></mutation><field name=\"NAME\">0</field></shadow>"

example_block_xml = "<block type=\"super_block\" id=\"j7aYZt@$F/fU6oXV`0E^\" x=\"9\" y=\"3\"><mutation items=\"2\" block_type=\"plus\" color=\"230\" value=\"undefined\"></mutation><field name=\"NAME\">plus</field><value name=\"OPERAND0\">"<>example_shadow_xml1<>"<block type=\"super_block\" id=\"Y.GH%G~yZop1.uKc:8HS\"><mutation items=\"2\" block_type=\"plus\" color=\"230\" value=\"undefined\"></mutation><field name=\"NAME\">plus</field><value name=\"OPERAND0\"><shadow type=\"super_block\" id=\"Wb(LU_uZU1bos,l;s]l?\"><mutation items=\"0\" block_type=\"math_number\" color=\"230\" value=\"0\"></mutation><field name=\"NAME\">0</field></shadow></value><value name=\"OPERAND1\"><shadow type=\"super_block\" id=\"li:=EkpPc#?f~$D!s#!}\"><mutation items=\"0\" block_type=\"math_number\" color=\"230\" value=\"0\"></mutation><field name=\"NAME\">0</field></shadow></value></block></value><value name=\"OPERAND1\"><shadow type=\"super_block\" id=\"Gra$;U4JPH8[xu$|Y:dy\"><mutation items=\"0\" block_type=\"math_number\" color=\"230\" value=\"0\"></mutation><field name=\"NAME\">0</field></shadow></value></block>"

example_blockly_xml = "<xml xmlns=\"http://www.w3.org/1999/xhtml\">"<>example_block_xml<>"</xml>"

