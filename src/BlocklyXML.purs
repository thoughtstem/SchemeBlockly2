module BlocklyXML where

import Prelude 

import Data.Show
import Data.List

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


indentation = "  "

pad 0   = ""
pad n   = indentation <> (pad $ n-1)

joinN l = foldl (\f s -> f <> "\n" <> s) "" l

instance prettyShowBlocklyXML :: Show Blockly where
  show blockly = showBlocklyXML 0 blockly


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
   Block (BlockType "text") (Field (FieldName "NUM") t : Nil) Nil

circle size mode color =
  let bs = (number_block size : text_block mode : text_block color : Nil) in
    block "circle" bs




circle_example = circle 40 "solid" "red"

above_example = 
  let circles = (circle_example : circle_example : Nil) in
    Blockly $ (block "above" circles) : Nil
    


