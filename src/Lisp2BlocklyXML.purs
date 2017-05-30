module Lisp2BlocklyXML where

import Prelude 
import Lisp
import BlocklyXML

import Data.List hiding (span)
import Data.Int (fromString)
import Data.Maybe

lispToBlocklyXML :: LispVal -> Blockly
lispToBlocklyXML v = Blockly $ (lispToBlockXML v : Nil)

lispToBlockXML :: LispVal -> Block
lispToBlockXML (List (Atom fname : rest)) = block fname (lispToBlockXML <$> rest)  
lispToBlockXML (String s)                 = text_block s
lispToBlockXML (Int i)                    = number_block i
lispToBlockXML (List bs)                  = block "nope" Nil
lispToBlockXML (Atom a)                   = text_block a
lispToBlockXML (Bool b)                   = bool_block b


blocklyXMLToLisp :: Blockly -> LispVal
blocklyXMLToLisp (Blockly (b:Nil)) = blockXMLToLisp b
blocklyXMLToLisp (Blockly bs)      = List $ blockXMLToLisp <$> bs


blockXMLToLisp :: Block -> LispVal
blockXMLToLisp (Block (BlockType "text") (Field (FieldName "TEXT") t : Nil) Nil) = String t
blockXMLToLisp (Block (BlockType "math_number") (Field (FieldName "NUM") i : Nil) Nil) = Int $ str2num i
blockXMLToLisp (Block (BlockType "logic_boolean") (Field (FieldName "BOOL") "TRUE" : Nil) Nil) = Bool true
blockXMLToLisp (Block (BlockType "logic_boolean") (Field (FieldName "BOOL") "FALSE" : Nil) Nil) = Bool false
blockXMLToLisp (Block (BlockType btype) Nil vs) = List $ (Atom btype) : (map blockXMLToLisp $ valueToBlock <$> vs)
blockXMLToLisp _ = Atom "nope"

valueToBlock :: Value -> Block
valueToBlock (Value attr b) = b


str2num :: String -> Int
str2num s = fromStringOrZero $ fromString s

fromStringOrZero :: Maybe Int -> Int
fromStringOrZero (Just s) = s
fromStringOrZero _        = 0
