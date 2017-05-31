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
lispToBlockXML (List ((Shadow (Atom fname)) : rest)) = block fname (lispToBlockXML <$> rest)  
lispToBlockXML (List bs)                  = block "can't convert any old lisp list to blockly yet" Nil
lispToBlockXML (String s)                 = text_block s
lispToBlockXML (Int i)                    = number_block i
lispToBlockXML (Atom a)                   = text_block a
lispToBlockXML (Bool b)                   = bool_block b
lispToBlockXML (Shadow b)                 = shadowize $ lispToBlockXML b -- Just a wrapper, supports shadowing in blockly... Not part of the language but just for the visuals.  Probably should have implemented this diferently.


blocklyXMLToLisp :: Blockly -> LispVal
blocklyXMLToLisp (Blockly (b:Nil)) = blockXMLToLisp b
blocklyXMLToLisp (Blockly bs)      = List $ blockXMLToLisp <$> bs


blockXMLToLisp :: Block -> LispVal
blockXMLToLisp (Block (BlockType "text") (Field _ t : Nil) _ _) = String $ t
blockXMLToLisp (Block (BlockType "math_number") (Field _ i : _) _ _) = Int $ str2num i
blockXMLToLisp (Block (BlockType "logic_boolean") (Field _ "true" : Nil) _ _) = Bool true
blockXMLToLisp (Block (BlockType "logic_boolean") (Field _ "false" : Nil) _ _) = Bool false
blockXMLToLisp (Block (BlockType btype) _ vs _) = List $ (Atom btype) : (map blockXMLToLisp $ valueToBlock <$> vs)
blockXMLToLisp b = Atom $ "couldn't covert this blockly to lisp: " <> (show b)

valueToBlock :: Value -> Block
valueToBlock (Value attr b) = b


str2num :: String -> Int
str2num s = fromStringOrZero $ fromString s

fromStringOrZero :: Maybe Int -> Int
fromStringOrZero (Just s) = s
fromStringOrZero _        = 0
