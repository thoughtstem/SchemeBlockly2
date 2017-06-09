module Lisp2BlocklyXML where

import Prelude 
import SandScript.AST (WFF(..), LangError(..), ThrowsError)
import BlocklyXML 

import Data.List hiding (span)
import Data.Int (fromString)
import Data.Maybe

lispToBlocklyXML :: WFF -> Blockly
lispToBlocklyXML v = Blockly $ (lispToBlockXML v : Nil)

lispToBlockXML :: WFF -> Block
lispToBlockXML (String s)   =  text_block s
lispToBlockXML (Integer i)      =  number_block i
lispToBlockXML (Atom a)     =  text_block a
lispToBlockXML (Bool b)     =  bool_block b
lispToBlockXML (List (Atom fname : rest)) = block fname (lispToBlockXML <$> rest)

lispToBlockXML (List bs)             = block "can't convert any old lisp list to blockly yet" Nil 

lispToBlockXML l                     =  block "what's this" Nil



blocklyXMLToLisp :: Blockly -> WFF
blocklyXMLToLisp (Blockly (b:Nil)) = blockXMLToLisp b
blocklyXMLToLisp (Blockly bs)      = List $ blockXMLToLisp <$> bs


blockXMLToLisp :: Block -> WFF
blockXMLToLisp (Block d (Field _ f : Nil) Nil) = 
  case d.block_type of 
       "text"        ->  String f
       "math_number" ->  Integer $ str2num f
       "logic_boolean" ->  Bool $ f == "true" 
       _               -> Atom $ "couldn't convert primitve block to lisp: " <> d.block_type 

blockXMLToLisp (Block d _ vs) =  List $ (Atom d.block_type) : (map blockXMLToLisp $ valueToBlock <$> vs)
blockXMLToLisp b = Atom $ "couldn't covert this blockly to lisp: " <> (show b)

valueToBlock :: Value -> Block
valueToBlock (Value attr b) = b


str2num :: String -> Int
str2num s = fromStringOrZero $ fromString s

fromStringOrZero :: Maybe Int -> Int
fromStringOrZero (Just s) = s
fromStringOrZero _        = 0
