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
lispToBlockXML (Meta d (Meta d2 v))   = lispToBlockXML (Meta d2 v)

lispToBlockXML (Meta d (String s))   = setBlockMeta d $ text_block s
lispToBlockXML (Meta d (Int i))      = setBlockMeta d $ number_block i
lispToBlockXML (Meta d (Atom a))     = setBlockMeta d $ text_block a
lispToBlockXML (Meta d (Bool b))     = setBlockMeta d $ bool_block b
lispToBlockXML (Meta d (List (Atom fname : rest))) = setBlockMeta d $ block fname (lispToBlockXML <$> rest)

lispToBlockXML (String s)   =  text_block s
lispToBlockXML (Int i)      =  number_block i
lispToBlockXML (Atom a)     =  text_block a
lispToBlockXML (Bool b)     =  bool_block b
lispToBlockXML (List (Atom fname : rest)) = block fname (lispToBlockXML <$> rest)

lispToBlockXML (List bs)             = block "can't convert any old lisp list to blockly yet" Nil 

lispToBlockXML l                     =  block "what's this" Nil



blocklyXMLToLisp :: Blockly -> LispVal
blocklyXMLToLisp (Blockly (b:Nil)) = blockXMLToLisp b
blocklyXMLToLisp (Blockly bs)      = List $ blockXMLToLisp <$> bs


blockXMLToLisp :: Block -> LispVal
blockXMLToLisp (Block d (Field _ f : Nil) Nil) = 
  case d.block_type of 
       "text"        -> Meta d $ String f
       "math_number" -> Meta d $ Int $ str2num f
       "logic_boolean" -> Meta d $ Bool $ f == "true" 
       _               -> Atom $ "couldn't convert primitve block to lisp: " <> d.block_type 

blockXMLToLisp (Block d _ vs) = Meta d $ List $ (Atom d.block_type) : (map blockXMLToLisp $ valueToBlock <$> vs)
blockXMLToLisp b = Atom $ "couldn't covert this blockly to lisp: " <> (show b)

valueToBlock :: Value -> Block
valueToBlock (Value attr b) = b


str2num :: String -> Int
str2num s = fromStringOrZero $ fromString s

fromStringOrZero :: Maybe Int -> Int
fromStringOrZero (Just s) = s
fromStringOrZero _        = 0
