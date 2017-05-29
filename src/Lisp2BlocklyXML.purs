module Lisp2BlocklyXML where

import Prelude 
import Lisp
import BlocklyXML

import Data.List hiding (span)

lispToBlocklyXML :: LispVal -> Blockly
lispToBlocklyXML v = Blockly $ (lispToBlockXML v : Nil)

lispToBlockXML :: LispVal -> Block
lispToBlockXML (List (Atom fname : rest)) = block fname (lispToBlockXML <$> rest)  
lispToBlockXML (String s)                 = text_block "text" 
lispToBlockXML (Int i)                    = number_block i
lispToBlockXML _                          = block "nope" Nil


