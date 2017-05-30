module Test.BlocklyXML where

import Prelude

import Text.Parsing.Parser

import Test.Assert
import BlocklyXML

import Data.Either
import Data.List


blockly_xml_test1 = 
  let orig = above_example in
    let parsed = parse (show orig) in
      do
        assert $ show orig == show parsed
 
parse s = 
  let parsed = runParser s blocklyParser in
    case parsed of  
      Right s -> s
      Left  e -> Blockly Nil 
