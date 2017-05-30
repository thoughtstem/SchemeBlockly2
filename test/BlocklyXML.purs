module Test.BlocklyXML where

import Prelude

import Text.Parsing.Parser

import Test.Assert
import BlocklyXML

import Data.List


blockly_xml_test1 = 
  let orig = above_example in
    let parsed = parseBlockly (show orig) in
      do
        assert $ show orig == show parsed
 
