module Test.Lisp2BlocklyXML where

import Prelude

import Test.Assert
import BlocklyXML
import Lisp2BlocklyXML

lisp_2_blockly_xml_test1 = 
  let orig = above_example in
    let there_and_back = lispToBlocklyXML (blocklyXMLToLisp orig) in
      do
        assert $ show orig == show there_and_back
