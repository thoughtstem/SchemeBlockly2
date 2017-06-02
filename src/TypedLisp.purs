module TypedLisp where

import Prelude 
import Data.List

import Lisp as L

data FunctionDefinition =
  FunctionDefinition String SomeType (List SomeType)

data SomeType = NumberType | StringType | BooleanType | ImageType

data FunctionDefinitions = List FunctionDefinition

defaultFunctionDefinitions =
 ( 
  FunctionDefinition "plus" NumberType (NumberType:NumberType:Nil) :
  FunctionDefinition "string-append" StringType (StringType:StringType:Nil) :
  FunctionDefinition "circle" ImageType (NumberType:StringType:StringType:Nil) :
  FunctionDefinition "above" ImageType (ImageType:ImageType:Nil) :
  FunctionDefinition "math_number" NumberType Nil :
  FunctionDefinition "text" StringType Nil :
  FunctionDefinition "logic_boolean" BooleanType Nil :
  Nil
 )

lookupDefinition defs fname = head $ filter (\d->(getFunctionName d) == fname)  $ defs

getFunctionReturnType (FunctionDefinition _ t _) = t

getFunctionName (FunctionDefinition s _ _) = s

toTemplateLisp (FunctionDefinition s output inputs) = 
  L.List (L.Atom s : shadows) where
    shadows = L.Meta {block_type: s, shadowness: true, x: 0, y: 0, id: ""} <$> typeToExampleLisp <$> inputs 
toTemplateLisp _ = L.Atom "nope"


typeToExampleLisp NumberType   = L.Meta {block_type: "math_number", shadowness: true, x: 0, y: 0, id: ""} $ L.Int 0
typeToExampleLisp StringType   = L.Meta {block_type: "text", shadowness: true, x: 0, y: 0, id: ""} $ L.String "text"
typeToExampleLisp BooleanType  = L.Meta {block_type: "logic_boolean", shadowness: true, x: 0, y: 0, id: ""} $ L.Bool true
typeToExampleLisp ImageType    = L.Meta {block_type: "circle", shadowness: true, x: 0, y: 0, id: ""} $ L.List (
                                    (L.Atom "circle") :
                                    (L.Meta {block_type: "math_number", shadowness: true, x: 0, y: 0, id: ""} $ L.Int 20) :
                                    (L.Meta {block_type: "text", shadowness: true, x: 0, y: 0, id: ""} $ L.String "solid") :
                                    (L.Meta {block_type: "text", shadowness: true, x: 0, y: 0, id: ""} $ L.String "red") :
                                    Nil)


