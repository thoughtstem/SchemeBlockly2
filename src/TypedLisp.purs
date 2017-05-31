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
    shadows = L.Shadow <$> typeToExampleLisp <$> inputs 
toTemplateLisp _ = L.Atom "nope"


typeToExampleLisp NumberType   = L.Shadow $ L.Int 0
typeToExampleLisp StringType   = L.Shadow $ L.String "text"
typeToExampleLisp BooleanType  = L.Shadow $ L.Bool true
typeToExampleLisp ImageType    = L.shadowizeLisp $ L.parseLisp "(circle 20 \"solid\" \"red\")"

