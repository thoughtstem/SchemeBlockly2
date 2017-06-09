module TypedLisp where

import Prelude 
import Data.List

import SandScript.AST as L

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

--toTemplateLisp (FunctionDefinition s output inputs) = 
toTemplateLisp _ = L.Atom "nope"


typeToExampleLisp NumberType   = L.Integer 0
typeToExampleLisp StringType   = L.String "text"
typeToExampleLisp BooleanType  = L.Bool true
typeToExampleLisp ImageType    = L.List (
                                    (L.Atom "circle") :
                                    (L.Integer 20) :
                                    (L.String "solid") :
                                    (L.String "red") :
                                    Nil)


