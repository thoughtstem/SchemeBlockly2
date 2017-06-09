module Lisp where

import Prelude 

import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.Expr
import Text.Parsing.Parser.String

import Data.Show
import Data.Tuple
import Data.String as S
import Data.Foldable hiding (oneOf, length)
import Data.Either
import Data.Maybe
import Data.List hiding (span)
import Data.Int (fromString)
import Control.Alt ((<|>))

import Control.Lazy

import Math (abs)


import Control.Monad.Eff.Console as Console
import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (message)
import SandScript.Eval (primitiveFuncs, runComputations)

runOne :: forall e. String -> Eff ( console :: Console.CONSOLE | e ) Unit
runOne input = runComputations primitiveFuncs input >>=
  case _ of
       Left err -> Console.error $ show err
       Right (Tuple _ wff) -> Console.log $ show wff


