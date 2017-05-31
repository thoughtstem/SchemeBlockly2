module Main where

import Prelude hiding (div, id)
import Control.Monad.Eff (Eff, kind Effect)
import Pux (CoreEffects, EffModel, start)
import Pux.DOM.Events (DOMEvent, onChange, onClick, targetValue)
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (renderToDOM)
import Text.Smolder.HTML (button, div, span, textarea)
import Text.Smolder.HTML.Attributes (id, style, value)
import Text.Smolder.Markup ((!), text, (#!))

import Lisp 
import Lisp2BlocklyXML
import BlocklyXML
import TypedLisp 

import Text.Parsing.Parser

import Data.List
import Data.Either


foreign import setBlockly :: String -> String
foreign import setBlocklyToolbox :: String -> String
foreign import getBlockly :: String -> String


data Event = CodeChange DOMEvent | 
             BlocklyChange DOMEvent

type State = {editor:: LispVal, blockly:: LispVal}


reconcileBlockly {editor: es, blockly: bs} val =
  {editor: val, blockly: val}

reconcileEditor {editor: es, blockly: bs} val =
  {editor: val, blockly: bs}

reconciled {editor: es, blockly: bs} = (show es) == (show bs)

extractBlockly {editor: es, blockly: bs} = bs

extractEditor {editor: es, blockly: bs} = es

foldp :: ∀ fx. Event -> State -> EffModel State Event fx
foldp (CodeChange ev) state = 
  do { state: newState, effects: [] }
    where newState = case (runParser (targetValue ev) parseExpr) of
              Right parsed -> reconcileEditor state parsed
              Left err -> state

foldp (BlocklyChange ev) state = do
  { state: reconcileBlockly state blockly, effects: [] }
  where blockly = blocklyXMLToLisp $ parseBlockly $ getBlockly ""

view :: State -> HTML Event
view state =
  let es = extractEditor state in
  let bs = extractBlockly state in
  div do
    textarea ! id "scheme" ! value (show $ extractEditor state) #! onChange CodeChange $ text $ ""
    div $ text $ show $ es
    div $ text $ if reconciled state then "MATCH!" else "NO MATCH!"
    div $ text $ show $ bs
    div ! id "debugging" $ do
      div ! id "set_from_blockly" #! onClick BlocklyChange $ text "Set From Blockly"
      div ! id "set_from_blockly" $ text "Set From Blockly"
      case reconciled state of
        true  -> div $ text $ "No need to sync"
        false -> div $ text $ show $ setBlockly $ show $ lispToBlocklyXML $ es
      case reconciled state of
        false -> div $ text $ show $ setBlocklyToolbox $ functionDefinitions
        true  -> div $ text $ "No need to sync"
      


main :: ∀ fx. Eff (CoreEffects fx) Unit
main = do
  app <- start
    { initialState: {editor: Atom "x", blockly: Atom "y"}
    , view
    , foldp
    , inputs: []
    }

  renderToDOM "#app" app.markup app.input



-- Accessed by JavaScript 

functionDefinitions = joinS $ show <$> lispToBlockXML <$> toTemplateLisp <$> defaultFunctionDefinitions

join Nil     c = ""
join (x:Nil) c = x
join (x:xs)  c = x <> c <> (joinN xs)

joinN ls = join ls "\n"
joinS ls = join ls " "
