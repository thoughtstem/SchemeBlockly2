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


foreign import setBlockly :: String -> String
foreign import getBlockly :: String -> String


data Event = CodeChange DOMEvent | 
             BlocklyChange DOMEvent

type State = String

foldp :: ∀ fx. Event -> State -> EffModel State Event fx
foldp (CodeChange ev) n = do
  { state: targetValue ev, effects: [] }
foldp (BlocklyChange ev) n = do
  { state: show $ blocklyXMLToLisp $ parseBlockly $ getBlockly "", effects: [] }

view :: State -> HTML Event
view code =
  div do
    textarea ! id "scheme" ! value code #! onChange CodeChange $ text code
    div ! id "debugging" $ do
      div ! id "set_from_blockly" #! onClick BlocklyChange $ text "Set From Blockly"
      div $ text $ show $ setBlockly $ show $ lispToBlocklyXML $ parseLisp code
      div $ text $ show $ parseLisp code
      div $ text $ show $ blocklyXMLToLisp $ lispToBlocklyXML $ parseLisp code


main :: ∀ fx. Eff (CoreEffects fx) Unit
main = do
  app <- start
    { initialState: "hi"
    , view
    , foldp
    , inputs: []
    }

  renderToDOM "#app" app.markup app.input



