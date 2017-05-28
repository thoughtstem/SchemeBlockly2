module Main where

import Prelude hiding (div, id)
import Control.Monad.Eff (Eff)
import Pux (CoreEffects, EffModel, start)
import Pux.DOM.Events (DOMEvent, onChange, targetValue)
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (renderToDOM)
import Text.Smolder.HTML (button, div, span, textarea)
import Text.Smolder.HTML.Attributes (id, style)
import Text.Smolder.Markup ((!), text, (#!))

import Lisp (readExpr)

data Event = CodeChange DOMEvent

type State = String

foldp :: ∀ fx. Event -> State -> EffModel State Event fx
foldp (CodeChange ev) n = { state: targetValue ev, effects: [] }

view :: State -> HTML Event
view code =
  div do
    textarea ! id "scheme" #! onChange CodeChange $ text code
    span $ text $ readExpr code

main :: ∀ fx. Eff (CoreEffects fx) Unit
main = do
  app <- start
    { initialState: "hi"
    , view
    , foldp
    , inputs: []
    }

  renderToDOM "#app" app.markup app.input
