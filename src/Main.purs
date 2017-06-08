module Main where

import Prelude hiding (div, id)
import Control.Monad.Eff (Eff, kind Effect)
import Pux (CoreEffects, EffModel, start)
import Pux.DOM.Events (DOMEvent, onFocus, onChange, onClick, targetValue)
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (renderToDOM, reactClassWithProps)
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

import React 



foreign import editorComponent_ :: forall props. ReactClass props

editorComponent = reactClassWithProps editorComponent_ "editor"


foreign import blocklyComponent_ :: forall props. ReactClass props

blocklyComponent = reactClassWithProps blocklyComponent_ "blockly"


data Event = CodeChange DOMEvent | 
             BlocklyChange DOMEvent |
             BlocklyFocus DOMEvent |
             CodeFocus DOMEvent |
             StepCode DOMEvent 

data FocusState = BlocklyFocused | CodeFocused | NoneFocused


instance eqT :: Eq FocusState where
    eq BlocklyFocused BlocklyFocused = true
    eq CodeFocused CodeFocused = true
    eq NoneFocused NoneFocused = true
    eq _ _ = false


type State = {editor:: LispVal, blockly:: LispVal, message:: String, focused:: FocusState}

stepState state = state{editor = stepLispProgram state.editor, blockly = stepLispProgram state.blockly, focused = NoneFocused}


reconcileBlockly state val = 
  case state.focused of
       NoneFocused -> state {blockly = val, editor = val}
       BlocklyFocused -> state {editor = val}
       CodeFocused -> state {blockly = val}

reconcileEditor state val = 
  case state.focused of
       NoneFocused -> state {blockly = val, editor = val}
       BlocklyFocused -> state {editor = val}
       CodeFocused -> state {blockly = val}

reconciled state = (show state.editor) == (show state.blockly)


foldp :: ∀ fx. Event -> State -> EffModel State Event fx
foldp (StepCode ev) state = do {state: stepState state ,effects: []}

foldp (CodeChange ev) state = 
      case (targetValue ev) == "<<undefined>>" of
           true -> do { state: state, effects: [] }
           false -> if state.focused == CodeFocused then  handleEvent (CodeChange ev) state else {state: state, effects: []}

foldp (BlocklyChange ev) state = 
    case (targetValue ev) == "<<undefined>>" of
         true -> do { state: state, effects: [] }
         false -> if state.focused == BlocklyFocused then handleEvent (BlocklyChange ev) state else  {state: state, effects: []} 

foldp (CodeFocus ev) state = {state: state {focused = CodeFocused}, effects: []}
foldp (BlocklyFocus ev) state =  {state: state {focused = BlocklyFocused}, effects: []}

handleEvent (CodeChange ev) state = 
  do { state: newState, effects: [] }
     where newState = case (runParser (targetValue ev) parseExpr) of
            Right parsed -> (reconcileEditor state parsed) { message = state.message <> " /// CodeChangeParsed"}
            Left err -> state{ message = state.message <> " /// CodeChangeFailedParse(" <> (targetValue ev) <> ")"}

handleEvent (BlocklyChange ev) state = 
  do { state: newState, effects: [] }
     where newState = case parseBlockly $ targetValue ev of
            Right parsed -> (reconcileBlockly state $ blocklyXMLToLisp parsed) { message = state.message <> " /// BlocklyChangeParsed"}
            Left err -> state{ message = state.message <> " /// BlocklyChangeFailedParse(" <> (targetValue ev) <> ")"}


handleEvent _ state = {state:state, effects:[]}


view :: State -> HTML Event
view state =
  let es = state.editor in
  let bs = state.blockly in
  let m  = state.message in
  div do
    (editorComponent {text: show es, should_update: state.focused /= CodeFocused})  #! onChange CodeChange #! onFocus CodeFocus $ text "HI"
    (blocklyComponent {text: show $ lispToBlocklyXML bs, toolboxXML: functionDefinitions, should_update: state.focused /= BlocklyFocused})  #! onChange BlocklyChange #! onFocus BlocklyFocus $ text "HI"
    div #! onClick StepCode $ text "Step"
    --div ! id "debugging" $ do
    div $ text $ lispDebugShow $ es
    div $ text $ if reconciled state then "MATCH!" else "NO MATCH!"
    div $ text $ lispDebugShow $ bs
    --div $ text $ show $ m
    div $ text $ show $ case state.focused of 
                             CodeFocused   -> "Code Focused"
                             BlocklyFocused -> "Blockly Focused"
                             NoneFocused -> "Nothing Focused"
      


main :: ∀ fx. Eff (CoreEffects fx) Unit
main = do
  app <- start
    { initialState: {editor: Atom "x", blockly: Atom "y", message: "No messages...", focused: NoneFocused}
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
