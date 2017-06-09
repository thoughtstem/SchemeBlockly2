module Main where

import Prelude hiding (div, id)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Monoid (mempty)

import Pux (CoreEffects, EffModel, start)
import Pux.DOM.Events (DOMEvent, onFocus, onChange, onClick, targetValue)
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (renderToDOM, reactClassWithProps)
import Text.Smolder.HTML (button, div, span, textarea, canvas)
import Text.Smolder.HTML.Attributes (id, style, value, width, height)
import Text.Smolder.Markup ((!), text, (#!))

import Partial.Unsafe (unsafePartial)

import Graphics.Canvas 
import Graphics.Drawing (scale, translate, shadowBlur, black, shadowColor,
                         shadow, render, rotate, closed, fillColor, outlineColor, filled, outlined, circle, rectangle)
import Color.Scale (sample)
import Color.Scheme.Clrs 


import Lisp 
import Lisp2BlocklyXML
import BlocklyXML
import TypedLisp as TL

import Text.Parsing.Parser

import Data.Int (toNumber)
import Data.List 
import Data.Either
import Data.Maybe

import Math (sin, cos, pi)

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


reconcileBlockly state val = state {blockly = val, editor = val}

reconcileEditor state val = state {blockly = val, editor = val}

reconciled state = (show state.editor) == (show state.blockly)






put_on_canvas stuff = liftEff $ unsafePartial do
                                          Just canvas <- getCanvasElementById "canvas"
                                          ctx <- getContext2D canvas
                                          _ <- clearRect ctx {x: 0.0, y: 0.0, w: 1000.0, h: 1000.0}
                                          _ <- render ctx $ translate 50.0 50.0 $ stuff
                                          pure Nothing


effects_from_lisp lisp = let the_effect = effect_from_lisp lisp in  --Assume one effect per program for now
                          case the_effect of
                            Just e  -> [e]
                            Nothing -> []

shape_from_lisp (List (Atom "circle" : Int r : String mode : String color : Nil)) = Just $ my_circle r color mode 
shape_from_lisp (List (Atom "square" : Int s : String mode : String color : Nil)) =  Just $ my_square s color mode
shape_from_lisp (List (Atom "rectangle" : Int w :  Int h : String mode : String color : Nil)) =  Just $ my_rectangle w h color mode 
shape_from_lisp (List (Atom "beside" : Int w :  Int h : String mode : String color : Nil)) =  Just $ my_rectangle w h color mode 
shape_from_lisp _ = Nothing

effect_from_lisp l = let shape = shape_from_lisp l in
                     case shape of
                       Just s  -> Just $ put_on_canvas $ s
                       Nothing -> Nothing
                  


my_circle radius color mode = fill_or_outline color mode $ circle 0.0 0.0 $ toNumber radius

my_square size color mode = fill_or_outline color mode $ rectangle 0.0 0.0 (toNumber size) (toNumber size)

my_rectangle w h color mode = fill_or_outline color mode $ rectangle 0.0 0.0 (toNumber w) (toNumber h)

fill_or_outline color "solid" shape   = filled (fillColor $ color_from_string color ) $ shape
fill_or_outline color "outline" shape = outlined (outlineColor $ color_from_string color ) $ shape

color_from_string "red"   = red
color_from_string "green" = green
color_from_string "blue"  = blue


foldp (StepCode ev) state = {state: newState, effects: effects_from_lisp state.editor }
                              where newState = stepState state

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
    canvas ! id "canvas" ! width "1000" ! height "1000" $ text "Canvas?"
    --div ! id "debugging" $ do
    div $ text $ lispDebugShow $ es
    div $ text $ if reconciled state then "MATCH!" else "NO MATCH!"
    div $ text $ lispDebugShow $ bs
    --div $ text $ show $ m
    div $ text $ show $ case state.focused of 
                             CodeFocused   -> "Code Focused"
                             BlocklyFocused -> "Blockly Focused"
                             NoneFocused -> "Nothing Focused"
      


main = do
  app <- start
    { initialState: {editor: Atom "x", blockly: Atom "y", message: "No messages...", focused: NoneFocused}
    , view
    , foldp
    , inputs: []
    }

  renderToDOM "#app" app.markup app.input



-- Accessed by JavaScript 

functionDefinitions = joinS $ show <$> lispToBlockXML <$> TL.toTemplateLisp <$> TL.defaultFunctionDefinitions

join Nil     c = ""
join (x:Nil) c = x
join (x:xs)  c = x <> c <> (joinN xs)

joinN ls = join ls "\n"
joinS ls = join ls " "
