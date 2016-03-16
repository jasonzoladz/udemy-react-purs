module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console

import Data.Maybe.Unsafe (fromJust)
import Data.Nullable (toMaybe)

import DOM (DOM())
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)

import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element(), ElementId(..), documentToNonElementParentNode)

import React
import ReactDOM (render)
import React.DOM as D
import React.DOM.Props as P
import Data.RxState as Rx

import Routing
import Routing.Match
import Routing.Match.Class
import Routing.Hash

import Network.HTTP.Affjax

import BlogRoutes
import ActionsEffects
import Model
import Channels
import Components.Pages
import Effects.AjaxEffects
import AddOns.Animation.Transition
import AddOns.Animation.Props

routingContainer :: forall p. ReactClass (Model p)
routingContainer = createClass $ spec unit $ \ctx -> do
  state <- getProps ctx
  return $
    reactCSSTransitionGroup
        [ transitionName "example"
        , transitionEnterTimeout 500
        , transitionLeaveTimeout 500
        ]


          (case state.currPage of
            Index    -> map (displayPage index) [state]
            Post n   -> map (displayPage (onePost n)) [state]
            NewPost  -> map (displayPage newPost)     [state] )

  where
    displayPage page state = createElement page (state {key = (show state.currPage)}) []


update :: forall p. (Model p) -> Action -> (Model p)
update model action =
  case action of
    NoOp        -> model
    SetPage r   -> model { currPage = r }
    SetPosts a  -> model { allPosts = a }
    IncrementId -> model { nextId = model.nextId + 1 }

performEffects :: forall e. Effect -> Eff (MainEffects e) Unit
performEffects fxt =
  case fxt of
    GoTo r -> do
      Rx.send [ SetPage r ] actionsChannel
      setHash (show r)
    NoFx           -> return unit
    FetchPosts     -> fetchPosts
    SubmitForm bp ->  createPost bp



--main :: forall e. Eff (MainEffects e) Unit
main = do

  Rx.startApp
            update
            performEffects
            myRender
            actionsChannel
            effectsChannel
            initialModel

  Rx.subscribe actionsChannel (\action -> log (show action)) -- FOR DEBUGGING

  matches routing (\old new -> Rx.send [ SetPage new ] actionsChannel )


  where
    ui :: forall p. (Model p) -> ReactElement
    ui model = D.div' [ createFactory routingContainer model ]

    myRender model = void (elm' >>= render (ui model))

    elm' :: forall eff. Eff (dom :: DOM | eff) Element
    elm' = do
      win <- window
      doc <- document win
      elm <- getElementById (ElementId "app") (documentToNonElementParentNode (htmlDocumentToDocument doc))
      return $ fromJust (toMaybe elm)
