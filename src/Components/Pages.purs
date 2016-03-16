module Components.Pages where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import React
import ReactDOM (render)
import React.DOM as D
import React.DOM.Props as P
import Data.RxState as Rx
import DOM.Event.Event
import DOM.Event.Types as T
import DOM.Node.Node
import Unsafe.Coerce
import Data.Validation
import Data.String

import Data.Nullable

import ReactHelpers

import BlogRoutes
import ActionsEffects
import Model
import Channels
import Validators

indexWillMount :: forall eff p. ComponentWillMount (Model p) Unit eff
indexWillMount = \_ -> Rx.send [ FetchPosts ] effectsChannel

indexRender :: forall eff p. Render (Model p) Unit eff
indexRender = \ctx -> do
  model <- getProps ctx
  return $
    D.div
      [ ]
      [ D.div
          [ P.className "text-xs-right" ]
          [
            D.a [ P.href (linkTo NewPost), P.className "btn btn-primary" ]
                [ D.text "Add a Post"]
          ]
      ]


indexSpec :: forall eff p. ReactSpec (Model p) Unit eff
indexSpec =
    { render: indexRender
    , displayName: "index"
    , getInitialState: \_ -> pure unit
    , componentWillMount: indexWillMount
    , componentDidMount: \_ -> return unit
    , componentWillReceiveProps: \_ _ -> return unit
    , shouldComponentUpdate: \_ _ _ -> return true
    , componentWillUpdate: \_ _ _ -> return unit
    , componentDidUpdate: \_ _ _ -> return unit
    , componentWillUnmount: \_ -> return unit
    }

index :: forall p. ReactClass (Model p)
index = createClass indexSpec

onePost :: forall p. Int -> ReactClass (Model p)
onePost n = createClass $ spec n $ \ctx -> do
  state <- getProps ctx
  return $
    D.div [] [ D.text "This is the NewPost Page"
             ]

newPost :: forall p. ReactClass (Model p)
newPost = createClass $ spec initialPostFormState $ \ctx -> do
 model <- getProps ctx
 state <- readState ctx
 let bPost = BlogPost { id: model.nextId, title: state.titleField.value, categories: state.categoriesField.value, content: state.contentField.value }
 return $ D.form [
                  P.onSubmit (\evt -> do
                      let domEvt = reactEventToDOMEvent evt
                      Rx.send [ IncrementId ] actionsChannel -- don't do this in a real app; must handle asynchronicity
                      if isValid (validateNewPostForm bPost)
                        then Rx.send [ SubmitForm bPost ] effectsChannel
                        else return unit
                      preventDefault domEvt)
                 ]
                 [ D.h3 [] [ D.text "Create A New Post" ]

                 , D.div [ P.className "form-group"]
                         [
                           D.label [] [ D.text "Title" ]
                         , D.input    [ P._type "text",
                                        P.className "form-control",
                                        P.value state.titleField.value,
                                        P.onChange (\evt -> do
                                             let newVal = inputTargetValue evt
                                             log newVal
                                             let newTitleField = state.titleField { value = newVal, wasTouched = true }
                                             writeState ctx $ state { titleField = newTitleField } )

                                      ]
                                      []
                         , D.text (if state.titleField.wasTouched && (length state.titleField.value < 1)
                                   then "Cannot be empty"
                                   else "")
                         ]
               , D.div [ P.className "form-group"]
                       [ D.label []
                                 [ D.text "Categories" ]
                       , D.input [    P._type "text",
                                      P.className "form-control",
                                      P.value state.categoriesField.value,
                                      P.onChange (\evt -> do
                                           let newVal = inputTargetValue evt
                                           let newCategoriesField = state.categoriesField { value = newVal, wasTouched = true }
                                           writeState ctx $ state { categoriesField = newCategoriesField } )
                                 ]
                                []
                        , D.text (if state.categoriesField.wasTouched && (length state.categoriesField.value < 1)
                                  then "Cannot be empty"
                                  else "")
                       ]
               , D.div [ P.className "form-group"]
                       [ D.label []
                                 [ D.text "Content" ]
                       , D.textarea [ P.className "form-control",
                                     P.value state.contentField.value,
                                     P.onChange (\evt -> do
                                          let newVal = inputTargetValue evt
                                          let newContentField = state.contentField { value = newVal, wasTouched = true }
                                          writeState ctx $ state { contentField = newContentField } )
                                      ]
                                    []
                        , D.text (if state.contentField.wasTouched && (length state.contentField.value < 1)
                                  then "Cannot be empty"
                                  else "")
                       ]
               , D.button [ P._type "submit", P.className "btn btn-primary"]
                          [ D.text "Submit"]
               , D.p [] [ D.text (if isValid (validateNewPostForm bPost) then "You're ready to submit" else "Form incomplete")]
                ]
    where
      reactEventToDOMEvent :: Event -> T.Event
      reactEventToDOMEvent = unsafeCoerce
