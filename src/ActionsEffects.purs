module ActionsEffects where

import Control.Monad.Eff.Console
import DOM (DOM())
import Network.HTTP.Affjax (AJAX)
import Control.Monad.Eff.Exception

import Prelude

import BlogRoutes
import Model

data Action
  = NoOp
  | SetPage Routes
  | SetPosts BlogPostArray
  | IncrementId

instance showAction :: Show Action where
  show NoOp         = "NoOp"
  show (SetPage r)  = "SetPage"
  show (SetPosts a) = "SetPosts"
  show IncrementId  = "IncrementId"

data Effect
  = NoFx
  | GoTo Routes
  | FetchPosts
  | SubmitForm BlogPost

type MainEffects e = ( console :: CONSOLE
                     , dom :: DOM
                     , ajax :: AJAX
                     , exception :: EXCEPTION
                     | e)
