module Effects.AjaxEffects where

import Prelude
import Data.Either

import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Eff.Exception
import Control.Monad.Error.Class
import Control.Monad.Aff
import Network.HTTP.Affjax
import Data.HTTP.Method
import Network.HTTP.Affjax.Response
import Data.Argonaut.Core
import Data.Argonaut.Encode

import Data.RxState as Rx

import Model
import ActionsEffects
import Channels
import BlogRoutes

rootUrl :: String
rootUrl = "http://reduxblog.herokuapp.com/api/posts"

apiKey :: String
apiKey = "?key=8237rahs"

-- http://reduxblog.herokuapp.com/api/posts?key=8237rahs

fetchPosts :: forall e. Eff (MainEffects e) Unit
fetchPosts = do
  let rUrl = rootUrl ++ apiKey
  runAff   (\_ -> Rx.send [ NoOp ] actionsChannel)
           (\resp -> do
                let ePosts = decodeBlogPostArray (resp.response)
                case ePosts of
                  Left _      -> log "An error occurred.  Remember, it's the API here.  It's not isomorphic, so you'd need to define separate decoders."
                  Right posts -> Rx.send [ SetPosts posts ] actionsChannel)
           (affjax $ defaultRequest { url = rUrl }) -- :: forall e. Aff (ajax :: AJAX | e) (AffjaxResponse Json))

createPost :: forall e. BlogPost -> Eff (MainEffects e) Unit
createPost bp = do
  let pUrl = rootUrl ++ apiKey
  let bpJSON = encodeJson bp
  runAff (\_ -> log "There was an error sending your BlogPost.")
         (\_ -> Rx.send [ GoTo Index ] effectsChannel)
         ((post pUrl bpJSON) :: forall e. Aff (ajax :: AJAX | e) (AffjaxResponse Json))
