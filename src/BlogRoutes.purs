module BlogRoutes where

import Prelude
import Control.Alt ((<|>))
import Control.Apply
import Data.Functor
import Routing
import Routing.Match
import Routing.Match.Class
import Routing.Hash
import Data.Int (floor)


data Routes
  = Index
  | Post Int
  | NewPost

instance routesShow :: Show Routes where
  show Index    = "/"
  show (Post n) = "/posts/" ++ (show n)
  show NewPost  = "/posts/new"

linkTo :: Routes -> String
linkTo r = "#" ++ (show r)

routing :: Match Routes
routing =
  newpost    <|>
  post       <|>
  index
  where
    route str = lit "" *> lit str
    index     = Index <$ oneSlash
    post      = Post <$> (route "posts" *> int)
    newpost   = NewPost <$ (oneSlash *> lit "posts" *> lit "new")
    int       = floor <$> num

oneSlash :: Match Unit
oneSlash = lit ""
