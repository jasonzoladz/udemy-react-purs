module Validators where

import Prelude

import Data.Validation
import Data.String (length)


import Model

validateNewPostForm :: BlogPost -> V (Array String) BlogPost
validateNewPostForm (BlogPost bp) =
  (\a b c d -> BlogPost { id: a, title: b, categories: c, content: d})
      <$> pure bp.id
      <*> validateNonEmpty bp.title
      <*> validateNonEmpty bp.categories
      <*> validateNonEmpty bp.content
    where
      validateNonEmpty str =
        if length str > 0
        then pure str
        else invalid [ "Field must be non empty" ]
