module AddOns.Animation.Transition where

import React
import React.DOM
import React.DOM.Props


foreign import reactCSSTransitionGroup :: Array Props -> Array ReactElement -> ReactElement
-- reactCSSTransitionGroup = mkDOM (IsDynamic true) "ReactCSSTransitionGroup"
