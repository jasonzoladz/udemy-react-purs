module AddOns.Animation.Props where

import React.DOM.Props

type Milliseconds = Int -- switch to num if broken

transitionName :: String -> Props
transitionName = unsafeMkProps "transitionName"

type TransitionNames =
  { enter        :: String
  , enterActive  :: String
  , leave        :: String
  , leaveActive  :: String
  , appearActive :: String
  }

transitionName' :: TransitionNames -> Props
transitionName' = unsafeUnfoldProps "transitionName"

transitionAppear :: Boolean -> Props
transitionAppear = unsafeMkProps "transitionAppear"

transitionEnter :: Boolean -> Props
transitionEnter = unsafeMkProps "transitionEnter"

transitionLeave :: Boolean -> Props
transitionLeave = unsafeMkProps "transitionLeave"

transitionEnterTimeout :: Milliseconds -> Props
transitionEnterTimeout = unsafeMkProps "transitionEnterTimeout"

transitionLeaveTimeout :: Milliseconds -> Props
transitionLeaveTimeout = unsafeMkProps "transitionLeaveTimeout"

component :: String -> Props
component = unsafeMkProps "component"
