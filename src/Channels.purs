module Channels where

import Data.RxState as Rx

import ActionsEffects

actionsChannel :: Rx.Channel (Array Action)
actionsChannel = Rx.newChannel []

effectsChannel :: Rx.Channel (Array Effect)
effectsChannel = Rx.newChannel []
