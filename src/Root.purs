module Root where

import Prelude

import Counter as Counter
import React.Basic as React
import React.Basic.DOM as R
import React.Basic.Events as Events

type Props =
  { label :: String
  }

type State = {
  counter :: Int
}

component :: React.Component Props
component = React.component { displayName: "Counter", initialState, receiveProps, render }
  where
    initialState :: State
    initialState =
      { counter: 0
      }

    receiveProps _ =
      pure unit

    render { props, state, setState } =
      let handleClick = Events.handler_ do
            setState \s -> s { counter = s.counter + 1 }
      in
      R.div { children: [
          R.h1 { children: [ R.text "Hello World"] },
          React.element Counter.component { label: "Click counts"}
        ]
      }
