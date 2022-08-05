module Main exposing (main)

import Browser
import Init exposing (init)
import State exposing (subscriptions, update)
import View exposing (view)


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
