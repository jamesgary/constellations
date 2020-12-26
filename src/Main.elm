module Main exposing (main)

-- mine

import Browser
import Html
import Init exposing (init)
import State exposing (subscriptions, update)
import Types exposing (..)
import View exposing (view)


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
