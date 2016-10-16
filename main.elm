module Main exposing (..)

import Html.App as App


-- mine

import Types exposing (..)
import State exposing (init, update, subscriptions)
import View exposing (view)


main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
