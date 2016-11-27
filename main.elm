module Main exposing (..)

import Html


-- mine

import Types exposing (..)
import State exposing (init, update, subscriptions)
import View exposing (view)


main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
