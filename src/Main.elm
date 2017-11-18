module Main exposing (..)

-- mine

import Html
import State exposing (init, subscriptions, update)
import Types exposing (..)
import View exposing (view)


main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
