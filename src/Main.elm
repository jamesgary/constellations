module Main exposing (..)

-- mine

import Html
import Init exposing (init)
import Navigation
import State exposing (subscriptions, update)
import Types exposing (..)
import View exposing (view)


main =
    Navigation.programWithFlags UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
