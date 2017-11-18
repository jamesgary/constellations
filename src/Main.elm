module Main exposing (..)

-- mine

import Html
import Navigation
import State exposing (init, subscriptions, update)
import Types exposing (..)
import View exposing (view)


main =
    Navigation.programWithFlags UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
