module Model exposing (Model)

import AppState exposing (AppState)
import Config exposing (Config)
import Random


type alias Model =
    { appState : AppState
    , levelsCleared : Int
    , seed : Random.Seed
    , config : Config
    }
