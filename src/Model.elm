module Model exposing (Model, init, mapGame)

import Config exposing (Config)
import Flags exposing (Flags)
import Game exposing (Game)
import LocalStorage exposing (LocalStorage)
import Random
import State exposing (State)


type alias Model =
    { state : State
    , seed : Random.Seed
    , config : Config
    , localStorage : LocalStorage
    }


init : Flags -> Model
init flags =
    { state = State.Start
    , seed = Random.initialSeed flags.timestamp
    , config = { radius = 25 }
    , localStorage = flags.localStorage
    }


mapGame : (Game -> Game) -> Model -> Model
mapGame mapper model =
    case model.state of
        State.Game game ->
            { model | state = State.Game (mapper game) }

        _ ->
            model
