module Types exposing (..)

import Dict exposing (Dict)
import Mouse
import Time


type AppState
    = LoadingState
    | ActiveState GameState


type MouseState
    = DefaultMouseState
    | HoveringMouseState Id
    | DraggingMouseState Id Pos


type alias Model =
    { appState : AppState
    , config : Config
    }


type alias Config =
    { radius : Float
    }


type alias GameState =
    { nodes : Dict Id Node
    , edges : List Edge
    , difficulty : Int
    , mouseState : MouseState
    }


type alias Node =
    { id : Id
    , dest : Pos
    , pos : Pos
    , vel : Vel
    }


type alias Edge =
    ( Id, Id )


type alias Mouse =
    { pos : Pos
    }


type alias Pos =
    { x : Float
    , y : Float
    }


type alias Vel =
    { x : Float
    , y : Float
    , r : Float
    , a : Float
    }


type alias Id =
    Int


type alias EdgeData =
    ( List Edge, Int, Int )


type Msg
    = GenerateEdges Int
    | GeneratedEdges EdgeData
    | MouseDown Mouse.Position
    | MouseMove Mouse.Position
    | MouseUp Mouse.Position
    | AnimationMsg Time.Time
      -- config stuff
      --| ChangeDifficulty String
    | ChangeConfigRadius String



-- Handy functions


getNode : Dict Id Node -> Id -> Node
getNode nodes id =
    case Dict.get id nodes of
        Just node ->
            node

        Nothing ->
            -- should never happen
            { id = -1
            , dest = Pos 42 42
            , pos = Pos 42 42
            , vel = Vel 0 0 0 0
            }
