module Types exposing (..)

import Dict exposing (Dict)
import Mouse
import Time


type AppState
    = LoadingState
    | ActiveState GameState


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
    , mouse : Mouse
    , difficulty : Int
    }


type alias Node =
    { id : Id
    , dest : Pos
    , pos : Pos
    , vel : Vel
    , isHovered : Bool
    , state : NodeState
    }


type NodeState
    = Default
    | Hovered
    | Dragged Pos


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
