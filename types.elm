module Types exposing (..)

import Dict exposing (Dict)
import Mouse
import Time


type alias Model =
    { nodes : Dict Id Node
    , edges : List ( Id, Id )
    , mouse : Mouse
    , now : Time.Time
    }


type alias Node =
    { id : Id
    , rad : Float
    , dest : Pos
    , pos : Pos
    , vel : Vel
    , isHovered : Bool
    , dragOffset : Maybe Pos
    }


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


type Msg
    = AnimationMsg Time.Time
    | MouseMove Mouse.Position
    | MouseDown Mouse.Position
    | MouseUp Mouse.Position
