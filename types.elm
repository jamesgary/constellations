module Types exposing (..)

import Mouse
import Time


type alias Model =
    { nodes : List Node
    , edges : List ( Int, Int )
    , mouse : Mouse
    , now : Time.Time
    }


type alias Node =
    { id : Int
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


type Msg
    = AnimationMsg Time.Time
    | MouseMove Mouse.Position
    | MouseDown Mouse.Position
    | MouseUp Mouse.Position
