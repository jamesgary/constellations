module Types exposing (..)

import Mouse
import Time


type alias Model =
    { node : Node
    , mouse : Mouse
    , now : Time.Time
    }


type alias Node =
    { rad : Float
    , pos : Pos
    , vel : Vel
    , isHovered : Bool
    }


type alias Mouse =
    { pos : Mouse.Position
    , isPressed : Bool
    }


type alias Pos =
    { x : Float
    , y : Float
    }


type alias Vel =
    { r : Float
    , a : Float
    }


type Msg
    = AnimationMsg Time.Time
    | MouseMove Mouse.Position
    | MouseDown Mouse.Position
    | MouseUp Mouse.Position
