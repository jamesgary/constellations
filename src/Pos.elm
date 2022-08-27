module Pos exposing (Pos, codec)

import Codec exposing (Codec)


type alias Pos =
    { x : Float
    , y : Float
    }



-- codec stuff


codec : Codec Pos
codec =
    Codec.object Pos
        |> Codec.field "x" .x Codec.float
        |> Codec.field "y" .y Codec.float
        |> Codec.buildObject
