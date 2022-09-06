module Pos exposing (Pos, applyAspectRatio, codec, toTuple)

import Codec exposing (Codec)


type alias Pos =
    { x : Float
    , y : Float
    }


toTuple : Pos -> ( Float, Float )
toTuple pos =
    ( pos.x, pos.y )


applyAspectRatio : Float -> Pos -> Pos
applyAspectRatio ratio pos =
    { pos | x = pos.x * ratio }



-- codec stuff


codec : Codec Pos
codec =
    Codec.object Pos
        |> Codec.field "x" .x Codec.float
        |> Codec.field "y" .y Codec.float
        |> Codec.buildObject
