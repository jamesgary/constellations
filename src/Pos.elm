module Pos exposing (Pos, add, applyAspectRatio, clampToCanvas, codec, toTuple)

import Cfg
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


clampToCanvas : Pos -> Pos
clampToCanvas pos =
    Pos
        (pos.x |> clamp Cfg.canvasBuffer (Cfg.canvasScale - Cfg.canvasBuffer))
        (pos.y |> clamp Cfg.canvasBuffer (Cfg.canvasScale - Cfg.canvasBuffer))


add : Pos -> Pos -> Pos
add p1 p2 =
    { x = p1.x + p2.x
    , y = p1.y + p2.y
    }



-- codec stuff


codec : Codec Pos
codec =
    Codec.object Pos
        |> Codec.field "x" .x Codec.float
        |> Codec.field "y" .y Codec.float
        |> Codec.buildObject
