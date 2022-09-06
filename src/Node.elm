module Node exposing (Id, Node, applyAspectRatio, codec, init)

import Codec exposing (Codec)
import Pos exposing (Pos)
import Vel exposing (Vel)


type alias Id =
    String


type alias Node =
    { pos : Pos
    , dest : Pos
    , vel : Vel
    }


init : Pos -> Node
init pos =
    { pos = pos
    , dest = pos
    , vel = Vel 0 0 0 0
    }


applyAspectRatio : Float -> Node -> Node
applyAspectRatio ratio node =
    { node
        | pos =
            node.pos
                |> Pos.applyAspectRatio ratio
        , dest =
            node.dest
                |> Pos.applyAspectRatio ratio
    }



-- codec stuff


codec : Codec Node
codec =
    -- Just encode a Pos without dest or velocity
    -- since we'll never need the other values (this is just for saving to localstorage).
    -- Using .dest is more accurate than .pos (speedrunners will like it)
    Pos.codec
        |> Codec.map init .dest
