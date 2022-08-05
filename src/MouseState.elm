module MouseState exposing (MouseState(..))

import Node exposing (Node)
import Pos exposing (Pos)


type MouseState
    = Default
    | Hovering Node.Id
    | Dragging Node.Id Pos (List Node.Id)
    | Lassoing Pos Pos (List Node.Id)
    | Lassoed (List Node.Id)
    | DraggingLassoed (List ( Node.Id, Pos ))
