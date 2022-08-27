module MouseState exposing (MouseState(..))

import Dict exposing (Dict)
import Node exposing (Node)
import Pos exposing (Pos)
import Set exposing (Set)


type MouseState
    = Default
    | Hovering String -- hovering 1 node
    | Lassoed (Set String) -- finished lassoing group of nodes
      -- mouse held down
    | Dragging String Pos (Set String) -- dragging 1 node
    | Lassoing Pos Pos (Set String) -- drawing lasso rect
    | DraggingLassoed (Dict String Pos) -- dragging group of nodes
