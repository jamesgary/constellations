module MouseState exposing (MouseState(..), applyAspectRatio)

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


applyAspectRatio : Float -> MouseState -> MouseState
applyAspectRatio ratio mouseState =
    let
        updatePos =
            Pos.applyAspectRatio ratio
    in
    case mouseState of
        Default ->
            mouseState

        Hovering id ->
            mouseState

        Lassoed ids ->
            mouseState

        Dragging id pos ids ->
            Dragging
                id
                (pos |> updatePos)
                ids

        Lassoing start end ids ->
            Lassoing
                (start |> updatePos)
                (end |> updatePos)
                ids

        DraggingLassoed nodeOffsetDict ->
            nodeOffsetDict
                |> Dict.map
                    (\id pos ->
                        updatePos pos
                    )
                |> DraggingLassoed
