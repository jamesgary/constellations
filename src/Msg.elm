module Msg exposing (Msg(..))

import Array exposing (Array)
import Edge exposing (Edge)
import EdgeData exposing (EdgeData)
import IntersectionResultData exposing (IntersectionResultData)
import MousePos exposing (MousePos)
import Node exposing (Node)


type Msg
    = ClickedGoToLevel Int -- numNodes
      -- mouse logic
    | MouseDown MousePos
    | MouseMove MousePos
    | MouseUp MousePos
      -- other subs
    | AnimationMsg Float
      -- ports
    | LoadedLevelFresh EdgeData
    | LoadedLevelInProgress ( { nodes : Array Node, edges : List Edge }, Int )
    | GetIntersectionResults IntersectionResultData
