module Msg exposing (Msg(..))

import Array exposing (Array)
import Edge exposing (Edge)
import EdgeData exposing (EdgeData)
import IntersectionResultData exposing (IntersectionResultData)
import Json.Encode as JE
import Node exposing (Node)
import Pos exposing (Pos)


type Msg
    = ClickedGoToLevel Int -- numNodes
      -- mouse logic
    | MouseDown Pos
    | MouseMove Pos
    | MouseUp
      -- other subs
    | AnimationMsg Float
      -- ports
    | LoadedLevelFresh EdgeData
    | LoadedLevelInProgress ( { nodes : Array Node, edges : List Edge }, Int )
    | GetIntersectionResults IntersectionResultData
    | ReceivedFromPort JE.Value
