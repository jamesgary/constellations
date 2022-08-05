module Msg exposing (Msg(..))

import Array exposing (Array)
import Edge exposing (Edge)
import EdgeData exposing (EdgeData)
import IntersectionResultData exposing (IntersectionResultData)
import MousePos exposing (MousePos)
import Node exposing (Node)


type Msg
    = LoadLevel Int -- numNodes
    | LoadedLevelFresh EdgeData
    | LoadedLevelInProgress ( { nodes : Array Node, edges : List Edge }, Int )
    | MouseDown MousePos
    | MouseMove MousePos
    | MouseUp MousePos
    | AnimationMsg Float
    | ChangeConfigRadius String
    | GetIntersectionResults IntersectionResultData
    | StartCampaign
    | GoToLevel Int
