module Msg exposing (Msg(..))

import Array exposing (Array)
import Edge exposing (Edge)
import EdgeData exposing (EdgeData)
import Game.Msg
import IntersectionResultData exposing (IntersectionResultData)
import Json.Encode as JE
import Node exposing (Node)
import Pos exposing (Pos)


type Msg
    = ClickedGoToLevel Int -- numNodes
    | GameMsg Game.Msg.Msg
