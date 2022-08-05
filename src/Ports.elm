port module Ports exposing (checkForIntersections, intersectionResults, loadLevel, loadedLevelFresh, loadedLevelInProgress, saveConfig)

import Array exposing (Array)
import Config exposing (Config)
import Edge exposing (Edge)
import EdgeData exposing (EdgeData)
import Node exposing (Node)
import Pos exposing (Pos)



-- outgoing


port loadLevel : Int -> Cmd msg


port saveConfig : Config -> Cmd msg


port checkForIntersections : ( List Node, List Edge, Int ) -> Cmd msg



-- incoming


port loadedLevelFresh : (EdgeData -> msg) -> Sub msg


port loadedLevelInProgress :
    (( { nodes : Array Node, edges : List Edge }, Int ) -> msg)
    -> Sub msg


port intersectionResults : (( Bool, List Edge ) -> msg) -> Sub msg
