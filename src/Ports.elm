port module Ports exposing (..)

import Array exposing (Array)
import Types exposing (..)


-- outgoing


port loadLevel : Int -> Cmd msg


port saveConfig : Config -> Cmd msg


port checkForIntersections : ( List Node, List Edge, Int ) -> Cmd msg



-- incoming


port loadedLevelFresh : (EdgeData -> msg) -> Sub msg


port loadedLevelInProgress : (( { nodes : Array Node, edges : List Edge }, Int ) -> msg) -> Sub msg


port intersectionResults : (( Bool, List Edge ) -> msg) -> Sub msg
