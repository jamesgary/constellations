port module Ports exposing (..)

-- TODO limit exposing

import Array exposing (Array)
import Codec exposing (Codec)
import Config exposing (Config)
import Edge exposing (Edge)
import EdgeData exposing (EdgeData)
import Graph exposing (Graph)
import Json.Encode as JE
import LocalStorage exposing (LocalStorage)
import Node exposing (Node)
import Pos exposing (Pos)
import Worker.AppToWorkerMsg as AppToWorkerMsg exposing (AppToWorkerMsg)
import Worker.WorkerToAppMsg as WorkerToAppMsg exposing (WorkerToAppMsg)



-- outgoing


save : LocalStorage -> Cmd msg
save ls =
    Save ls
        |> encodeElmToJsMsg
        |> elmToJs


loadLevel : Int -> Cmd msg
loadLevel difficulty =
    { difficulty = difficulty }
        |> AppToWorkerMsg.GenerateGraph
        |> AppToWorkerMsg
        |> encodeElmToJsMsg
        |> elmToJs


checkForIntersections : Graph -> Cmd msg
checkForIntersections graph =
    { graph = graph }
        |> AppToWorkerMsg.CheckForIntersections
        |> AppToWorkerMsg
        |> encodeElmToJsMsg
        |> elmToJs


port elmToJs : JE.Value -> Cmd msg


type ElmToJsMsg
    = Save LocalStorage
    | AppToWorkerMsg AppToWorkerMsg


encodeElmToJsMsg : ElmToJsMsg -> JE.Value
encodeElmToJsMsg msg =
    case msg of
        Save ls ->
            JE.object
                [ ( "id", JE.string "Save" )
                , ( "localStorage"
                  , ls |> Codec.encodeToValue LocalStorage.codec
                  )
                ]

        AppToWorkerMsg appToWorkerMsg ->
            JE.object
                [ ( "id", JE.string "WorkerMsg" )
                , ( "msg"
                  , appToWorkerMsg
                        |> Codec.encodeToValue AppToWorkerMsg.codec
                  )
                ]



-- incoming


type JsToElmMsg
    = WorkerToAppMsg WorkerToAppMsg


port jsToElm : (JE.Value -> msg) -> Sub msg



-- TODO deprecate below


port loadedLevelFresh : (EdgeData -> msg) -> Sub msg


port loadedLevelInProgress :
    (( { nodes : Array Node, edges : List Edge }, Int ) -> msg)
    -> Sub msg


port intersectionResults : (( Bool, List Edge ) -> msg) -> Sub msg
