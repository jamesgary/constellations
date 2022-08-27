module Worker.Main exposing (main)

import Codec exposing (Codec)
import Edge exposing (Edge)
import Graph exposing (Graph)
import Json.Decode as JD
import Json.Encode as JE
import Node exposing (Node)
import Platform
import Pos exposing (Pos)
import Worker.AppToWorkerMsg as AppToWorkerMsg exposing (AppToWorkerMsg)
import Worker.Ports as Ports
import Worker.WorkerToAppMsg as WorkerToAppMsg exposing (WorkerToAppMsg)


type Msg
    = ReceivedFromPort JE.Value


main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


type alias Flags =
    {}


type alias Model =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( {}, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "Worker.App got Msg!" msg
    in
    case msg of
        ReceivedFromPort jsonVal ->
            case Codec.decodeValue AppToWorkerMsg.codec jsonVal of
                Ok appToWorkerMsg ->
                    let
                        _ =
                            Debug.log
                                "Woo!"
                                appToWorkerMsg
                    in
                    case appToWorkerMsg of
                        AppToWorkerMsg.GenerateGraph { difficulty } ->
                            ( {}
                            , { graph = Graph.init difficulty }
                                |> WorkerToAppMsg.GeneratedGraph
                                |> Codec.encodeToValue WorkerToAppMsg.codec
                                |> Ports.elmToJs
                            )

                        AppToWorkerMsg.CheckForIntersections { graph } ->
                            ( {}, Cmd.none )

                Err err ->
                    let
                        _ =
                            Debug.log
                                "error decoding jsToElm worker msg"
                                (JD.errorToString err)
                    in
                    ( {}, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    [ Ports.jsToElm ReceivedFromPort ]
        |> Sub.batch
