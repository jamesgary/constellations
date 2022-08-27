module App exposing (init, subscriptions, update)

import Array
import Browser
import Browser.Events
import Cfg
import Codec exposing (Codec)
import Color
import Colors
import Config exposing (Config)
import Dict exposing (Dict)
import Ease
import Edge exposing (Edge)
import Flags exposing (Flags)
import Game exposing (Game)
import Graph exposing (Graph)
import IntersectionResultData exposing (IntersectionResultData)
import Json.Decode as JD
import List.Extra
import Model exposing (Model)
import MouseState exposing (MouseState)
import Msg exposing (Msg(..))
import Node exposing (Node)
import Ports
import Pos exposing (Pos)
import Random
import Random.Extra
import Random.List
import Shape exposing (Shape)
import State exposing (State)
import Vel exposing (Vel)
import Worker.WorkerToAppMsg exposing (WorkerToAppMsg)



-- INIT


init : JD.Value -> ( Model, Cmd Msg )
init jsonFlags =
    case jsonFlags |> JD.decodeValue Flags.decoder of
        Ok flags ->
            ( Model.init flags
            , Cmd.none
            )

        Err err ->
            Debug.todo "Error loading flags" err



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseMove mousePos ->
            ( model
                |> Model.mapGame (Game.mouseMove mousePos)
            , Cmd.none
            )

        MouseDown mousePos ->
            ( model
                |> Model.mapGame (Game.mouseDown mousePos)
            , Cmd.none
            )

        MouseUp ->
            ( model
                |> Model.mapGame Game.mouseUp
            , Cmd.none
            )

        AnimationMsg delta ->
            --updateAnimation model time
            ( model
                |> Model.mapGame (Game.tick delta)
            , Cmd.none
            )

        -- response from js-land
        LoadedLevelFresh ( edges_, numNodes_, difficulty ) ->
            {-
               let
                   ( edges, numNodes, newSeed ) =
                       makeGraphEdges difficulty model.seed
               in
               ( { model
                   | state =
                       State.Game
                           { nodes =
                               List.range 0 (numNodes - 1)
                                   |> List.indexedMap (makeNode model.config numNodes)
                                   |> Dict.fromList
                           , edges = edges
                           , difficulty = difficulty
                           , mouseState = MouseState.Default
                           , mode = Game.Loading 0
                           }
                   , seed = newSeed
                 }
               , Cmd.none
               )
            -}
            ( model, Cmd.none )

        LoadedLevelInProgress ( { nodes, edges }, difficulty ) ->
            {-
               ( { model
                   | state =
                       State.Game
                           { nodes =
                               nodes
                                   |> Array.toList
                                   |> List.map (\n -> ( n.id, { n | pos = n.dest } ))
                                   |> Dict.fromList
                           , edges = edges
                           , difficulty = difficulty
                           , mouseState = MouseState.Default
                           , mode = Game.Playing
                           }
                 }
               , Cmd.none
               )
            -}
            ( model, Cmd.none )

        GetIntersectionResults intersectionResultData ->
            --updateGetIntersectionResults model intersectionResultData
            ( model, Cmd.none )

        ReceivedFromPort json ->
            -- currently only receiving worker msgs from port
            case Codec.decodeValue Worker.WorkerToAppMsg.codec json of
                Ok workerMsg ->
                    case workerMsg of
                        Worker.WorkerToAppMsg.GeneratedGraph { graph } ->
                            ( handleGeneratedGraph graph model, Cmd.none )

                        Worker.WorkerToAppMsg.GotIntersections { edges } ->
                            ( model, Cmd.none )

                Err err ->
                    let
                        _ =
                            Debug.log "Can't decode worker msg" err
                    in
                    ( model, Cmd.none )

        ClickedGoToLevel difficulty ->
            ( model, Ports.loadLevel difficulty )


handleGeneratedGraph : Graph -> Model -> Model
handleGeneratedGraph graph model =
    { model
        | state =
            State.Game
                (Game.init graph)
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.loadedLevelFresh LoadedLevelFresh
        , Ports.loadedLevelInProgress LoadedLevelInProgress
        , Ports.intersectionResults GetIntersectionResults
        , Ports.jsToElm ReceivedFromPort
        , Browser.Events.onAnimationFrameDelta AnimationMsg
        ]
