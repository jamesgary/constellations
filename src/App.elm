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
            let
                isTestingGame =
                    True
            in
            if isTestingGame then
                Model.init flags
                    |> update (ClickedGoToLevel 1)

            else
                ( Model.init flags
                , Cmd.none
                )

        Err err ->
            Debug.todo "Error loading flags" err



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedGoToLevel difficulty ->
            let
                ( game, cmd ) =
                    Game.init difficulty
            in
            ( { model | state = State.Game game }
            , cmd
                |> Cmd.map GameMsg
            )

        GameMsg gameMsg ->
            case model.state of
                State.Game game ->
                    let
                        ( newGame, gameCmd ) =
                            game
                                |> Game.update gameMsg
                    in
                    ( { model | state = State.Game newGame }
                    , gameCmd
                        |> Cmd.map GameMsg
                    )

                _ ->
                    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Game.subscriptions |> Sub.map GameMsg
        ]
