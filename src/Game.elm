module Game exposing (Game, init, subscriptions, update, view)

import Browser.Events
import Cfg
import Dict exposing (Dict)
import Element as E exposing (Element)
import Game.Model as Model exposing (Model)
import Game.Msg as Msg exposing (Msg(..))
import Game.View
import Graph exposing (Graph)
import Html exposing (Html)
import Json.Decode as Decode
import List.Extra
import MouseState exposing (MouseState)
import Ports
import Pos exposing (Pos)
import Shape exposing (Shape)
import Worker.WorkerToAppMsg as WorkerToAppMsg exposing (WorkerToAppMsg)


type Game
    = Game Model


init : Int -> ( Game, Cmd Msg )
init diff =
    let
        ( model, cmd ) =
            Model.init diff
    in
    ( Game model, cmd )


view : Int -> Game -> Element Msg
view numLevelsCleared (Game model) =
    Game.View.view numLevelsCleared model


update : Msg -> Game -> ( Game, Cmd Msg )
update msg game =
    let
        _ =
            case msg of
                Tick _ ->
                    ""

                _ ->
                    --Debug.log "gameMsg" msg
                    ()
                        |> always ""
    in
    case msg of
        Tick delta ->
            game
                |> mapModel
                    (Model.tick delta)

        MouseMove mousePos ->
            game
                |> mapModel
                    (Model.mouseMove mousePos)

        MouseDown mousePos ->
            game
                |> mapModel
                    (Model.mouseDown mousePos)

        MouseUp ->
            game
                |> mapModel
                    Model.mouseUp

        ClickedGoToLevel numLvl ->
            -- TODO
            ( game, Cmd.none )

        GotContainerDom result ->
            case result of
                Ok element ->
                    game
                        |> mapModel
                            (Model.updateDom element)

                Err err ->
                    Debug.todo
                        ("couldn't get dom: "
                            ++ Debug.toString err
                        )

        ViewportResized ->
            ( game
            , Model.getContainerDomCmd
            )

        GotWorkerMsg workerMsg ->
            game
                |> mapModel
                    (Model.handleWorkerMsg workerMsg)


mapModel : (Model.Model -> ( Model.Model, Cmd Msg )) -> Game -> ( Game, Cmd Msg )
mapModel mapper (Game model) =
    mapper model
        |> Tuple.mapFirst Game


subscriptions =
    [ Browser.Events.onAnimationFrameDelta Tick
    , Browser.Events.onMouseUp (Decode.succeed MouseUp)
    , Ports.workerToAppSub GotWorkerMsg
    , Browser.Events.onResize (\x_ y_ -> ViewportResized)
    ]
        |> Sub.batch
