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
import LocalStorage exposing (LocalStorage)
import MouseState exposing (MouseState)
import Ports
import Pos exposing (Pos)
import Shape exposing (Shape)
import Time
import Worker.WorkerToAppMsg as WorkerToAppMsg exposing (WorkerToAppMsg)


type Game
    = Game Model


init : LocalStorage -> Int -> ( Game, Cmd Msg )
init localStorage lvlIndex =
    let
        ( model, cmd ) =
            Model.init localStorage lvlIndex
    in
    ( Game model, cmd )


view : Game -> Element Msg
view (Game model) =
    Game.View.view model


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

        ClickedGoToLevel lvlIndex ->
            game
                |> mapModel
                    (Model.goToLvl lvlIndex)

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