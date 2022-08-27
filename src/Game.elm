module Game exposing (Game, init, mouseDown, mouseMove, mouseUp, tick, view)

import Cfg
import Dict exposing (Dict)
import Game.Model
import Game.View
import Graph exposing (Graph)
import Html exposing (Html)
import List.Extra
import MouseState exposing (MouseState)
import Msg exposing (Msg)
import Pos exposing (Pos)
import Shape exposing (Shape)


type Game
    = Game Game.Model.Model


init : Graph -> Game
init graph =
    Game.Model.init graph
        |> Game


view : Int -> Game -> Html Msg
view numLevelsCleared (Game model) =
    Game.View.view numLevelsCleared model


tick : Float -> Game -> Game
tick delta game =
    game
        |> mapModel
            (Game.Model.tick delta)


mouseMove : Pos -> Game -> Game
mouseMove mousePos game =
    game
        |> mapModel
            (Game.Model.mouseMove mousePos)


mouseDown : Pos -> Game -> Game
mouseDown mousePos game =
    game
        |> mapModel
            (Game.Model.mouseDown mousePos)


mouseUp : Game -> Game
mouseUp game =
    game
        |> mapModel Game.Model.mouseUp


mapModel : (Game.Model.Model -> Game.Model.Model) -> Game -> Game
mapModel mapper (Game model) =
    Game (mapper model)
