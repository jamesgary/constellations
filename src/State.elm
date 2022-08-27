module State exposing (State(..))

import Dict exposing (Dict)
import Edge exposing (Edge)
import Game exposing (Game)
import MouseState exposing (MouseState)
import Node exposing (Node)


type State
    = Start
    | Game Game
