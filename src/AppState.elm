module AppState exposing (ActiveStateData, AppState(..))

import Dict exposing (Dict)
import Edge exposing (Edge)
import GameMode exposing (GameMode)
import MouseState exposing (MouseState)
import Node exposing (Node)


type AppState
    = Start
    | Active ActiveStateData


type alias ActiveStateData =
    { nodes : Dict Node.Id Node
    , edges : List Edge
    , difficulty : Int
    , mouseState : MouseState
    , mode : GameMode
    }
