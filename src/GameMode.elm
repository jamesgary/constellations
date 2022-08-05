module GameMode exposing (GameMode(..))

import Shape exposing (Shape)


type alias Time =
    Float


type GameMode
    = Loading Time
    | Playing
    | Won Time (List Shape)
