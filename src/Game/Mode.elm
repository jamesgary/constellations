module Game.Mode exposing (Mode(..))

import Shape exposing (Shape)


type Mode
    = Loading Float
    | Playing
    | Won Float (List Shape)
