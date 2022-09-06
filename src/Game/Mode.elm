module Game.Mode exposing (Mode(..), applyAspectRatio)

import Shape exposing (Shape)


type Mode
    = Loading Float
    | Playing
    | Won Float (List Shape)


applyAspectRatio : Float -> Mode -> Mode
applyAspectRatio ratio mode =
    case mode of
        Loading _ ->
            mode

        Playing ->
            mode

        Won time shapes ->
            Won
                time
                (shapes
                    |> List.map (Shape.applyAspectRatio ratio)
                )
