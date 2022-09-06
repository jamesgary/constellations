module Effect exposing (Effect(..))

import Game.Msg


type Effect
    = GoToTitle
    | GameCmd (Cmd Game.Msg.Msg)
