module ViewHelpers exposing (px, sides)


px : Float -> String
px number =
    String.fromFloat number ++ "px"


sides =
    { top = 0
    , left = 0
    , right = 0
    , bottom = 0
    }
