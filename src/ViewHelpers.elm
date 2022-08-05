module ViewHelpers exposing (px)


px : Float -> String
px number =
    String.fromFloat number ++ "px"
