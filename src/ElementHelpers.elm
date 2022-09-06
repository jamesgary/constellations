module ElementHelpers exposing (..)

import Colors
import Element as E exposing (Element)
import Element.Background as EBackground
import Element.Border as EBorder
import Element.Events as EEvents
import Element.Font as EFont
import Element.Input as EInput
import Html exposing (Html)
import Html.Attributes as Attr exposing (style)
import Html.Events


class : String -> E.Attribute msg
class className =
    Attr.class className
        |> E.htmlAttribute


vw : Float -> E.Attribute msg
vw vws =
    style "font-size" (String.fromFloat vws ++ "vw")
        |> E.htmlAttribute


btn :
    List (E.Attribute msg)
    ->
        { onPress : Maybe msg
        , label : Element msg
        , colors : Colors.BtnColors
        }
    -> Element msg
btn attrs ({ onPress, label, colors } as params) =
    btnHelper
        ([ E.centerX
         , EBackground.gradient
            { angle = pi -- down
            , steps = colors.default
            }
         , EBorder.rounded 5
         , EBorder.width 2
         , EBorder.color colors.border
         , E.mouseDown
            [ EBackground.gradient
                { angle = pi -- down
                , steps = colors.down
                }
            ]
         , E.mouseOver
            [ EBackground.gradient
                { angle = pi -- down
                , steps = colors.hover
                }
            ]
         ]
            ++ attrs
        )
        params


btnHelper :
    List (E.Attribute msg)
    ->
        { onPress : Maybe msg
        , label : Element msg
        , colors : Colors.BtnColors
        }
    -> Element msg
btnHelper attrs { onPress, label } =
    EInput.button
        attrs
        { onPress = onPress
        , label = label
        }
