module Colors exposing (baseBtnColors, starryNightColorGen)

import Element as E exposing (Element)
import HSLuv exposing (HSLuv)
import HSLuv.Manipulate
import List.Extra
import Random


baseBtnColors :
    { default : List E.Color
    , hover : List E.Color
    , down : List E.Color
    , border : E.Color
    }
baseBtnColors =
    { default =
        [ baseBtnColor
        , baseBtnColor
            |> HSLuv.Manipulate.mapSaturation ((*) 0.5)
            |> HSLuv.Manipulate.mapLightness ((*) 0.5)
        ]
            |> List.map hsluvToE
    , hover =
        [ baseBtnColor
            |> HSLuv.Manipulate.mapSaturation ((*) 1.1)
            |> HSLuv.Manipulate.mapLightness ((*) 1.1)
        , baseBtnColor
            |> HSLuv.Manipulate.mapSaturation ((*) 0.7)
            |> HSLuv.Manipulate.mapLightness ((*) 0.7)
        ]
            |> List.map hsluvToE
    , down =
        [ baseBtnColor
            |> HSLuv.Manipulate.mapSaturation ((*) 0.6)
            |> HSLuv.Manipulate.mapLightness ((*) 0.6)
        , baseBtnColor
            |> HSLuv.Manipulate.mapSaturation ((*) 1.1)
            |> HSLuv.Manipulate.mapLightness ((*) 1.1)
        ]
            |> List.map hsluvToE
    , border =
        baseBtnColor
            |> HSLuv.Manipulate.mapSaturation ((*) 1.5)
            |> HSLuv.Manipulate.mapLightness ((*) 1.5)
            |> hsluvToE
    }


baseBtnColor =
    HSLuv.hsluv360
        { hue = 252
        , saturation = 100
        , lightness = 54
        , alpha = 1
        }



-- utils


hsluvToE : HSLuv -> E.Color
hsluvToE hsluv =
    hsluv
        |> HSLuv.toRgba
        |> E.fromRgb



-- starry night stuff


starryNightColors =
    -- https://coolors.co/dfc6ff-927aff-aabdff-7068ff-f9dc59
    [ ( 223, 198, 255 )
    , ( 146, 122, 255 )
    , ( 170, 189, 255 )
    , ( 112, 104, 255 )
    , ( 249, 220, 89 )
    ]


starryNightColorGen : Random.Generator String
starryNightColorGen =
    Random.map4
        (\i rRand gRand bRand ->
            List.Extra.getAt i starryNightColors
                |> Maybe.withDefault ( 0, 0, 0 )
                |> (\( r, g, b ) ->
                        "rgb("
                            ++ String.fromInt (r * rRand |> round)
                            ++ ","
                            ++ String.fromInt (g * gRand |> round)
                            ++ ","
                            ++ String.fromInt (b * bRand |> round)
                            ++ ")"
                   )
        )
        (Random.int 0 (List.length starryNightColors - 1))
        randomVariance
        randomVariance
        randomVariance


randomVariance : Random.Generator Float
randomVariance =
    Random.float 0.7 1.3
