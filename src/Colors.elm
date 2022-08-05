module Colors exposing (starryNightColorGen)

import List.Extra
import Random


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
