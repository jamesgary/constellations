module ViewStella exposing (viewStella)

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Types exposing (..)


viewStella : Html Msg
viewStella =
    let
        x =
            50

        y =
            50
    in
    svg [ height "100%", width "100%", class "stella" ]
        [ Svg.path [ class "face", d """
        M 386 229
        C 389 182
          335 157
          282 164
        C 230 170
          180 207
          178 254
        C 176 302
          222 359
          274 353
        C 326 347
          384 277
          386 229
        """ ] []

        --, ellipse [ class "face" ] []
        , ellipse [ class "eye eye1" ] []
        , ellipse [ class "eye eye2" ] []
        , polygon
            [ class "mouth"
            , points "142 169, 181 169, 162 183"
            ]
            []
        ]
