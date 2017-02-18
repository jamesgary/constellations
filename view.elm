module View exposing (view)

import Html exposing (Html, div, span, h1, h2, main_, br, text)
import Html.Attributes exposing (href, target, class)
import Html.Events


-- mine

import Types exposing (..)
import ViewGame


baseStretch =
    5


baseBlur =
    0.8


angleConvert =
    180 / pi


view : Model -> Html Msg
view model =
    div
        [ class "appState-container"
        ]
        (case model.appState of
            StartState ->
                [ main_ [ Html.Attributes.id "start" ]
                    [ h1 [ class "title" ] [ text "Constellations" ]
                    , h2
                        [ class "author" ]
                        [ text "By "
                        , Html.a
                            [ class "twitter"
                            , href "https://twitter.com/james_gary"
                            , target "_blank"
                            ]
                            [ text "@james_gary" ]
                        ]
                    , Html.a
                        [ class "btn start-btn campaign-btn"
                        , Html.Events.onClick (StartCampaign)
                        ]
                        [ text "Play Campaign" ]
                    , br [] []
                    , Html.a
                        [ class "btn start-btn sandbox-btn"
                        , Html.Events.onClick (GenerateEdges 1)
                        ]
                        [ text "Sandbox" ]
                    ]
                ]

            LoadingState ->
                []

            LoadingCampaignState ->
                []

            ActiveState gameState ->
                ViewGame.drawGameState True model.config gameState

            CampaignState gameState ->
                ViewGame.drawGameState False model.config gameState
        )
