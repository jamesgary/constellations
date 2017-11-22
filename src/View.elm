module View exposing (view)

-- mine

import Html exposing (Html, br, button, div, h1, h2, h3, main_, span, text)
import Html.Attributes exposing (class, href, target)
import Html.Events
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
        [ class "appState-container" ]
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
                    , h3
                        [ class "source" ]
                        [ text "Source code: "
                        , Html.a
                            [ class "link"
                            , href "https://github.com/jamesgary/constellations"
                            , target "_blank"
                            ]
                            [ text "github.com/jamesgary/constellations" ]
                        ]
                    , button
                        [ class "btn start-btn campaign-btn"
                        , Html.Events.onClick StartCampaign
                        ]
                        [ text "Play Campaign" ]
                    , br [] []
                    , button
                        [ class "btn start-btn sandbox-btn"
                        , Html.Events.onClick (GenerateEdges 1)
                        ]
                        [ text "Sandbox" ]
                    , div [ class "stars-bg" ]
                        [ div [ Html.Attributes.id "stars" ] []
                        , div [ Html.Attributes.id "stars2" ] []
                        , div [ Html.Attributes.id "stars3" ] []
                        ]
                    ]
                ]

            --LoadingState age numNodes ->
            --    ViewGame.drawLoadingAnim model.config age numNodes
            --LoadingCampaignState ->
            --    []
            ActiveState gameState ->
                ViewGame.drawGameState model.config gameState
        )
