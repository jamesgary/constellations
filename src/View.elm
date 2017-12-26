module View exposing (view)

-- mine

import Array exposing (Array)
import Html exposing (Html, br, button, div, h1, h2, h3, main_, span, text)
import Html.Attributes exposing (class, href, target)
import Html.Events
import Html.Lazy
import Types exposing (..)
import ViewGame
import ViewStella


baseStretch =
    5


baseBlur =
    0.8


angleConvert =
    180 / pi


view : Model -> Html Msg
view { config, levelsCleared, appState } =
    Html.Lazy.lazy2
        div
        [ class "appState-container" ]
        (case config.showStella of
            True ->
                [ ViewStella.viewStella ]

            False ->
                case appState of
                    StartState ->
                        viewStartScreen levelsCleared

                    ActiveState gameState ->
                        ViewGame.drawGameState config levelsCleared gameState
        )


viewStartScreen : Int -> List (Html Msg)
viewStartScreen levelsCleared =
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
                [ text "James Gary" ]
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
        , h3
            [ class "source" ]
            [ text "Images courtesy of "
            , Html.a
                [ class "link"
                , href "https://commons.wikimedia.org/wiki/File:Carina_Nebula.jpg"
                , target "_blank"
                ]
                [ text "European Southern Observatory (ESO)" ]
            ]
        , if levelsCleared > 1 then
            button
                [ class "btn start-btn campaign-btn"
                , Html.Events.onClick (GenerateEdges (levelsCleared + 1))
                ]
                [ text ("Resume Level " ++ toString (levelsCleared + 1)) ]
          else
            button
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
