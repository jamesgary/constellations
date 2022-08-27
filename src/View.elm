module View exposing (view)

import Array exposing (Array)
import Game
import Html exposing (Html, br, button, div, h1, h2, h3, main_, span, text)
import Html.Attributes exposing (class, href, target)
import Html.Events
import Html.Lazy
import Model exposing (Model)
import Msg exposing (Msg(..))
import State exposing (State)
import ViewStella


view : Model -> Html Msg
view { config, state, localStorage } =
    --Html.Lazy.lazy2
    div
        [ class "appState-container" ]
        [ case state of
            State.Start ->
                viewStartScreen localStorage.numLevelsCleared

            State.Game game ->
                Game.view localStorage.numLevelsCleared game
        ]


viewStartScreen : Int -> Html Msg
viewStartScreen numLevelsCleared =
    main_ [ Html.Attributes.id "start" ]
        [ h1 [ class "title" ] [ text "Constellations" ]
        , h2
            [ class "author" ]
            [ text "By "
            , Html.a
                [ class "twitter"
                , href "https://github.com/jamesgary"
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
        , if numLevelsCleared > 1 then
            button
                [ class "btn start-btn campaign-btn"
                , Html.Events.onClick (ClickedGoToLevel (numLevelsCleared + 1))
                ]
                [ text ("Resume Level " ++ String.fromInt (numLevelsCleared + 1)) ]

          else
            button
                [ class "btn start-btn campaign-btn"
                , Html.Events.onClick (ClickedGoToLevel 1)
                ]
                [ text "Play Campaign" ]
        , br [] []
        , div [ class "stars-bg" ]
            [ div [ Html.Attributes.id "stars" ] []
            , div [ Html.Attributes.id "stars2" ] []
            , div [ Html.Attributes.id "stars3" ] []
            ]
        ]
