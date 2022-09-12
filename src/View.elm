module View exposing (view)

import Array exposing (Array)
import Colors
import Element as E exposing (Element)
import Element.Background as EBackground
import Element.Border as EBorder
import Element.Events as EEvents
import Element.Font as EFont
import Element.Input as EInput
import ElementHelpers as EH
import ElmLogo
import Game
import Html exposing (Html)
import Html.Attributes as Attr exposing (style)
import Html.Events
import Html.Lazy
import Model exposing (Model)
import Msg exposing (Msg(..))
import State exposing (State)
import ViewHelpers exposing (..)


view : Model -> Html Msg
view model =
    E.layout
        [ E.width E.fill
        , E.height E.fill
        , E.clip
        , EBackground.color (E.rgb 0 0 0)
        , EFont.color (E.rgb 0.9 0.9 0.9)
        , EFont.family [ EFont.typeface "Comfortaa" ]
        ]
        (case model.state of
            State.Start ->
                viewStartScreen model

            State.Game game ->
                Game.view
                    game
                    |> E.map GameMsg
        )


viewStartScreen : Model -> Element Msg
viewStartScreen model =
    let
        numLevelsCleared =
            model.localStorage.numLevelsCleared

        link href text =
            E.newTabLink
                [ EFont.color <| E.rgb 0.4 0.8 1
                ]
                { url = href
                , label = E.text text
                }

        startBtn =
            EH.btn
                []
                { onPress = Just (ClickedGoToLevel numLevelsCleared)
                , label =
                    E.el
                        [ E.paddingXY 20 10 ]
                        (if numLevelsCleared >= 1 then
                            E.text
                                ("Resume Level "
                                    ++ String.fromInt (numLevelsCleared + 1)
                                )

                         else
                            E.text "Start"
                        )
                , colors = Colors.baseBtnColors
                }
    in
    E.el
        [ E.width E.fill
        , E.height E.fill
        , E.behindContent viewStarBg
        ]
        (E.column
            [ E.centerX
            , E.centerY
            , E.spacing 20
            ]
            -- title
            [ E.el
                [ E.centerX
                , EFont.bold
                , EH.vw 7
                ]
                (E.text "Constellations")

            -- smaller author/source/credits
            , E.column
                [ E.spacing 15
                , E.centerX
                ]
                -- author (me!)
                [ E.row
                    [ E.centerX
                    , EFont.bold
                    , EH.vw 3
                    ]
                    [ E.text "By "
                    , link
                        "https://github.com/jamesgary"
                        "James Gary"
                    ]

                -- source (elm!)
                , E.row
                    [ E.centerX
                    , EH.vw 3
                    ]
                    [ E.text "Made with "

                    -- TODO scale like vw
                    , ElmLogo.element 50
                    , E.text " elm "
                    , link
                        "https://github.com/jamesgary/constellations"
                        "(github)"
                    ]

                -- other credits
                , E.row
                    [ E.centerX
                    , EH.vw 2
                    , E.paddingEach { sides | top = 30 }
                    ]
                    [ E.text "Background Image by "
                    , link
                        "https://commons.wikimedia.org/wiki/File:Carina_Nebula.jpg"
                        "European Southern Observatory / T. Preibisch"
                    ]
                , E.row
                    [ E.centerX
                    , EH.vw 2
                    ]
                    [ E.text "Cute planet by "
                    , link
                        "https://www.kindpng.com/imgv/hbbxTb_cute-saturn-clip-art-cute-planet-clipart-hd/"
                        "Sayre Weaver"
                    ]
                ]
            , startBtn
            ]
        )


viewStarBg : Element Msg
viewStarBg =
    Html.div [ Attr.class "stars-bg" ]
        [ Html.div [ Attr.id "stars" ] []
        , Html.div [ Attr.id "stars2" ] []
        , Html.div [ Attr.id "stars3" ] []
        ]
        |> E.html



{-
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
-}
{-
   , div [ class "stars-bg" ]
       [ div [ Attrs.id "stars" ] []
       , div [ Attrs.id "stars2" ] []
       , div [ Attrs.id "stars3" ] []
       ]
-}
