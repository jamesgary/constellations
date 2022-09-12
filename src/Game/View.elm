module Game.View exposing (view)

import Cfg
import Colors
import Config exposing (Config)
import Dict exposing (Dict)
import Ease
import Edge exposing (Edge)
import Element as E exposing (Element)
import Element.Background as EBackground
import Element.Border as EBorder
import Element.Events as EEvents
import Element.Font as EFont
import Element.Input as EInput
import ElementHelpers as EH
import Game.Mode as Mode exposing (Mode)
import Game.Model as Model exposing (Model)
import Game.Msg as Msg exposing (Msg(..))
import Graph exposing (Graph)
import Html exposing (Html)
import Html.Attributes as Attr exposing (style)
import Html.Events
import Json.Decode as Decode
import Json.Encode
import MouseState exposing (MouseState)
import Node exposing (Node)
import Pos exposing (Pos)
import Set exposing (Set)
import Shape exposing (Shape)
import Svg exposing (ellipse, line, polygon, rect)
import Svg.Attributes as SvgAttr exposing (class, cx, cy, dx, dy, fill, height, id, offset, r, rx, ry, spreadMethod, stdDeviation, stopColor, transform, viewBox, width, x, x1, x2, y, y1, y2)
import Svg.Events
import Vel exposing (Vel)
import ViewHelpers exposing (..)


baseStretch =
    5


baseBlur =
    0.8


angleConvert =
    180 / pi


view : Model -> Element Msg
view model =
    let
        modClass =
            case model.mouseState of
                MouseState.Hovering _ ->
                    "is-hovering"

                MouseState.Dragging _ _ _ ->
                    "is-dragging"

                MouseState.Lassoing _ _ _ ->
                    "is-lassoing"

                _ ->
                    ""
    in
    E.row
        [ E.width E.fill
        , E.height E.fill
        , E.inFront
            (if model.isSidebarCollapsed then
                viewCollapsedSidebar model

             else
                E.none
            )
        ]
        [ if model.isSidebarCollapsed then
            E.none

          else
            viewSidebar model
        , E.el
            [ E.width E.fill
            , E.height E.fill
            , E.clip

            -- constellation
            , E.inFront (drawConstellation model)

            -- glass
            , E.inFront
                (E.el
                    [ E.width E.fill
                    , E.height E.fill
                    , onMouseDown
                    , Attr.class modClass
                        |> E.htmlAttribute
                    ]
                    E.none
                )

            -- starry rotating bg
            , E.behindContent
                (E.el
                    [ E.width E.fill
                    , E.height E.fill
                    , EH.class "star-bg-container"
                    , E.inFront
                        (E.el
                            [ E.width E.fill
                            , E.height E.fill
                            , EH.class "star-bg-outer"
                            ]
                            (E.el
                                [ E.width E.fill
                                , E.height E.fill
                                , EH.class "star-bg-inner"
                                ]
                                E.none
                            )
                        )
                    , E.inFront
                        (E.el
                            [ E.width E.fill
                            , E.height E.fill
                            , EH.class "star-bg-screen"
                            ]
                            E.none
                        )
                    ]
                    E.none
                )
            ]
            E.none

        --div [ class "star-bg-container" ]
        --  [ div [ class "star-bg" ] []
        --  , div [ class "star-bg-screen" ] []
        --  ]
        --, drawWinModal gameState
        --, drawInstructions 1 --FIXME
        --drawConstellation model
        --, drawLevelSelect numLevelsCleared gameState
        --, p [ class "instructions" ] [ text (Debug.toString model.mouseState) ]
        --, p [ class "instructions" ] [ text (Debug.toString model.graph) ]
        ]


viewCollapsedSidebar : Model -> Element Msg
viewCollapsedSidebar model =
    EH.btn
        [ E.alignLeft
        ]
        { onPress = Just ToggledCollapse
        , label =
            E.el
                [ E.padding 5 ]
                (E.text "Expand")
        , colors = Colors.baseBtnColors
        }


viewSidebar : Model -> Element Msg
viewSidebar model =
    let
        numCols =
            5

        numRows =
            5
    in
    E.column
        [ E.height E.fill
        , E.width <| E.px 250
        , E.paddingXY 15 10
        , E.spacing 20
        , EBackground.color (E.rgb 0 0 0.1)
        , EBackground.gradient
            { angle = pi
            , steps =
                [ E.rgb255 1 5 11
                , E.rgb255 26 50 93
                ]
            }
        , EBorder.color (E.rgb255 53 113 195)
        , EBorder.widthEach { sides | right = 2 }
        ]
        [ EH.btn
            []
            { onPress = Just ToggledCollapse
            , label =
                E.el
                    [ E.padding 5 ]
                    (E.text "Collapse")
            , colors = Colors.baseBtnColors
            }
        , E.el
            [ E.centerX
            , EFont.underline
            ]
            (E.text "Level Select")
        , E.column
            [ E.centerX
            ]
            (List.range 0 (numRows - 1)
                |> List.map
                    (\rowIndex ->
                        E.row []
                            (List.range 0 (numCols - 1)
                                |> List.map
                                    (\colIndex ->
                                        let
                                            lvlIndex =
                                                (numCols * rowIndex) + colIndex

                                            isLocked =
                                                lvlIndex > model.localStorage.numLevelsCleared
                                        in
                                        viewLevelSelectBtn isLocked lvlIndex model
                                    )
                            )
                    )
            )
        , EH.btn
            [ E.padding 5 ]
            { onPress = Just ClickedBackToTitle
            , label = E.text "Back to Title"
            , colors = Colors.baseBtnColors
            }
        , EH.btn
            [ E.padding 5 ]
            { onPress = Just ClickedResetLvl
            , label = E.text "Reset Level"
            , colors = Colors.redBtnColors
            }

        -- mascot
        , E.column
            [ E.alignBottom
            ]
            [ E.el
                [ class "speech-bubble"
                    |> E.htmlAttribute
                ]
                (E.column
                    [ E.padding 10
                    , E.spacing 10
                    , EFont.color (E.rgb 0 0 0.1)
                    ]
                    (getMascotSpeech model)
                )
            , let
                className =
                    if Model.hasWon model then
                        "mascot won"

                    else
                        "mascot"
              in
              E.el
                [ class className |> E.htmlAttribute
                ]
                E.none
            ]
        ]


getMascotSpeech : Model -> List (Element Msg)
getMascotSpeech model =
    let
        ( inProgText, winText ) =
            case model.currentLvlIndex of
                0 ->
                    ( "The stars are in disarray! Click and drag to untangle them so no lines cross."
                    , "Hooray! You did it!"
                    )

                1 ->
                    ( "This next level is more complex, but I believe in you!"
                    , "Great job!"
                    )

                2 ->
                    ( "Did you know you can lasso stars to move many at once?"
                    , "Yee haw!"
                    )

                3 ->
                    ( "This game is still a work in progress. All I've got left to say are astronomy facts. Did you know it takes our solar system 230 million years to complete an orbit around the Milky Way?"
                    , "Neato!"
                    )

                4 ->
                    ( "Enceladus is the sixth-larget moon of Saturn. It's mostly covered by fresh, clean ice making one of the most reflective bodies of our solar system."
                    , "Cool!"
                    )

                5 ->
                    ( "Space is big! If our sun was the size of a basketball, Earth would be 2.2mm wide, 29m meters away from the sun, or about the length of a basketball court. The nearest star would be 7000km away!"
                    , "Far out!"
                    )

                6 ->
                    ( "33 light years away is an exoplanet called Gliese 436 b, which is composed of burning ice! The surface temperature is 300° C, but the ice remain solid due to pressure."
                    , "Awesome!"
                    )

                7 ->
                    ( "Astronauts returning from space have reported interesting odors on their spacesuits, such as burning coal, wood, gasolate, and charcoal-broiled meat! These smells are due to polycyclic armoatic hydrocarbons which are by-products of dying stars."
                    , "Fresh!"
                    )

                8 ->
                    ( "Uranus rotates on its side. In our solar system, we love planets of all orientations!"
                    , "Aw yeah!"
                    )

                9 ->
                    ( "It takes our solar system 230 million years to complete an orbit around the Milky Way."
                    , "Woah!"
                    )

                10 ->
                    ( "The sun and moon look the same size in the sky, but they actually are not! The sun is 400 times larger than the moon, but also 400 times further away from us."
                    , "Far out!"
                    )

                11 ->
                    ( "The North Star is Polaris, but won't always be. In 12,000 years, Vega will replace it. This is because Earth's axis changes over a 26,000-year cycle."
                    , "Trippy!"
                    )

                12 ->
                    ( "Venus has a temperature of over 500° C, but also has a cold layer with temperatures of -175° C."
                    , "Wild!"
                    )

                13 ->
                    ( "Clouds at the center of the Milky Way are packed with ethyl formate, which smells like rum and gives raspberries their flavor and smell of rum!"
                    , "Delicious!"
                    )

                14 ->
                    ( "One day on Mercury lasts 59 Earth days, while a year lasts 88. Due to Mercury's eccentric orbit and alignment with the Sun, the length of time from sunrise to sunrise is equal to 176 Earth days — twice as long as a Mercurian year!"
                    , "Trippy!"
                    )

                15 ->
                    ( "One teaspoonful of a neutron star would weigh over a trillion kilograms! That's more than the weight of the entire human population!"
                    , "Heavy!"
                    )

                16 ->
                    ( "Gamma-ray bursts can release more energy in 10 seconds than our Sun will in its entire life. GRBs happen when a massive star implodes or when two neutron stars merge together."
                    , "Incredible!"
                    )

                17 ->
                    ( "Neptune takes 165 years to complete one full orbit around the sun."
                    , "Great!"
                    )

                18 ->
                    ( "It's believed that three of Jupiter's moons (Europa, Ganymede, and Callisto) and two of Saturn's (Enceladus and Titan) have underwater seas."
                    , "Spiffy!"
                    )

                19 ->
                    ( "The entire asteroid belt's total mass is just 3% of the Moon's mass."
                    , "Neato!"
                    )

                20 ->
                    ( "The Milky Way contains around 100 million stars."
                    , "Fantastic!"
                    )

                21 ->
                    ( "Saturn has a hexagonal-shaped storm centered around its north pole."
                    , "Bizarre!"
                    )

                22 ->
                    ( "Venus has around 1,600 volcanos. Its largest volcano is Maat Mons, which rises 8 km above the surface. Mars has the largest volcano: Olympus Mons, 25 km high!"
                    , "Hot!"
                    )

                23 ->
                    ( "On Mars, the temperature at your feet can be a temperate 24° C, but 0° C at your head! This is because the atmosphere is so thin, the heat from the Sun quickly escapes the planet."
                    , "Wowie!"
                    )

                24 ->
                    ( "This is the last level!"
                    , "You beat the game! I'm so proud of you."
                    )

                _ ->
                    ( "This level should not exist!", "But you beat it? Well done!" )
    in
    if Model.hasWon model then
        [ E.paragraph [ E.alpha 0.5 ] [ E.el [] (E.text inProgText) ]
        , E.paragraph [] [ E.el [] (E.text winText) ]
        , EH.btn
            [ EFont.color (E.rgb 1 1 1) ]
            { onPress = Just (ClickedGoToLevel (model.currentLvlIndex + 1))
            , label =
                E.el
                    [ E.padding 5 ]
                    (E.text "Go to Next Level")
            , colors = Colors.baseBtnColors
            }
        ]

    else
        [ E.paragraph [] [ E.el [] (E.text inProgText) ]
        ]


viewLevelSelectBtn : Bool -> Int -> Model -> Element Msg
viewLevelSelectBtn isLocked lvlIndex model =
    let
        ( onPress, text, colors ) =
            --if False then
            if isLocked then
                ( Nothing
                , "🔒"
                , Colors.baseBtnColors
                )

            else
                ( Just <| ClickedGoToLevel lvlIndex
                , String.fromInt <| (lvlIndex + 1)
                , if lvlIndex < model.localStorage.numLevelsCleared then
                    Colors.greenBtnColors

                  else
                    Colors.baseBtnColors
                )

        isActive =
            lvlIndex == model.currentLvlIndex

        borderBgColor =
            if isActive then
                E.rgb 1 1 0.5

            else
                E.rgba 0 0 0 0
    in
    E.el
        [ EBackground.color borderBgColor
        , EBorder.color borderBgColor
        , EBorder.rounded 7
        , EBorder.width 3
        ]
        (EH.btn
            [ EBorder.width 1
            ]
            { onPress = onPress
            , label =
                E.el
                    [ E.width <| E.px 30
                    , E.height <| E.px 30
                    , EFont.size 16
                    , EFont.bold
                    ]
                    (E.el
                        [ E.centerX
                        , E.centerY
                        ]
                        (E.text text)
                    )
            , colors = colors
            }
        )


drawConstellation : Model -> Element Msg
drawConstellation origModel =
    let
        ( width, height ) =
            ( origModel.canvasEl.width
            , origModel.canvasEl.height
            )

        aspectRatio =
            Model.getAspectRatio origModel

        model =
            origModel
                |> Model.applyAspectRatio aspectRatio
    in
    E.el
        [ E.width E.fill
        , E.height E.fill
        , E.clip
        , Attr.id Cfg.constellationContainerId
            |> E.htmlAttribute
        ]
        (Svg.svg
            [ SvgAttr.class "constellation"

            --, SvgAttr.viewBox "0 0 1 1"
            , SvgAttr.viewBox
                ([ 0
                 , 0
                 , Cfg.canvasScale * aspectRatio
                 , Cfg.canvasScale
                 ]
                    |> List.map String.fromFloat
                    |> String.join " "
                )

            --, SvgAttr.preserveAspectRatio "none"
            , style "width" (px width)
            , style "height" (px height)
            ]
            (List.concat
                [ drawDefs
                , drawShapes model
                , drawEdges model
                , drawNodes model.mouseState model.graph
                , drawLasso model.mouseState

                --, drawMouseDebug model
                ]
            )
            |> E.html
            |> E.el
                [ E.width E.fill
                , E.height E.fill
                , E.clip
                ]
        )


drawMouseDebug : Model -> List (Html Msg)
drawMouseDebug model =
    [ Svg.g
        [ SvgAttr.class "OOOOOOOOOOOOOOO" ]
        [ ellipse
            [ cx (px model.mousePos.x)
            , cy (px model.mousePos.y)
            , rx (String.fromFloat 30)
            , ry (String.fromFloat 30)
            , SvgAttr.style "fill: red; user-input: none"
            ]
            []
        ]
    ]


drawEdges : Model -> List (Html Msg)
drawEdges model =
    model.graph
        |> Graph.getEdges
        |> Dict.map (drawEdge model)
        |> Dict.values


drawEdge : Model -> Edge.Id -> Edge -> Html Msg
drawEdge model edgeId ( nodeId1, nodeId2 ) =
    let
        node1 =
            Graph.getNodeUnsafe nodeId1 model.graph

        node2 =
            Graph.getNodeUnsafe nodeId2 model.graph

        className =
            if Set.member edgeId model.intersectingEdges then
                "is-overlapping"

            else
                ""
    in
    line
        [ x1 (String.fromFloat node1.pos.x)
        , y1 (String.fromFloat node1.pos.y)
        , x2 (String.fromFloat node2.pos.x)
        , y2 (String.fromFloat node2.pos.y)
        , class ("edge " ++ className)
        , SvgAttr.stroke "#0ff"
        , SvgAttr.strokeWidth "0.01px"
        ]
        []


drawNodes : MouseState -> Graph -> List (Html Msg)
drawNodes mouseState graph =
    graph
        |> Graph.getNodes
        |> Dict.toList
        |> List.reverse
        -- for correct overlapping
        |> List.map (drawNode mouseState)


drawNode : MouseState -> ( Node.Id, Node ) -> Html Msg
drawNode mouseState ( nodeId, node ) =
    let
        nodeVel =
            node.vel

        stretch =
            baseStretch * nodeVel.r

        rad =
            Cfg.radius

        realXRad =
            rad + stretch

        realYRad =
            rad * (rad / realXRad)

        className =
            case mouseState of
                MouseState.Default ->
                    ""

                MouseState.Hovering hoveredId ->
                    if nodeId == hoveredId then
                        "is-hovering"

                    else
                        ""

                MouseState.Dragging draggedId _ neighborIds ->
                    if nodeId == draggedId then
                        "is-dragging"

                    else if Set.member nodeId neighborIds then
                        "is-neighboring"

                    else
                        ""

                MouseState.Lassoing startPos curPos nodeIds ->
                    if Set.member nodeId nodeIds then
                        "is-lassoing"

                    else
                        ""

                MouseState.Lassoed nodeIds ->
                    if Set.member nodeId nodeIds then
                        "is-lassoed"

                    else
                        ""

                MouseState.DraggingLassoed offsetNodeList ->
                    if Dict.member nodeId offsetNodeList then
                        "is-lassoing"

                    else
                        ""
    in
    Svg.g
        []
        [ ellipse
            [ cx (px node.pos.x)
            , cy (px node.pos.y)
            , rx (String.fromFloat realXRad)
            , ry (String.fromFloat realYRad)
            , transform (getTransform node)
            , class ("node " ++ className)
            ]
            []

        --, Svg.text_
        --    [ Svg.Attributes.color "red"
        --    , Svg.Attributes.x (String.fromFloat pos.x)
        --    , Svg.Attributes.y (String.fromFloat pos.y)
        --    , Svg.Attributes.fontSize "20"
        --    ]
        --    [ Svg.text nodeId ]
        ]


drawShapes : Model -> List (Html Msg)
drawShapes model =
    case model.mode of
        Mode.Won time shapes ->
            List.map drawShape shapes

        _ ->
            []


drawShape : Shape -> Html Msg
drawShape { dimmerAnimationDurationMs, shimmerAnimationDelayMs, pts, color } =
    let
        points =
            pts
                |> List.map (\{ x, y } -> String.fromFloat x ++ "," ++ String.fromFloat y)
                |> List.intersperse " "
                |> String.concat
    in
    Svg.g
        [ SvgAttr.class "shape-container"
        , SvgAttr.style ("animation-delay:" ++ String.fromInt shimmerAnimationDelayMs ++ "ms")
        ]
        [ polygon
            [ SvgAttr.points points
            , fill color
            , SvgAttr.class "shape"
            , SvgAttr.style ("animation-duration:" ++ String.fromInt dimmerAnimationDurationMs ++ "ms")
            ]
            []
        , polygon
            [ SvgAttr.points points
            , SvgAttr.class "shimmer"
            , SvgAttr.style ("animation-delay:" ++ String.fromInt shimmerAnimationDelayMs ++ "ms")
            ]
            []
        ]


onMouseDown : E.Attribute Msg
onMouseDown =
    Html.Events.on "mousedown"
        (Decode.map MouseDown decodeMousePos)
        |> E.htmlAttribute


decodeMousePos : Decode.Decoder Pos
decodeMousePos =
    Decode.map2 Pos
        (Decode.map2 (/)
            (Decode.at [ "offsetX" ] Decode.float)
            (Decode.at [ "target", "clientWidth" ] Decode.float)
        )
        (Decode.map2 (/)
            (Decode.at [ "offsetY" ] Decode.float)
            (Decode.at [ "target", "clientHeight" ] Decode.float)
        )


getLoadAnimPos : Float -> Int -> Int -> Pos
getLoadAnimPos time id numNodes =
    let
        age =
            if time < Cfg.wait then
                0

            else
                min Cfg.loadAnimDur (time - Cfg.wait)

        ease =
            Ease.outElastic (age / Cfg.loadAnimDur)

        easeRot =
            Ease.outCubic (age / Cfg.loadAnimDur)

        easeInv =
            1 - ease

        rotation =
            (toFloat id / toFloat numNodes) + (easeRot * 0.1)

        destX =
            Cfg.graphCenterX + cos (2 * pi * rotation) * Cfg.graphRadius

        destY =
            Cfg.graphCenterY + sin (2 * pi * rotation) * Cfg.graphRadius
    in
    Pos (ease * destX + easeInv * Cfg.graphCenterX)
        (ease * destY + easeInv * Cfg.graphCenterY)


drawLasso : MouseState -> List (Html Msg)
drawLasso mouseState =
    case mouseState of
        MouseState.Lassoing startPos curPos nodeIds ->
            let
                minX =
                    min startPos.x curPos.x

                minY =
                    min startPos.y curPos.y

                maxX =
                    max startPos.x curPos.x

                maxY =
                    max startPos.y curPos.y

                lassoWidth =
                    maxX - minX

                lassoHeight =
                    maxY - minY
            in
            [ rect
                [ x (px minX)
                , y (px minY)
                , width (px lassoWidth)
                , height (px lassoHeight)
                , class "lasso"
                ]
                []
            ]

        _ ->
            []


drawDefs : List (Html Msg)
drawDefs =
    [ Svg.defs []
        [ Svg.radialGradient
            [ cx "50%", cy "50%", r "100%", spreadMethod "pad", id "radGrad" ]
            [ Svg.stop [ offset "0%", stopColor "hsl(309, 100%, 75%)" ] []
            , Svg.stop [ offset "100%", stopColor "hsl(309, 100%, 65%)" ] []
            ]
        ]
    ]


getTransform : Node -> String
getTransform node =
    let
        angleStr =
            String.fromFloat (aToDegs node.vel.a)

        xStr =
            String.fromFloat node.pos.x

        yStr =
            String.fromFloat node.pos.y
    in
    "rotate (" ++ angleStr ++ " " ++ xStr ++ " " ++ yStr ++ ")"


aToDegs : Float -> Float
aToDegs a =
    angleConvert * a
