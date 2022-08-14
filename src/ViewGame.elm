module ViewGame exposing (drawGameState)

import App
import AppState exposing (ActiveStateData, AppState)
import Cfg
import Config exposing (Config)
import Dict exposing (Dict)
import Ease
import Edge exposing (Edge)
import GameMode exposing (GameMode)
import Html exposing (Html, br, button, div, h1, h2, main_, p, span)
import Html.Attributes exposing (href, property, target)
import Html.Events exposing (on, onClick)
import Json.Decode as Decode
import Json.Encode
import MouseState exposing (MouseState)
import Msg exposing (Msg(..))
import Node exposing (Node)
import Pos exposing (Pos)
import Shape exposing (Shape)
import Svg exposing (..)
import Svg.Attributes exposing (class, cx, cy, dx, dy, fill, height, id, offset, r, rx, ry, spreadMethod, stdDeviation, stopColor, transform, viewBox, width, x, x1, x2, y, y1, y2)
import Svg.Events
import Vel exposing (Vel)
import ViewHelpers exposing (..)


baseStretch =
    5


baseBlur =
    0.8


angleConvert =
    180 / pi


drawGameState : Config -> Int -> ActiveStateData -> List (Html Msg)
drawGameState config levelsCleared gameState =
    [ drawInstructions gameState
    , drawWinModal gameState
    , div [ class "star-bg-container" ]
        [ div [ class "star-bg" ] []
        , div [ class "star-bg-screen" ] []
        ]
    , drawShapesContainer config gameState
    , drawConstellation config gameState
    , drawLevelSelect config levelsCleared gameState
    ]


drawLevelSelect : Config -> Int -> ActiveStateData -> Html Msg
drawLevelSelect config levelsCleared { difficulty } =
    div [ class "level-select-container" ]
        [ if difficulty > 1 then
            span
                [ class "level-select-picker level-select-prev"
                , onClick (ClickedGoToLevel (difficulty - 1))
                ]
                [ Html.text " <<< " ]

          else
            span
                [ property "innerHTML" (Json.Encode.string "&nbsp;")
                , class "level-select-picker level-select-prev"
                ]
                []
        , span [ class "level-select-level" ] [ text ("Level " ++ String.fromInt difficulty) ]
        , if levelsCleared >= difficulty then
            span
                [ class "level-select-picker level-select-next"
                , onClick (ClickedGoToLevel (difficulty + 1))
                ]
                [ Html.text " >>> " ]

          else
            span
                [ class "level-select-picker level-select-next"
                ]
                [ Html.text " ???" ]
        ]


drawShapesContainer : Config -> ActiveStateData -> Html Msg
drawShapesContainer config { mode } =
    case mode of
        GameMode.Won time shapes ->
            div [ class "shapes-container" ]
                [ svg
                    [ class "shapes"
                    , viewBox "0 0 1600 900"
                    ]
                    (List.map drawShape shapes)
                ]

        _ ->
            text ""


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
        [ Svg.Attributes.class "shape-container"
        , Svg.Attributes.style ("animation-delay:" ++ String.fromInt shimmerAnimationDelayMs ++ "ms")
        ]
        [ polygon
            [ Svg.Attributes.points points
            , fill color
            , Svg.Attributes.class "shape"
            , Svg.Attributes.style ("animation-duration:" ++ String.fromInt dimmerAnimationDurationMs ++ "ms")
            ]
            []
        , polygon
            [ Svg.Attributes.points points
            , Svg.Attributes.class "shimmer"
            , Svg.Attributes.style ("animation-delay:" ++ String.fromInt shimmerAnimationDelayMs ++ "ms")
            ]
            []
        ]


drawInstructions : ActiveStateData -> Html Msg
drawInstructions { difficulty } =
    case difficulty of
        1 ->
            p [ class "instructions" ] [ text "Please drag the stars so that no edges overlap!" ]

        2 ->
            p [ class "instructions" ] [ text "Thank you very much!" ]

        _ ->
            text ""


drawConstellation : Config -> ActiveStateData -> Html Msg
drawConstellation config { mouseState, nodes, edges, mode, difficulty } =
    let
        modClass =
            case mouseState of
                MouseState.Hovering _ ->
                    "is-hovering"

                MouseState.Dragging _ _ _ ->
                    "is-dragging"

                MouseState.Lassoing _ _ _ ->
                    "is-lassoing"

                _ ->
                    ""

        constellationGlassClass =
            "constellation-glass " ++ modClass
    in
    div [ class "constellation-container" ]
        [ svg
            [ class "constellation"
            , viewBox "0 0 1600 900"
            ]
            (List.concat
                [ drawDefs
                , drawEdges nodes edges
                , drawNodes config
                    mouseState
                    (List.reverse (Dict.values nodes))
                , drawLasso mouseState
                ]
            )
        , div
            [ class constellationGlassClass
            , onMouseMove
            , onMouseUp
            , onMouseDown
            ]
            []
        ]


drawDefs : List (Html Msg)
drawDefs =
    [ Svg.defs []
        [ Svg.radialGradient
            [ cx "50%", cy "50%", r "100%", spreadMethod "pad", id "radGrad" ]
            [ Svg.stop [ offset "0%", stopColor "rgb(255,255,255)" ] []
            , Svg.stop [ offset "30%", stopColor "rgb(255,255,250)" ] []
            , Svg.stop [ offset "66%", stopColor "rgb(255,255,94)" ] []
            , Svg.stop [ offset "76%", stopColor "rgb(255,243,13)" ] []
            , Svg.stop [ offset "100%", stopColor "rgb(255,243,13)" ] []
            ]
        ]
    ]


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


drawWinModal : ActiveStateData -> Html Msg
drawWinModal gameState =
    let
        isHidden =
            case gameState.mode of
                GameMode.Loading _ ->
                    True

                GameMode.Playing ->
                    True

                GameMode.Won time shapes ->
                    -- TODO
                    time > 3000

        className =
            if isHidden then
                "win-modal hidden"

            else
                "win-modal"

        nextDifficulty =
            gameState.difficulty + 1
    in
    div
        [ class className ]
        [ div
            [ class "win-modal-text" ]
            [ text "You did it!" ]
        , div
            [ class "win-modal-button"
            , Html.Events.onClick (ClickedGoToLevel nextDifficulty)
            ]
            [ text "Next Level" ]
        ]


drawNodes : Config -> MouseState -> List Node -> List (Html Msg)
drawNodes config mouseState nodesList =
    nodesList
        |> List.map (drawNode config mouseState)
        |> List.concat


drawNode : Config -> MouseState -> Node -> List (Html Msg)
drawNode config mouseState node =
    let
        realPosition =
            node.pos

        stretch =
            baseStretch * node.vel.r

        blur =
            getBlur node

        rad =
            config.radius

        realXRad =
            rad + stretch

        realYRad =
            rad * (rad / realXRad)

        className =
            case mouseState of
                MouseState.Default ->
                    ""

                MouseState.Hovering hoveredId ->
                    if node.id == hoveredId then
                        "is-hovering"

                    else
                        ""

                MouseState.Dragging draggedId pos neighborIds ->
                    if node.id == draggedId then
                        "is-dragging"

                    else if List.member node.id neighborIds then
                        "is-neighboring"

                    else
                        ""

                MouseState.Lassoing startPos curPos nodeIds ->
                    if List.member node.id nodeIds then
                        "is-lassoing"

                    else
                        ""

                MouseState.Lassoed nodeIds ->
                    if List.member node.id nodeIds then
                        "is-lassoed"

                    else
                        ""

                MouseState.DraggingLassoed offsetNodeList ->
                    if List.member node.id (List.map Tuple.first offsetNodeList) then
                        "is-lassoing"

                    else
                        ""
    in
    [ ellipse
        [ cx (px realPosition.x)
        , cy (px realPosition.y)
        , rx (String.fromFloat realXRad)
        , ry (String.fromFloat realYRad)
        , transform (getTransform node)
        , class ("node " ++ className)
        ]
        []

    --, Svg.text_
    --    [ Svg.Attributes.color "red"
    --    , Svg.Attributes.x (String.fromFloat realPosition.x)
    --    , Svg.Attributes.y (String.fromFloat realPosition.y)
    --    , Svg.Attributes.fontSize "20"
    --    ]
    --    [ Svg.text (node.id |> String.fromFloat) ]
    ]


drawEdges : Dict Node.Id Node -> List Edge -> List (Html Msg)
drawEdges nodes edges =
    List.concat (List.map (drawEdge nodes) edges)


drawEdge : Dict Node.Id Node -> Edge -> List (Html Msg)
drawEdge nodes edge =
    let
        node1 =
            App.getNode nodes (Tuple.first edge.pair)

        node2 =
            App.getNode nodes (Tuple.second edge.pair)

        className =
            --if List.isEmpty edge.overlappingEdges then
            --"is-overlapping"
            --else
            ""
    in
    [ line
        [ x1 (String.fromFloat node1.pos.x)
        , y1 (String.fromFloat node1.pos.y)
        , x2 (String.fromFloat node2.pos.x)
        , y2 (String.fromFloat node2.pos.y)
        , class ("edge " ++ className)
        ]
        []
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


getBlur : Node -> String
getBlur node =
    let
        xStr =
            String.fromFloat (baseBlur * node.vel.x)

        yStr =
            String.fromFloat (baseBlur * node.vel.y)
    in
    xStr ++ "," ++ yStr


aToDegs : Float -> Float
aToDegs a =
    angleConvert * a


onMouseMove : Attribute Msg
onMouseMove =
    on "mousemove" (Decode.map MouseMove decodeClickLocation)


onMouseDown : Attribute Msg
onMouseDown =
    on "mousedown" (Decode.map MouseDown decodeClickLocation)


onMouseUp : Attribute Msg
onMouseUp =
    on "mouseup" (Decode.map MouseUp decodeClickLocation)


decodeClickLocation : Decode.Decoder ( Float, Float )
decodeClickLocation =
    Decode.map2 (\a b -> ( a, b ))
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


posToNode : Int -> Pos -> Node
posToNode id pos =
    { id = id
    , dest = pos
    , pos = pos
    , vel = Vel 0 0 0 0
    }
