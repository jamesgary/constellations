module ViewGame exposing (drawGameState)

import Dict exposing (Dict)
import Html exposing (Html, div, span, h1, h2, main_, br)
import Html.Attributes exposing (href, target)
import Html.Events exposing (on)
import Json.Decode as Json
import Mouse
import String exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (width, height, class, x1, y1, x2, y2, transform, rx, ry, cx, cy, fill, x, y)


-- mine

import Types exposing (..)


baseStretch =
    5


baseBlur =
    0.8


angleConvert =
    180 / pi


drawGameState : Config -> GameState -> List (Html Msg)
drawGameState config gameState =
    if gameState.isSandbox then
        [ drawWinModal (gameState)
        , svg
            [ width "100%"
            , height "100%"
            , onMouseMove
            , onMouseUp
            , onMouseDown
            ]
            (List.concat
                [ drawEdges gameState.nodes gameState.edges
                , drawNodes config gameState.mouseState (List.reverse (Dict.values gameState.nodes))
                , drawLasso gameState.mouseState
                ]
            )
        , drawLevelSelect (gameState.difficulty)
        , drawConfig (config)
        ]
    else
        [ drawWinModal (gameState)
        , svg
            [ width "100%"
            , height "100%"
            , onMouseMove
            , onMouseUp
            , onMouseDown
            ]
            (List.concat
                [ drawEdges gameState.nodes gameState.edges
                , drawNodes config gameState.mouseState (List.reverse (Dict.values gameState.nodes))
                , drawLasso gameState.mouseState
                ]
            )
        ]


drawLasso : MouseState -> List (Html Msg)
drawLasso mouseState =
    case mouseState of
        LassoingMouseState startPos curPos nodeIds ->
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


drawWinModal : GameState -> Html Msg
drawWinModal gameState =
    let
        isHidden =
            if gameState.hasWon then
                False
            else
                True

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
                , Html.Events.onClick (GenerateEdges nextDifficulty)
                ]
                [ text "Next Level" ]
            ]


drawNodes : Config -> MouseState -> List Node -> List (Html Msg)
drawNodes config mouseState nodesList =
    (List.concat (List.map (drawNode config mouseState) nodesList))


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
                DefaultMouseState ->
                    ""

                HoveringMouseState hoveredId ->
                    if node.id == hoveredId then
                        "is-hovering"
                    else
                        ""

                DraggingMouseState draggedId pos neighborIds ->
                    if node.id == draggedId then
                        "is-dragging"
                    else if List.member node.id neighborIds then
                        "is-neighboring"
                    else
                        ""

                LassoingMouseState startPos curPos nodeIds ->
                    if List.member node.id nodeIds then
                        "is-lassoing"
                    else
                        ""

                LassoedMouseState nodeIds ->
                    if List.member node.id nodeIds then
                        "is-lassoed"
                    else
                        ""

                DraggingLassoedMouseState offsetNodeList ->
                    if List.member node.id (List.map Tuple.first offsetNodeList) then
                        "is-lassoing"
                    else
                        ""
    in
        [ ellipse
            [ cx (px realPosition.x)
            , cy (px realPosition.y)
            , rx (toString realXRad)
            , ry (toString realYRad)
            , transform (getTransform node)
            , class ("node " ++ className)
            ]
            []
        ]


drawEdges : Dict NodeId Node -> List Edge -> List (Html Msg)
drawEdges nodes edges =
    (List.concat (List.map (drawEdge nodes) edges))


drawEdge : Dict NodeId Node -> Edge -> List (Html Msg)
drawEdge nodes edge =
    let
        node1 =
            getNode nodes (Tuple.first edge.pair)

        node2 =
            getNode nodes (Tuple.second edge.pair)

        className =
            if List.isEmpty edge.overlappingEdges then
                "is-overlapping"
            else
                ""
    in
        [ line
            [ x1 (toString node1.pos.x)
            , y1 (toString node1.pos.y)
            , x2 (toString node2.pos.x)
            , y2 (toString node2.pos.y)
            , class ("edge " ++ className)
            ]
            []
        ]


drawLevelSelect : Int -> Html Msg
drawLevelSelect currentDifficulty =
    div
        [ class "levelSelect-container" ]
        (List.map (drawLevelSelector currentDifficulty) (List.range 1 50))


drawLevelSelector : Int -> Int -> Html Msg
drawLevelSelector currentDifficulty difficulty =
    let
        isMatching =
            currentDifficulty == difficulty
    in
        if isMatching then
            div
                [ class "levelSelect-selector levelSelect-selector-isCurrent"
                , Html.Events.onClick (GenerateEdges difficulty)
                ]
                [ text ("[[" ++ (toString difficulty) ++ "]]") ]
        else
            div
                [ class "levelSelect-selector"
                , Html.Events.onClick (GenerateEdges difficulty)
                ]
                [ text (toString difficulty) ]


drawConfig : Config -> Html Msg
drawConfig config =
    div
        [ class "config-container"
        ]
        [ div [ class "config-pair" ]
            [ Html.label
                [ class "config-label"
                ]
                [ text "Radius" ]
            , Html.input
                [ Html.Events.onInput ChangeConfigRadius
                , Html.Attributes.type_ "range"
                , Html.Attributes.min "1"
                , Html.Attributes.max "100"
                , Html.Attributes.defaultValue (toString config.radius)
                ]
                []
            , Html.span
                [ class "config-val"
                ]
                [ text (toString config.radius) ]
            ]
        ]


getTransform : Node -> String
getTransform node =
    let
        angleStr =
            toString (aToDegs node.vel.a)

        xStr =
            toString node.pos.x

        yStr =
            toString node.pos.y
    in
        "rotate (" ++ angleStr ++ " " ++ xStr ++ " " ++ yStr ++ ")"


getBlur : Node -> String
getBlur node =
    let
        xStr =
            toString (baseBlur * node.vel.x)

        yStr =
            toString (baseBlur * node.vel.y)
    in
        xStr ++ "," ++ yStr


aToDegs : Float -> Float
aToDegs a =
    angleConvert * a


px : Float -> String
px number =
    toString number ++ "px"


onMouseMove : Attribute Msg
onMouseMove =
    on "mousemove" (Json.map MouseMove Mouse.position)


onMouseDown : Attribute Msg
onMouseDown =
    on "mousedown" (Json.map MouseDown Mouse.position)


onMouseUp : Attribute Msg
onMouseUp =
    on "mouseup" (Json.map MouseUp Mouse.position)
