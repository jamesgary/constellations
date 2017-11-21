module ViewGame exposing (drawGameState, drawLoadingAnim)

-- mine

import Dict exposing (Dict)
import Ease
import Html exposing (Html, br, button, div, h1, h2, main_, p, span)
import Html.Attributes exposing (href, target)
import Html.Events exposing (on)
import Json.Decode as Decode
import Mouse
import Svg exposing (..)
import Svg.Attributes exposing (class, cx, cy, fill, height, rx, ry, transform, viewBox, width, x, x1, x2, y, y1, y2)
import Svg.Events
import Time exposing (Time)
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
        [ drawWinModal gameState
        , drawConstellation config gameState
        , drawLevelSelect gameState.difficulty
        , drawConfig config
        ]
    else
        [ drawWinModal gameState
        , drawNarration gameState.isNarrationVisible gameState.difficulty
        , drawConstellation config gameState
        ]


drawNarration : Bool -> Int -> Html Msg
drawNarration isVisible difficulty =
    let
        className =
            if isVisible then
                "narration"
            else
                "narration is-hidden"
    in
    div
        [ class className ]
        [ p [] [ text (difficultyToNarration difficulty) ]
        , button [ class "btn", Html.Events.onClick CloseNarration ] [ text "OK" ]
        ]


drawConstellation : Config -> GameState -> Html Msg
drawConstellation config gameState =
    let
        modClass =
            case gameState.mouseState of
                HoveringMouseState _ ->
                    "is-hovering"

                DraggingMouseState _ _ _ ->
                    "is-dragging"

                LassoingMouseState _ _ _ ->
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
                [ drawEdges gameState.nodes gameState.edges
                , drawNodes config
                    gameState.mouseState
                    (List.reverse (Dict.values gameState.nodes))
                , drawLasso gameState.mouseState
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
    List.concat (List.map (drawEdge nodes) edges)


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
            [ text ("[[" ++ toString difficulty ++ "]]") ]
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
    on "mousemove" (Decode.map MouseMove decodeClickLocation)


onMouseDown : Attribute Msg
onMouseDown =
    on "mousedown" (Decode.map MouseDown decodeClickLocation)


onMouseUp : Attribute Msg
onMouseUp =
    on "mouseup" (Decode.map MouseUp decodeClickLocation)


decodeClickLocation : Decode.Decoder ( Float, Float )
decodeClickLocation =
    Decode.map2 (,)
        (Decode.map2 (/)
            (Decode.at [ "offsetX" ] Decode.float)
            (Decode.at [ "target", "clientWidth" ] Decode.float)
        )
        (Decode.map2 (/)
            (Decode.at [ "offsetY" ] Decode.float)
            (Decode.at [ "target", "clientHeight" ] Decode.float)
        )


difficultyToNarration : Int -> String
difficultyToNarration difficulty =
    case difficulty of
        1 ->
            "Untangle the stars so that no edges overlap."

        2 ->
            "Great job! Can you solve this one, too?"

        3 ->
            "Huzzah! There's three more constellations to untangle. Can you solve them all?"

        4 ->
            "You're over halfway there! Take my hand, we'll make it, I swear."

        5 ->
            "It's the final level! Get ready!"

        _ ->
            "I AM ERROR"


drawLoadingAnim : Config -> Time -> Int -> List (Html Msg)
drawLoadingAnim config age numNodes =
    [ drawLevelSelect numNodes
    , div [ class "constellation-container" ]
        [ svg
            [ class "constellation"
            , viewBox "0 0 1600 900"
            ]
            (List.range 0 (numNodes - 1)
                |> List.map
                    (\id ->
                        getLoadAnimPos age id numNodes
                            |> posToNode id
                            |> drawNode config DefaultMouseState
                    )
                |> List.concat
            )
        ]
    , drawConfig config
    ]


graphCenterX =
    800


graphCenterY =
    450


graphRadius =
    300


loadAnimDur =
    900


wait =
    300


getLoadAnimPos : Time -> Int -> Int -> Pos
getLoadAnimPos time id numNodes =
    let
        age =
            if time < wait then
                0
            else
                min loadAnimDur (time - wait)

        rotation =
            toFloat id / toFloat numNodes

        destX =
            graphCenterX + cos (2 * pi * rotation) * graphRadius

        destY =
            graphCenterY + sin (2 * pi * rotation) * graphRadius

        ease =
            Ease.outElastic (age / loadAnimDur)

        easeInv =
            1 - ease
    in
    Pos (ease * destX + easeInv * graphCenterX)
        (ease * destY + easeInv * graphCenterY)


posToNode : Int -> Pos -> Node
posToNode id pos =
    { id = id
    , dest = pos
    , pos = pos
    , vel = Vel 0 0 0 0
    }



--type alias Node =
--    { id : NodeId
--    , dest : Pos
--    , pos : Pos
--    , vel : Vel
--    }
--drawNode : Config -> MouseState -> Node -> List (Html Msg)
--drawNode config mouseState node =
--    let
--        realPosition =
--            node.pos
--
--        stretch =
--            baseStretch * node.vel.r
--
--        blur =
--            getBlur node
--
--        rad =
--            config.radius
--
--        realXRad =
--            rad + stretch
--
--        realYRad =
--            rad * (rad / realXRad)
--
--        className =
--            case mouseState of
--                DefaultMouseState ->
--                    ""
--
--                HoveringMouseState hoveredId ->
--                    if node.id == hoveredId then
--                        "is-hovering"
--                    else
--                        ""
--
--                DraggingMouseState draggedId pos neighborIds ->
--                    if node.id == draggedId then
--                        "is-dragging"
--                    else if List.member node.id neighborIds then
--                        "is-neighboring"
--                    else
--                        ""
--
--                LassoingMouseState startPos curPos nodeIds ->
--                    if List.member node.id nodeIds then
--                        "is-lassoing"
--                    else
--                        ""
--
--                LassoedMouseState nodeIds ->
--                    if List.member node.id nodeIds then
--                        "is-lassoed"
--                    else
--                        ""
--
--                DraggingLassoedMouseState offsetNodeList ->
--                    if List.member node.id (List.map Tuple.first offsetNodeList) then
--                        "is-lassoing"
--                    else
--                        ""
--    in
--    [ ellipse
--        [ cx (px realPosition.x)
--        , cy (px realPosition.y)
--        , rx (toString realXRad)
--        , ry (toString realYRad)
--        , transform (getTransform node)
--        , class ("node " ++ className)
--        ]
--        []
--    ]
