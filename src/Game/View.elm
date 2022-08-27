module Game.View exposing (view)

import Cfg
import Config exposing (Config)
import Dict exposing (Dict)
import Ease
import Edge exposing (Edge)
import Game.Mode as Mode exposing (Mode)
import Game.Model exposing (Model)
import Graph exposing (Graph)
import Html exposing (Html, br, button, div, h1, h2, main_, p, span)
import Html.Attributes exposing (href, property, target)
import Html.Events exposing (on, onClick)
import Json.Decode as Decode
import Json.Encode
import MouseState exposing (MouseState)
import Msg exposing (Msg(..))
import Node exposing (Node)
import Pos exposing (Pos)
import Set exposing (Set)
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


view : Int -> Model -> Html Msg
view numLevelsCleared model =
    Html.div []
        [ div [ class "star-bg-container" ]
            [ div [ class "star-bg" ] []
            , div [ class "star-bg-screen" ] []
            ]

        --, drawWinModal gameState
        , drawInstructions 1 --FIXME

        --, drawShapesContainer gameState
        , drawConstellation model

        --, drawLevelSelect numLevelsCleared gameState
        ]


drawInstructions : Int -> Html Msg
drawInstructions currentLvl =
    case currentLvl of
        1 ->
            p [ class "instructions" ] [ text "Please drag the stars so that no edges overlap!" ]

        2 ->
            p [ class "instructions" ] [ text "Thank you very much!" ]

        _ ->
            text ""


drawConstellation : Model -> Html Msg
drawConstellation model =
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
                , drawEdges model.graph
                , drawNodes model.mouseState model.graph
                , drawLasso model.mouseState
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


drawEdges : Graph -> List (Html Msg)
drawEdges graph =
    graph
        |> Graph.getEdges
        |> Dict.values
        |> List.map (drawEdge graph)


drawEdge : Graph -> Edge -> Html Msg
drawEdge graph ( id1, id2 ) =
    let
        node1 =
            Graph.getNodeUnsafe id1 graph

        node2 =
            Graph.getNodeUnsafe id2 graph

        className =
            --if List.isEmpty edge.overlappingEdges then
            --"is-overlapping"
            --else
            ""
    in
    line
        [ x1 (String.fromFloat node1.dest.x)
        , y1 (String.fromFloat node1.dest.y)
        , x2 (String.fromFloat node2.dest.x)
        , y2 (String.fromFloat node2.dest.y)
        , class ("edge " ++ className)
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
            Vel 0 0 0 0

        stretch =
            baseStretch * nodeVel.r

        blur =
            getBlur nodeVel

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
            [ cx (px node.dest.x)
            , cy (px node.dest.y)
            , rx (String.fromFloat realXRad)
            , ry (String.fromFloat realYRad)

            --, transform (getTransform node)
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



{-
    drawGameState : Config -> Int -> ActiveStateData -> List (Html Msg)
    drawGameState config numLevelsCleared gameState =


   drawLevelSelect : Config -> Int -> ActiveStateData -> Html Msg
   drawLevelSelect config numLevelsCleared { difficulty } =
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
           , if numLevelsCleared >= difficulty then
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
-}


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
            [ Svg.stop [ offset "0%", stopColor "rgb(255,255,255)" ] []
            , Svg.stop [ offset "30%", stopColor "rgb(255,255,250)" ] []
            , Svg.stop [ offset "66%", stopColor "rgb(255,255,94)" ] []
            , Svg.stop [ offset "76%", stopColor "rgb(255,243,13)" ] []
            , Svg.stop [ offset "100%", stopColor "rgb(255,243,13)" ] []
            ]
        ]
    ]


onMouseMove : Attribute Msg
onMouseMove =
    on "mousemove" (Decode.map MouseMove decodeClickLocation)


onMouseDown : Attribute Msg
onMouseDown =
    on "mousedown" (Decode.map MouseDown decodeClickLocation)


onMouseUp : Attribute Msg
onMouseUp =
    on "mouseup" (Decode.succeed MouseUp)


decodeClickLocation : Decode.Decoder Pos
decodeClickLocation =
    Decode.map2 Pos
        (Decode.map2 (/)
            (Decode.at [ "offsetX" ] Decode.float)
            (Decode.at [ "target", "clientWidth" ] Decode.float)
        )
        (Decode.map2 (/)
            (Decode.at [ "offsetY" ] Decode.float)
            (Decode.at [ "target", "clientHeight" ] Decode.float)
        )


getBlur : Vel -> String
getBlur vel =
    let
        xStr =
            String.fromFloat (baseBlur * vel.x)

        yStr =
            String.fromFloat (baseBlur * vel.y)
    in
    xStr ++ "," ++ yStr


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
