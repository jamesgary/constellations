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
        ]
        [ viewSidebar model
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
        , E.width <| E.px 200
        , E.paddingXY 15 10
        , E.spacing 20
        , EBackground.color (E.rgb 0 0 0.1)
        ]
        [ EH.btn
            []
            { onPress = Nothing
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
            [ E.spacing 4
            , E.centerX
            ]
            (List.range 0 (numRows - 1)
                |> List.map
                    (\rowIndex ->
                        E.row [ E.spacing 4 ]
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

        -- mascot
        , E.paragraph
            [ E.alignBottom ]
            [ E.el []
                (E.text "Untangle all the stars so that no lines cross!")
            ]
        , E.el [] E.none
        ]


viewLevelSelectBtn : Bool -> Int -> Model -> Element Msg
viewLevelSelectBtn isLocked lvlIndex model =
    let
        ( onPress, text, colors ) =
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
    in
    EH.btn
        [ EBorder.width 1 ]
        { onPress = onPress
        , label =
            E.el
                [ E.width <| E.px 20
                , E.height <| E.px 20
                , EFont.size 14
                ]
                (E.el
                    [ E.centerX
                    , E.centerY
                    ]
                    (E.text text)
                )
        , colors = colors
        }


drawConstellation : Model -> Element Msg
drawConstellation origModel =
    let
        ( width, height ) =
            ( origModel.canvasEl.width
            , origModel.canvasEl.height
            )

        aspectRatio =
            width / height

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
