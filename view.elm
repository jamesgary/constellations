module View exposing (view)

import Animation
import Dict exposing (Dict)
import Html exposing (Html, div, span)
import Html.Attributes
import Html.Events exposing (on)
import Json.Decode as Json
import Mouse
import String exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)


-- mine

import Types exposing (..)


baseStretch =
    5


baseBlur =
    0.8


angleConvert =
    180 / pi


view : Model -> Html Msg
view model =
    div
        [ class "appState-container"
        ]
        [ drawWinModal (model)
        , svg
            [ width "100%"
            , height "100%"
            , onMouseMove
            , onMouseUp
            , onMouseDown
            ]
            (case model.appState of
                LoadingState ->
                    []

                ActiveState gameState ->
                    (List.concat
                        [ drawEdges gameState.nodes gameState.edges
                          --, drawNodes model.config gameState.mouseState (List.reverse (Dict.values gameState.nodes))
                        , drawNodes model.config gameState
                        ]
                    )
            )
        , drawLevelSelect (model)
        , drawConfig (model.config)
        ]


drawWinModal : Model -> Html Msg
drawWinModal model =
    case model.appState of
        LoadingState ->
            div [] []

        ActiveState gameState ->
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


drawNodes : Config -> GameState -> List (Html Msg)
drawNodes config gameState =
    List.reverse (List.map (drawNode config gameState) (Dict.keys gameState.nodes))


drawNode : Config -> GameState -> NodeId -> Html Msg
drawNode config gameState nodeId =
    let
        node =
            getNode gameState.nodes nodeId

        nodeStyle =
            getNodeStyle gameState.nodeStyles nodeId

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
    in
        ellipse
            (Animation.render nodeStyle
                ++ [ cx (px realPosition.x)
                   , cy (px realPosition.y)
                   , rx (toString realXRad)
                   , ry (toString realYRad)
                   , transform (getTransform node)
                   , class "node"
                   ]
            )
            []


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


drawLevelSelect : Model -> Html Msg
drawLevelSelect model =
    let
        currentDifficulty =
            case model.appState of
                LoadingState ->
                    -1

                ActiveState gameState ->
                    gameState.difficulty
    in
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
