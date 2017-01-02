module View exposing (view)

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
                        [ drawFilters
                        , drawEdges gameState.nodes gameState.edges
                        , drawNodes model.config gameState.mouseState (List.reverse (Dict.values gameState.nodes))
                        ]
                    )
            )
        , drawLevelSelect (model)
        , drawConfig (model.config)
        ]


drawFilters : List (Html Msg)
drawFilters =
    [ (Svg.filter
        [ id "node-shadow" ]
        [ feGaussianBlur [ stdDeviation "2.5" ] [] ]
      )
    , (Svg.radialGradient
        [ id "node-fill-default" ]
        [ Svg.stop [ offset "75%", stopColor "rgba(255, 255, 255, 1.0)" ] []
        , Svg.stop [ offset "85%", stopColor "rgba(220, 220, 220, 0.7)" ] []
        , Svg.stop [ offset "90%", stopColor "rgba(200, 200, 200, 0.3)" ] []
        , Svg.stop [ offset "95%", stopColor "rgba(180, 180, 180, 0.1)" ] []
        , Svg.stop [ offset "100%", stopColor "rgba(100, 100, 100, 0)" ] []
        ]
      )
    , (Svg.radialGradient
        [ id "node-fill-hover" ]
        [ Svg.stop [ offset "75%", stopColor "rgba(165, 205, 255, 1.0)" ] []
        , Svg.stop [ offset "85%", stopColor "rgba(180, 200, 220, 0.7)" ] []
        , Svg.stop [ offset "90%", stopColor "rgba(150, 200, 200, 0.3)" ] []
        , Svg.stop [ offset "95%", stopColor "rgba(120, 180, 180, 0.1)" ] []
        , Svg.stop [ offset "100%", stopColor "rgba(50, 100, 100, 0)" ] []
        ]
      )
    , (Svg.radialGradient
        [ id "node-fill-drag" ]
        [ Svg.stop [ offset "75%", stopColor "rgba(145, 145, 255, 1.0)" ] []
        , Svg.stop [ offset "85%", stopColor "rgba(120, 140, 220, 0.7)" ] []
        , Svg.stop [ offset "90%", stopColor "rgba(90, 140, 200, 0.3)" ] []
        , Svg.stop [ offset "95%", stopColor "rgba(80, 140, 180, 0.1)" ] []
        , Svg.stop [ offset "100%", stopColor "rgba(50, 100, 100, 0)" ] []
        ]
      )
    , (Svg.radialGradient
        [ id "node-fill-neighbors" ]
        [ Svg.stop [ offset "75%", stopColor "rgba(255, 205, 255, 1.0)" ] []
        , Svg.stop [ offset "85%", stopColor "rgba(255, 200, 220, 0.7)" ] []
        , Svg.stop [ offset "90%", stopColor "rgba(255, 200, 200, 0.3)" ] []
        , Svg.stop [ offset "95%", stopColor "rgba(120, 180, 180, 0.1)" ] []
        , Svg.stop [ offset "100%", stopColor "rgba(50, 100, 100, 0)" ] []
        ]
      )
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

        color =
            case mouseState of
                DefaultMouseState ->
                    "url(#node-fill-default)"

                HoveringMouseState hoveredId ->
                    if node.id == hoveredId then
                        "url(#node-fill-hover)"
                    else
                        "url(#node-fill-default)"

                DraggingMouseState draggedId pos neighborIds ->
                    if node.id == draggedId then
                        "url(#node-fill-drag)"
                    else if List.member node.id neighborIds then
                        "url(#node-fill-neighbors)"
                    else
                        "url(#node-fill-default)"
    in
        [ ellipse
            [ cx (px realPosition.x)
            , cy (px realPosition.y)
            , rx (toString realXRad)
            , ry (toString realYRad)
            , transform (getTransform node)
            , fill color
            , class "node"
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

        color =
            if List.isEmpty edge.overlappingEdges then
                "rgba(255,255,255,.5)"
            else
                "rgba(255,0,0,.5)"
    in
        [ line
            [ x1 (toString node1.pos.x)
            , y1 (toString node1.pos.y)
            , x2 (toString node2.pos.x)
            , y2 (toString node2.pos.y)
            , strokeWidth "3"
            , stroke color
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
