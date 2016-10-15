module Main exposing (..)

import Html exposing (Html, div, text)
import Html.App as App
import Html.Events exposing (on)
import Json.Decode as Json exposing ((:=))
import Mouse exposing (Position)
import Svg exposing (..)
import Svg.Attributes exposing (..)


radius =
    50


main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { node : Node
    , mouse : Mouse
    }


type alias Node =
    { pos : Position
    }


type alias Mouse =
    { pos : Position
    }


init : ( Model, Cmd Msg )
init =
    ( { node = (Node (Position 200 200)), mouse = (Mouse (Position 200 200)) }, Cmd.none )



-- UPDATE


type Msg
    = MouseMove Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case msg of
        MouseMove newMousePos ->
            { node = { pos = newMousePos }
            , mouse = { pos = newMousePos }
            }
    , Cmd.none
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        realPosition =
            model.node.pos

        --getPosition model
    in
        svg
            [ width "800"
            , height "600"
            , viewBox "0 0 800 600"
            , onMouseMove
            ]
            [ circle
                [ cx (px realPosition.x)
                , cy (px realPosition.y)
                , r (toString radius)
                , fill "red"
                ]
                []
            ]


px : Int -> String
px number =
    toString number ++ "px"


onMouseMove : Attribute Msg
onMouseMove =
    on "mousemove" (Json.map MouseMove Mouse.position)
