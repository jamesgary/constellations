module Types exposing (..)

import Dict exposing (Dict)
import Mouse
import Time


type AppState
    = StartState
    | LoadingCampaignState
    | LoadingState
    | ActiveState GameState


type MouseState
    = DefaultMouseState
    | HoveringMouseState NodeId
    | DraggingMouseState NodeId Pos (List NodeId)
    | LassoingMouseState Pos Pos (List NodeId)
    | LassoedMouseState (List NodeId)
    | DraggingLassoedMouseState (List ( NodeId, Pos ))


type alias Model =
    { appState : AppState
    , config : Config
    }


type alias Config =
    { radius : Float
    }


type alias GameState =
    { nodes : Dict NodeId Node
    , edges : List Edge
    , difficulty : Int
    , mouseState : MouseState
    , hasWon : Bool
    , isSandbox : Bool
    , isNarrationVisible : Bool
    }


type alias Node =
    { id : NodeId
    , dest : Pos
    , pos : Pos
    , vel : Vel
    }


type alias Edge =
    { id : EdgeId
    , pair : ( NodeId, NodeId )
    , overlappingEdges : List EdgeId
    }


type alias MousePos =
    ( Float, Float )


type alias Pos =
    { x : Float
    , y : Float
    }


type alias Vel =
    { x : Float
    , y : Float
    , r : Float
    , a : Float
    }


type alias NodeId =
    Int


type alias EdgeId =
    Int


type alias EdgeData =
    ( List Edge, Int, Int )



-- edges, numNodes, difficulty


type alias IntersectionResultData =
    ( Bool, List Edge )


type Msg
    = GenerateEdges Int
      -- numNodes
    | GeneratedEdges EdgeData
    | MouseDown MousePos
    | MouseMove MousePos
    | MouseUp MousePos
    | AnimationMsg Time.Time
      -- config stuff
      --| ChangeDifficulty String
    | ChangeConfigRadius String
    | GetIntersectionResults IntersectionResultData
    | StartCampaign
    | CloseNarration



-- Handy functions


getNode : Dict NodeId Node -> NodeId -> Node
getNode nodes nodeId =
    case Dict.get nodeId nodes of
        Just node ->
            node

        Nothing ->
            -- should never happen
            { id = -1
            , dest = Pos 42 42
            , pos = Pos 42 42
            , vel = Vel 0 0 0 0
            }
