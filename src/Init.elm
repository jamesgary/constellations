port module Init exposing (init)

import Dict
import Navigation
import State exposing (generateEdges)
import Types exposing (..)


--type alias Flags =
--    { radius : Float
--    , showStella : Bool
--    , levelsCleared : Int
--    , currentLevel :
--        Maybe
--            { nodes : Dict NodeId Node
--            , edges : List Edge
--            }
--    }


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init { radius, showStella, levelsCleared, currentLevelProgress } location =
    case location.hash of
        "#won" ->
            ( { appState = wonAppState
              , levelsCleared = levelsCleared
              , lastLevelProgress = currentLevelProgress
              , config =
                    { radius = radius
                    , showStella = showStella
                    }
              }
            , Cmd.none
            )

        _ ->
            ( { appState = StartState
              , levelsCleared = levelsCleared
              , lastLevelProgress = currentLevelProgress
              , config =
                    { radius = radius
                    , showStella = showStella
                    }
              }
            , Cmd.none
            )


wonAppState : AppState
wonAppState =
    ActiveState
        { nodes = Dict.fromList [ ( 0, { id = 0, dest = { x = 548.4424728036588, y = 472.8429285466048 }, pos = { x = 548.4424728036588, y = 472.8429285466048 }, vel = { x = 0, y = 0, r = 0, a = 0 } } ), ( 1, { id = 1, dest = { x = 526.9068004092718, y = 179.51291770229938 }, pos = { x = 526.9068004092718, y = 179.51291770229938 }, vel = { x = 0, y = 0, r = 0, a = 0 } } ), ( 2, { id = 2, dest = { x = 1045.3787571231821, y = 224.1248916801001 }, pos = { x = 1045.0484530280758, y = 225.09524842929196 }, vel = { x = 0.0066060819021282035, y = -0.019407134983838305, r = 0.02050066355946915, a = -1.2427042461174884 } } ), ( 3, { id = 3, dest = { x = 1060.3246988548367, y = 759.5125983698154 }, pos = { x = 1060.3246988548367, y = 759.5125983698154 }, vel = { x = 0, y = 0, r = 0, a = 0 } } ), ( 4, { id = 4, dest = { x = 432.84901556888116, y = 554.9729056428106 }, pos = { x = 432.84901556888116, y = 554.9729056428106 }, vel = { x = 0, y = 0, r = 0, a = 0 } } ), ( 5, { id = 5, dest = { x = 1339.7732924303025, y = 236.3311458383422 }, pos = { x = 1339.7732924303025, y = 236.3311458383422 }, vel = { x = 0, y = 0, r = 0, a = 0 } } ), ( 6, { id = 6, dest = { x = 566.5026143931373, y = 309.5699319996705 }, pos = { x = 566.5026143931373, y = 309.5699319996705 }, vel = { x = 0, y = 0, r = 0, a = 0 } } ), ( 7, { id = 7, dest = { x = 983.7098804421591, y = 631.620000893546 }, pos = { x = 983.7098804421591, y = 631.620000893546 }, vel = { x = 0, y = 0, r = 0, a = 0 } } ), ( 8, { id = 8, dest = { x = 397.5095542078131, y = 770.3135419823053 }, pos = { x = 397.5095542078131, y = 770.3135419823053 }, vel = { x = 0, y = 0, r = 0, a = 0 } } ), ( 9, { id = 9, dest = { x = 245.0391532637202, y = 511.9023526288006 }, pos = { x = 245.0391532637202, y = 511.9023526288006 }, vel = { x = 0, y = 0, r = 0, a = 0 } } ), ( 10, { id = 10, dest = { x = 695.4559409560273, y = 665.5449235739378 }, pos = { x = 695.4559409560273, y = 665.5449235739378 }, vel = { x = 0, y = 0, r = 0, a = 0 } } ), ( 11, { id = 11, dest = { x = 1424.6697909864029, y = 746.5681166924765 }, pos = { x = 1424.6697909864026, y = 746.5681166924763 }, vel = { x = 0, y = 0, r = 0, a = 0 } } ), ( 12, { id = 12, dest = { x = 924.9407642740173, y = 232.71037108198766 }, pos = { x = 924.9407642740173, y = 232.71037108198766 }, vel = { x = 0, y = 0, r = 0, a = 0 } } ), ( 13, { id = 13, dest = { x = 339.4570342924646, y = 570.9542997863954 }, pos = { x = 339.4570342924646, y = 570.9542997863954 }, vel = { x = 0, y = 0, r = 0, a = 0 } } ), ( 14, { id = 14, dest = { x = 724.3665159603863, y = 770.9570714533951 }, pos = { x = 724.3665159603863, y = 770.9570714533951 }, vel = { x = 0, y = 0, r = 0, a = 0 } } ), ( 15, { id = 15, dest = { x = 161.63252543342514, y = 605.2870822977004 }, pos = { x = 161.63252543342514, y = 605.2870822977004 }, vel = { x = 0, y = 0, r = 0, a = 0 } } ), ( 16, { id = 16, dest = { x = 1541.5875350116487, y = 447.27510831990014 }, pos = { x = 1541.5875350116485, y = 447.2751083199001 }, vel = { x = 0, y = 0, r = 0, a = 0 } } ), ( 17, { id = 17, dest = { x = 216.07977332449786, y = 779.4874276284088 }, pos = { x = 216.07977332449786, y = 779.4874276284088 }, vel = { x = 0, y = 0, r = 0, a = 0 } } ), ( 18, { id = 18, dest = { x = 419.2858158917934, y = 265.82709435718937 }, pos = { x = 419.2858158917934, y = 265.82709435718937 }, vel = { x = 0, y = 0, r = 0, a = 0 } } ), ( 19, { id = 19, dest = { x = 332.1368199292485, y = 703.268854161658 }, pos = { x = 332.1368199292485, y = 703.268854161658 }, vel = { x = 0, y = 0, r = 0, a = 0 } } ), ( 20, { id = 20, dest = { x = 384.50862156222445, y = 405.03006799892694 }, pos = { x = 384.50862156222445, y = 405.03006799892694 }, vel = { x = 0, y = 0, r = 0, a = 0 } } ), ( 21, { id = 21, dest = { x = 544.3800071982898, y = 675.1799991064539 }, pos = { x = 544.3800071982898, y = 675.1799991064539 }, vel = { x = 0, y = 0, r = 0, a = 0 } } ), ( 22, { id = 22, dest = { x = 704.5129177022993, y = 253.8864580176947 }, pos = { x = 704.5129177022993, y = 253.8864580176947 }, vel = { x = 0, y = 0, r = 0, a = 0 } } ), ( 23, { id = 23, dest = { x = 723.9496107812233, y = 143.29764737119947 }, pos = { x = 723.9496107812233, y = 143.29764737119947 }, vel = { x = 0, y = 0, r = 0, a = 0 } } ), ( 24, { id = 24, dest = { x = 769.7125983698155, y = 553.055076426062 }, pos = { x = 769.7125983698155, y = 553.055076426062 }, vel = { x = 0, y = 0, r = 0, a = 0 } } ), ( 25, { id = 25, dest = { x = 543.8695348562937, y = 772.6318833075236 }, pos = { x = 543.8695348562937, y = 772.6318833075236 }, vel = { x = 0, y = 0, r = 0, a = 0 } } ), ( 26, { id = 26, dest = { x = 268.76710089452183, y = 364.88962891801236 }, pos = { x = 268.76710089452183, y = 364.88962891801236 }, vel = { x = 0, y = 0, r = 0, a = 0 } } ), ( 27, { id = 27, dest = { x = 1219.1946510999667, y = 210.2457002138086 }, pos = { x = 1219.194651099967, y = 210.24570021380856 }, vel = { x = 0, y = 0, r = 0, a = 0 } } ) ]
        , edges = [ { id = 0, pair = ( 23, 1 ), overlappingEdges = [] }, { id = 1, pair = ( 1, 18 ), overlappingEdges = [] }, { id = 2, pair = ( 18, 26 ), overlappingEdges = [] }, { id = 3, pair = ( 26, 9 ), overlappingEdges = [] }, { id = 4, pair = ( 9, 15 ), overlappingEdges = [] }, { id = 5, pair = ( 15, 17 ), overlappingEdges = [] }, { id = 6, pair = ( 23, 22 ), overlappingEdges = [] }, { id = 7, pair = ( 22, 6 ), overlappingEdges = [] }, { id = 8, pair = ( 6, 20 ), overlappingEdges = [] }, { id = 9, pair = ( 20, 13 ), overlappingEdges = [] }, { id = 10, pair = ( 13, 19 ), overlappingEdges = [] }, { id = 11, pair = ( 19, 8 ), overlappingEdges = [] }, { id = 12, pair = ( 1, 22 ), overlappingEdges = [] }, { id = 13, pair = ( 22, 12 ), overlappingEdges = [] }, { id = 14, pair = ( 12, 0 ), overlappingEdges = [] }, { id = 15, pair = ( 0, 4 ), overlappingEdges = [] }, { id = 16, pair = ( 4, 21 ), overlappingEdges = [] }, { id = 17, pair = ( 21, 25 ), overlappingEdges = [] }, { id = 18, pair = ( 18, 6 ), overlappingEdges = [] }, { id = 19, pair = ( 6, 12 ), overlappingEdges = [] }, { id = 20, pair = ( 12, 2 ), overlappingEdges = [] }, { id = 21, pair = ( 2, 24 ), overlappingEdges = [] }, { id = 22, pair = ( 24, 10 ), overlappingEdges = [] }, { id = 23, pair = ( 10, 14 ), overlappingEdges = [] }, { id = 24, pair = ( 26, 20 ), overlappingEdges = [] }, { id = 25, pair = ( 20, 0 ), overlappingEdges = [] }, { id = 26, pair = ( 0, 2 ), overlappingEdges = [] }, { id = 27, pair = ( 2, 27 ), overlappingEdges = [] }, { id = 28, pair = ( 27, 7 ), overlappingEdges = [] }, { id = 29, pair = ( 7, 3 ), overlappingEdges = [] }, { id = 30, pair = ( 9, 13 ), overlappingEdges = [] }, { id = 31, pair = ( 13, 4 ), overlappingEdges = [] }, { id = 32, pair = ( 4, 24 ), overlappingEdges = [] }, { id = 33, pair = ( 24, 27 ), overlappingEdges = [] }, { id = 34, pair = ( 27, 5 ), overlappingEdges = [] }, { id = 35, pair = ( 5, 11 ), overlappingEdges = [] }, { id = 36, pair = ( 15, 19 ), overlappingEdges = [] }, { id = 37, pair = ( 19, 21 ), overlappingEdges = [] }, { id = 38, pair = ( 21, 10 ), overlappingEdges = [] }, { id = 39, pair = ( 10, 7 ), overlappingEdges = [] }, { id = 40, pair = ( 7, 5 ), overlappingEdges = [] }, { id = 41, pair = ( 5, 16 ), overlappingEdges = [] }, { id = 42, pair = ( 17, 8 ), overlappingEdges = [] }, { id = 43, pair = ( 8, 25 ), overlappingEdges = [] }, { id = 44, pair = ( 25, 14 ), overlappingEdges = [] }, { id = 45, pair = ( 14, 3 ), overlappingEdges = [] }, { id = 46, pair = ( 3, 11 ), overlappingEdges = [] }, { id = 47, pair = ( 11, 16 ), overlappingEdges = [] } ]
        , difficulty = 5
        , mouseState = HoveringMouseState 2
        , mode = PlayingMode
        }
