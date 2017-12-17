port module Init exposing (init)

import Dict
import Navigation
import State exposing (generateEdges)
import Types exposing (..)


init : Config -> Navigation.Location -> ( Model, Cmd Msg )
init config location =
    case location.hash of
        "#won" ->
            ( wonModel, Cmd.none )

        "#load" ->
            ( { appState = StartState --LoadingState 0 12
              , config = config
              }
            , generateEdges 1
            )

        _ ->
            ( { appState = StartState
              , config = config
              }
            , Cmd.none
            )


wonModel : Model
wonModel =
    { appState =
        ActiveState
            { nodes =
                Dict.fromList
                    [ ( 0, { id = 0, dest = { x = 1154.0084067146672, y = 438.1498965726834 }, pos = { x = 1154.008406714667, y = 438.1498965726835 }, vel = { x = 0, y = 0, r = 0, a = 0 } } )
                    , ( 1
                      , { id = 1
                        , dest = { x = 874.6522088962215, y = 301.5331675653083 }
                        , pos = { x = 874.6522088962214, y = 301.53316756530836 }
                        , vel = { x = 0, y = 0, r = 0, a = 0 }
                        }
                      )
                    , ( 2
                      , { id = 2
                        , dest = { x = 1480.641620526776, y = 131.947820290893 }
                        , pos = { x = 1480.641620526776, y = 131.947820290893 }
                        , vel = { x = 0, y = 0, r = 0, a = 0 }
                        }
                      )
                    , ( 3, { id = 3, dest = { x = 1283.1177124303942, y = 383.3391798835496 }, pos = { x = 1283.117712430394, y = 383.3391798835496 }, vel = { x = 0, y = 0, r = 0, a = 0 } } )
                    , ( 4, { id = 4, dest = { x = 389.65414625603626, y = 99.27503170154817 }, pos = { x = 389.6541462560363, y = 99.27503170154816 }, vel = { x = 0, y = 0, r = 0, a = 0 } } )
                    , ( 5, { id = 5, dest = { x = 1094.760540624016, y = 671.5704569243678 }, pos = { x = 1094.7605406240157, y = 671.5704569243679 }, vel = { x = 0, y = 0, r = 0, a = 0 } } )
                    , ( 6, { id = 6, dest = { x = 1469.3955010914192, y = 350.2231354815361 }, pos = { x = 1469.3955010914192, y = 350.2231354815361 }, vel = { x = 0, y = 0, r = 0, a = 0 } } )
                    , ( 7, { id = 7, dest = { x = 1085.4934559648968, y = 190.96921275802802 }, pos = { x = 1085.493455964897, y = 190.96921275802805 }, vel = { x = 0, y = 0, r = 0, a = 0 } } )
                    , ( 8, { id = 8, dest = { x = 1330.331360428698, y = 274.8315664183461 }, pos = { x = 1330.331360428698, y = 274.8315664183461 }, vel = { x = 0, y = 0, r = 0, a = 0 } } )
                    , ( 9, { id = 9, dest = { x = 928.8768459645161, y = 396.2223509547858 }, pos = { x = 928.8768459645162, y = 396.22235095478584 }, vel = { x = 0, y = 0, r = 0, a = 0 } } )
                    , ( 10, { id = 10, dest = { x = 1173.9079905456888, y = 330.3046940882841 }, pos = { x = 1173.907990545689, y = 330.30469408828407 }, vel = { x = 0, y = 0, r = 0, a = 0 } } )
                    , ( 11, { id = 11, dest = { x = 1393.9956593577958, y = 444.6741243320615 }, pos = { x = 1393.9956593577958, y = 444.6741243320615 }, vel = { x = 0, y = 0, r = 0, a = 0 } } )
                    , ( 12, { id = 12, dest = { x = 1420.227547583191, y = 225.42285587381673 }, pos = { x = 1420.227547583191, y = 225.42285587381673 }, vel = { x = 0, y = 0, r = 0, a = 0 } } )
                    , ( 13, { id = 13, dest = { x = 982.2168721364308, y = 514.8204578772397 }, pos = { x = 982.2168721364307, y = 514.8204578772398 }, vel = { x = 0, y = 0, r = 0, a = 0 } } )
                    , ( 14, { id = 14, dest = { x = 483.23652362850487, y = 554.5765428432017 }, pos = { x = 508.96907406989965, y = 531.2421567800244 }, vel = { x = -0.5146510088278947, y = 0.4275572864809392, r = 0.6690821280758541, a = 2.4483683048260176 } } )
                    , ( 15, { id = 15, dest = { x = 1325.5787778633448, y = 126.02657036417185 }, pos = { x = 1325.5787778633448, y = 126.02657036417185 }, vel = { x = 0, y = 0, r = 0, a = 0 } } )
                    , ( 16, { id = 16, dest = { x = 682.9020714060185, y = 174.2211331021471 }, pos = { x = 682.9020714060186, y = 174.22113310214712 }, vel = { x = 0, y = 0, r = 0, a = 0 } } )
                    , ( 17, { id = 17, dest = { x = 312.65644861315906, y = 350.7039521151228 }, pos = { x = 312.6564486131591, y = 350.7039521151227 }, vel = { x = 0, y = 0, r = 0, a = 0 } } )
                    , ( 18, { id = 18, dest = { x = 768.9174315499818, y = 561.4856265750857 }, pos = { x = 768.9174315499819, y = 561.4856265750856 }, vel = { x = 0, y = 0, r = 0, a = 0 } } )
                    , ( 19, { id = 19, dest = { x = 990.1463599291824, y = 273.00673769005374 }, pos = { x = 990.1463599291825, y = 273.0067376900538 }, vel = { x = 0, y = 0, r = 0, a = 0 } } )
                    , ( 20, { id = 20, dest = { x = 1233.4732013442776, y = 600.3911892499198 }, pos = { x = 1233.4732013442779, y = 600.3911892499199 }, vel = { x = 0, y = 0, r = 0, a = 0 } } )
                    ]
            , edges =
                [ { id = 0, pair = ( 4, 17 ), overlappingEdges = [] }
                , { id = 1, pair = ( 17, 14 ), overlappingEdges = [] }
                , { id = 2, pair = ( 14, 18 ), overlappingEdges = [] }
                , { id = 3, pair = ( 18, 13 ), overlappingEdges = [] }
                , { id = 4, pair = ( 13, 5 ), overlappingEdges = [] }
                , { id = 5, pair = ( 4, 16 ), overlappingEdges = [] }
                , { id = 6, pair = ( 16, 1 ), overlappingEdges = [] }
                , { id = 7, pair = ( 1, 9 ), overlappingEdges = [] }
                , { id = 8, pair = ( 9, 0 ), overlappingEdges = [] }
                , { id = 9, pair = ( 0, 20 ), overlappingEdges = [] }
                , { id = 10, pair = ( 17, 16 ), overlappingEdges = [] }
                , { id = 11, pair = ( 16, 19 ), overlappingEdges = [] }
                , { id = 12, pair = ( 19, 10 ), overlappingEdges = [] }
                , { id = 13, pair = ( 10, 3 ), overlappingEdges = [] }
                , { id = 14, pair = ( 3, 11 ), overlappingEdges = [] }
                , { id = 15, pair = ( 14, 1 ), overlappingEdges = [] }
                , { id = 16
                  , pair = ( 1, 19 )
                  , overlappingEdges = []
                  }
                , { id = 17, pair = ( 19, 7 ), overlappingEdges = [] }
                , { id = 18, pair = ( 7, 8 ), overlappingEdges = [] }
                , { id = 19, pair = ( 8, 6 ), overlappingEdges = [] }
                , { id = 20, pair = ( 18, 9 ), overlappingEdges = [] }
                , { id = 21, pair = ( 9, 10 ), overlappingEdges = [] }
                , { id = 22, pair = ( 10, 7 ), overlappingEdges = [] }
                , { id = 23, pair = ( 7, 15 ), overlappingEdges = [] }
                , { id = 24, pair = ( 15, 2 ), overlappingEdges = [] }
                , { id = 25, pair = ( 13, 0 ), overlappingEdges = [] }
                , { id = 26, pair = ( 0, 3 ), overlappingEdges = [] }
                , { id = 27, pair = ( 3, 8 ), overlappingEdges = [] }
                , { id = 28, pair = ( 8, 15 ), overlappingEdges = [] }
                , { id = 29, pair = ( 15, 12 ), overlappingEdges = [] }
                , { id = 30, pair = ( 5, 20 ), overlappingEdges = [] }
                , { id = 31, pair = ( 20, 11 ), overlappingEdges = [] }
                , { id = 32, pair = ( 11, 6 ), overlappingEdges = [] }
                , { id = 33, pair = ( 6, 2 ), overlappingEdges = [] }
                , { id = 34, pair = ( 2, 12 ), overlappingEdges = [] }
                ]
            , difficulty = 4
            , mouseState = DefaultMouseState
            , mode = PlayingMode
            }
    , config = { radius = 27, showStella = False }
    }
