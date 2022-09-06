module Game.Msg exposing (Msg(..))

import Browser.Dom
import Edge exposing (Edge)
import Graph exposing (Graph)
import Pos exposing (Pos)
import Set exposing (Set)
import Worker.WorkerToAppMsg as WorkerToAppMsg exposing (WorkerToAppMsg)


type Msg
    = Tick Float
      -- mouse stuff
    | MouseMove Pos
    | MouseDown Pos
    | MouseUp
    | ClickedGoToLevel Int
      -- dom stuff
    | GotContainerDom (Result Browser.Dom.Error Browser.Dom.Element)
    | ViewportResized
      -- incoming port subscriptions
    | GotWorkerMsg WorkerToAppMsg
