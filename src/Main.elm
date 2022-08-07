module Main exposing (main)

import App
import Browser
import View


main =
    Browser.element
        { init = App.init
        , view = View.view
        , update = App.update
        , subscriptions = App.subscriptions
        }
