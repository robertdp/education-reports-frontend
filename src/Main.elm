module Main exposing (main)

import App.State exposing (..)
import App.View exposing (..)
import Html


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
