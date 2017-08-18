module Main exposing (..)

import Html exposing (Html)
import State exposing (init, subscriptions, update)
import Types exposing (Flags, Model, Msg)
import View exposing (view)


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
