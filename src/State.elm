module State exposing (..)

import Api
import RemoteData exposing (RemoteData(..), WebData)
import Types exposing (..)


init : Flags -> ( Model, Cmd Msg )
init flags =
    { data = NotAsked
    , report = Nothing
    , sidebarMode = True
    , api = flags.api
    }
        ! [ Api.loadInitialData flags.api ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    model ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
