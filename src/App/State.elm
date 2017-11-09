module App.State exposing (..)

import App.Data exposing (..)
import Dict exposing (Dict)
import RemoteData exposing (RemoteData(NotAsked), WebData)
import Set exposing (Set)


type alias Flags =
    { api : String
    , competingDivisions : List String
    }


type alias Model =
    { api : String
    , employees : WebData (List Employee)
    , employeeMap : Dict Email Employee
    , competingDivisions : Set String
    , courses : WebData (List Course)
    , organisations : WebData (List Organisation)
    , organisationSummaries : WebData (List OrganisationSummary)
    }


type Msg
    = NoOp
    | LoadedInitialData (WebData InitialData)
    | DrawerSelectTab Int
    | DrawerUpdateFilter String


init : Flags -> ( Model, Cmd Msg )
init flags =
    { api = flags.api
    , employees = NotAsked
    , employeeMap = Dict.empty
    , competingDivisions = Set.fromList flags.competingDivisions
    , courses = NotAsked
    , organisations = NotAsked
    , organisationSummaries = NotAsked
    }
        ! [-- loadInitialData flags.api
          ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    model ! []
