module App.State exposing (..)

import App.Component.Menu as Menu
import App.Page.User as User
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
    , competingDivisions : Set String
    , courses : WebData (List Course)
    , employeeMap : Dict Email Employee
    , employees : WebData (List Employee)
    , menu : Menu.Model
    , organisationSummaries : WebData (List OrganisationSummary)
    , organisations : WebData (List Organisation)
    , userPage : User.Model
    }


type Msg
    = MenuMsg Menu.Msg
    | UserMsg User.Msg


init : Flags -> ( Model, Cmd Msg )
init flags =
    { api = flags.api
    , competingDivisions = Set.fromList flags.competingDivisions
    , courses = NotAsked
    , employeeMap = Dict.empty
    , employees = NotAsked
    , menu = Menu.User
    , organisationSummaries = NotAsked
    , organisations = NotAsked
    , userPage = User.init
    }
        ! [-- loadInitialData flags.api
          ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MenuMsg msg_ ->
            { model | menu = Menu.update msg_ model.menu } ! []

        UserMsg msg_ ->
            let
                ( model_, cmds ) =
                    User.update msg_ model.userPage
            in
                { model | userPage = model_ }
                    ! [ cmds ]
