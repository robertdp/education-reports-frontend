module App.State exposing (..)

import App.Component.Menu as Menu
import App.Page.Organisation as Organisation
import App.Page.Summary as Summary
import App.Page.Employee as Employee
import App.Data exposing (..)
import Data.Record as Record
import Dict exposing (Dict)
import RemoteData exposing (RemoteData(NotAsked), WebData)
import Set exposing (Set)


type alias Flags =
    { api : String
    , competingDivisions : List String
    }


type alias Model =
    { api : String
    , categories : WebData (List Category)
    , competingCourseIds : Set Id
    , competingDivisions : Set String
    , competingDivisionIds : Set Id
    , courses : WebData (List Course)
    , employeeMap : Dict Email Employee
    , employeePage : Employee.Model
    , employees : WebData (List Employee)
    , menu : Menu.Model
    , organisationPage : Organisation.Model
    , organisationSummaryMap : Dict Id (Dict Id OrganisationSummary)
    , organisations : WebData (List Organisation)
    , summaryPage : Summary.Model
    }


type Msg
    = MenuMsg Menu.Msg
    | SummaryMsg Summary.Msg
    | OrganisationMsg Organisation.Msg
    | EmployeeMsg Employee.Msg
    | LoadedInitialData (WebData InitialData)


init : Flags -> ( Model, Cmd Msg )
init flags =
    { api = flags.api
    , categories = NotAsked
    , competingCourseIds = Set.empty
    , competingDivisions = Set.fromList flags.competingDivisions
    , competingDivisionIds = Set.empty
    , courses = NotAsked
    , employeeMap = Dict.empty
    , employeePage = Employee.init
    , employees = NotAsked
    , menu = Menu.init
    , organisationPage = Organisation.init
    , organisationSummaryMap = Dict.empty
    , organisations = NotAsked
    , summaryPage = Summary.init
    }
        ! [ loadInitialData LoadedInitialData flags.api
          ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MenuMsg msg_ ->
            { model | menu = Menu.update msg_ model.menu } ! []

        SummaryMsg msg_ ->
            let
                ( model_, cmds ) =
                    Summary.update model.api msg_ model.summaryPage
            in
                { model | summaryPage = model_ }
                    ! [ Cmd.map SummaryMsg cmds ]

        EmployeeMsg msg_ ->
            let
                ( model_, cmds ) =
                    Employee.update model.api msg_ model.employeePage
            in
                { model | employeePage = model_ }
                    ! [ Cmd.map EmployeeMsg cmds ]

        OrganisationMsg msg_ ->
            let
                ( model_, cmds ) =
                    Organisation.update model.api msg_ model.organisationPage
            in
                { model | organisationPage = model_ }
                    ! [ Cmd.map OrganisationMsg cmds ]

        LoadedInitialData data ->
            let
                courses =
                    RemoteData.map .courses data

                categories =
                    RemoteData.map .categories data

                employees =
                    RemoteData.map .employees data

                employeeMap =
                    employees
                        |> RemoteData.map (Record.toDict .email)
                        |> RemoteData.withDefault model.employeeMap

                organisations =
                    RemoteData.map .organisations data

                competingDivisionIds =
                    model.competingDivisions
                        |> Set.toList
                        |> List.filterMap
                            (\divisionName ->
                                organisations
                                    |> RemoteData.withDefault []
                                    |> List.filter
                                        (\organisation ->
                                            organisation.name == divisionName && organisation.parentId == Nothing
                                        )
                                    |> List.head
                                    |> Maybe.map .id
                            )
                        |> Set.fromList

                organisationSummaryMap =
                    data
                        |> RemoteData.map
                            (.organisationSummaries
                                >> Record.toDict2 .organisationId .courseId
                            )
                        |> RemoteData.withDefault model.organisationSummaryMap
            in
                { model
                    | categories = categories
                    , competingDivisionIds = competingDivisionIds
                    , courses = courses
                    , employees = employees
                    , employeeMap = employeeMap
                    , organisations = organisations
                    , organisationSummaryMap = organisationSummaryMap
                }
                    ! []
