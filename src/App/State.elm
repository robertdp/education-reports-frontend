module App.State exposing (..)

import App.Component.Menu as Menu
import App.Page.Organisation as Organisation
import App.Page.Employee as Employee
import App.Data exposing (..)
import Data.Record as Record
import Dict exposing (Dict)
import RemoteData exposing (RemoteData(NotAsked), WebData)
import Set exposing (Set)


type alias Flags =
    { api : String
    , competingCourses : List String
    , competingDivisions : List String
    }


type alias Model =
    { api : String
    , categories : WebData (List Category)
    , competingCourses : Set String
    , competingCourseIds : Set Id
    , competingDivisions : Set String
    , competingDivisionIds : Set Id
    , courses : WebData (List Course)
    , employeeMap : Dict Email Employee
    , employees : WebData (List Employee)
    , menu : Menu.Model
    , organisationPage : Organisation.Model
    , organisationSummaryMap : Dict Id (Dict Id OrganisationSummary)
    , organisations : WebData (List Organisation)
    , employeePage : Employee.Model
    }


type Msg
    = MenuMsg Menu.Msg
    | EmployeeMsg Employee.Msg
    | OrganisationMsg Organisation.Msg
    | LoadedInitialData (WebData InitialData)


init : Flags -> ( Model, Cmd Msg )
init flags =
    { api = flags.api
    , categories = NotAsked
    , competingCourses = Set.fromList flags.competingCourses
    , competingCourseIds = Set.empty
    , competingDivisions = Set.fromList flags.competingDivisions
    , competingDivisionIds = Set.empty
    , courses = NotAsked
    , employeeMap = Dict.empty
    , employees = NotAsked
    , menu = Menu.init
    , organisationPage = Organisation.init
    , organisationSummaryMap = Dict.empty
    , organisations = NotAsked
    , employeePage = Employee.init
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

                competingCourseIds =
                    model.competingCourses
                        |> Set.toList
                        |> List.filterMap
                            (\courseName ->
                                courses
                                    |> RemoteData.withDefault []
                                    |> List.filter (\course -> course.name == courseName)
                                    |> List.head
                                    |> Maybe.map .id
                            )
                        |> Set.fromList

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
                    , competingCourseIds = competingCourseIds
                    , competingDivisionIds = competingDivisionIds
                    , courses = courses
                    , employees = employees
                    , employeeMap = employeeMap
                    , organisations = organisations
                    , organisationSummaryMap = organisationSummaryMap
                }
                    ! []
