module App.State exposing (..)

import App.Component.Menu as Menu
import App.Page.User as User
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
    , competingCourses : Set String
    , competingCourseIds : Set Id
    , competingDivisions : Set String
    , competingDivisionIds : Set Id
    , courses : WebData (List Course)
    , employeeMap : Dict Email Employee
    , employees : WebData (List Employee)
    , menu : Menu.Model
    , organisationSummaryMap : Dict Id (Dict Id OrganisationSummary)
    , organisations : WebData (List Organisation)
    , userPage : User.Model
    }


type Msg
    = MenuMsg Menu.Msg
    | UserMsg User.Msg
    | InitialData (WebData InitialData)


init : Flags -> ( Model, Cmd Msg )
init flags =
    { api = flags.api
    , competingCourses = Set.fromList flags.competingCourses
    , competingCourseIds = Set.empty
    , competingDivisions = Set.fromList flags.competingDivisions
    , competingDivisionIds = Set.empty
    , courses = NotAsked
    , employeeMap = Dict.empty
    , employees = NotAsked
    , menu = Menu.Summary
    , organisationSummaryMap = Dict.empty
    , organisations = NotAsked
    , userPage = User.init
    }
        ! [ loadInitialData InitialData flags.api
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

        InitialData data ->
            let
                courses =
                    RemoteData.map .courses data

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
                        |> Debug.log "competingCourseIds"

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
                        |> Debug.log "competingDivisionIds"

                organisationSummaryMap =
                    data
                        |> RemoteData.map
                            (.organisationSummaries
                                >> Record.toDict2 .organisationId .courseId
                            )
                        |> RemoteData.withDefault model.organisationSummaryMap
            in
                { model
                    | competingCourseIds = competingCourseIds
                    , competingDivisionIds = competingDivisionIds
                    , courses = courses
                    , employees = employees
                    , employeeMap = employeeMap
                    , organisations = organisations
                    , organisationSummaryMap = organisationSummaryMap
                }
                    ! []
