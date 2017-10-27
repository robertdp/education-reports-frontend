module State exposing (..)

import Api
import Component.Layout as Layout
import Data.Record as Record
import Data.Recursive as Recursive
import Dict
import RemoteData exposing (RemoteData(..), WebData)
import Set
import Types exposing (..)


init : Flags -> ( Model, Cmd Msg )
init flags =
    { api = flags.api
    , courses = NotAsked
    , courseMap = Dict.empty
    , competingDivisions = Set.fromList flags.competingDivisions
    , employees = NotAsked
    , employeeMap = Dict.empty
    , organisations = NotAsked
    , organisationMap = Dict.empty
    , organisationSummaries = NotAsked
    , organisationSummaryMap = Dict.empty
    , report = SummaryReport
    , sidebar = SearchOrganisation
    , search = ""
    }
        ! [ Api.loadInitialData flags.api ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        InitialDataLoaded data ->
            let
                courses =
                    RemoteData.map .courses data

                courseMap =
                    courses
                        |> RemoteData.map (Record.toDict .id)
                        |> RemoteData.withDefault model.courseMap

                employees =
                    RemoteData.map .employees data

                employeeMap =
                    employees
                        |> RemoteData.map (Record.toDict .email)
                        |> RemoteData.withDefault model.employeeMap

                organisations =
                    RemoteData.map .organisations data

                organisationMap =
                    organisations
                        |> RemoteData.map (Record.toDict .id)
                        |> RemoteData.withDefault model.organisationMap

                organisationSummaries =
                    RemoteData.map .organisationSummaries data

                organisationSummaryMap =
                    organisationSummaries
                        |> RemoteData.map (Record.toDict2 .organisationId .courseId)
                        |> RemoteData.withDefault model.organisationSummaryMap
            in
                { model
                    | courses = courses
                    , courseMap = courseMap
                    , employees = employees
                    , employeeMap = employeeMap
                    , organisations = organisations
                    , organisationMap = organisationMap
                    , organisationSummaries = organisationSummaries
                    , organisationSummaryMap = organisationSummaryMap
                }
                    ! []

        EmployeeReportLoaded employee data ->
            let
                report =
                    case model.report of
                        EmployeeReport selectedEmployee _ ->
                            if selectedEmployee == employee then
                                EmployeeReport employee data
                            else
                                model.report

                        _ ->
                            model.report
            in
                { model
                    | report = report
                }
                    ! []

        OrganisationReportLoaded organisation data ->
            let
                report =
                    case model.report of
                        OrganisationReport selectedOrganisation _ ->
                            if selectedOrganisation == organisation then
                                OrganisationReport organisation data
                            else
                                model.report

                        _ ->
                            model.report
            in
                { model
                    | report = report
                }
                    ! []

        Search value ->
            { model | search = value } ! []

        ToggleSidebarMode ->
            let
                sidebar =
                    case model.sidebar of
                        SearchEmployee ->
                            SearchOrganisation

                        SearchOrganisation ->
                            SearchEmployee
            in
                { model
                    | sidebar = sidebar
                }
                    ! []

        SelectEmployee employee ->
            { model
                | report =
                    EmployeeReport employee Loading
            }
                ! [ Api.loadEnrolmentData model.api employee
                  , Layout.scrollContentToTop
                  ]

        SelectOrganisation organisation ->
            let
                newReport =
                    case model.report of
                        OrganisationCourseReport lastOrganisation _ data ->
                            if organisation == lastOrganisation then
                                OrganisationReport organisation data
                            else
                                OrganisationReport organisation Loading

                        _ ->
                            OrganisationReport organisation Loading
            in
                { model
                    | report =
                        newReport
                }
                    ! [ Api.loadOrganisationData model.api organisation
                      , Layout.scrollContentToTop
                      ]

        SelectCourse course ->
            let
                newModel =
                    case model.report of
                        OrganisationReport organisation data ->
                            { model
                                | report =
                                    OrganisationCourseReport organisation course data
                            }

                        _ ->
                            model
            in
                newModel
                    ! [ Layout.scrollContentToTop
                      ]

        DeselectCourse ->
            let
                newModel =
                    case model.report of
                        OrganisationCourseReport organisation _ data ->
                            { model
                                | report =
                                    OrganisationReport organisation data
                            }

                        _ ->
                            model
            in
                newModel
                    ! [ Layout.scrollContentToTop
                      ]

        SelectParentOrganisation ->
            let
                getParent organisation =
                    Recursive.getParent model.organisationMap organisation
                        |> Maybe.withDefault organisation

                newModel =
                    case model.report of
                        OrganisationReport organisation data ->
                            { model
                                | report =
                                    OrganisationReport (getParent organisation) data
                            }

                        OrganisationCourseReport organisation course data ->
                            { model
                                | report =
                                    OrganisationCourseReport (getParent organisation) course data
                            }

                        _ ->
                            model
            in
                newModel
                    ! [ Layout.scrollContentToTop
                      ]

        SelectOrganisationSummary ->
            { model | report = SummaryReport }
                ! [ Layout.scrollContentToTop ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
