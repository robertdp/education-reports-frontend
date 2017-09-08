module State exposing (..)

import Api
import Component.Layout as Layout
import Dict
import RemoteData exposing (RemoteData(..), WebData)
import Types exposing (..)


init : Flags -> ( Model, Cmd Msg )
init flags =
    { courses = NotAsked
    , courseMap = Dict.empty
    , employees = NotAsked
    , employeeMap = Dict.empty
    , organisations = NotAsked
    , organisationMap = Dict.empty
    , report = Nothing
    , sidebar = SearchOrganisation
    , search = ""
    , api = flags.api
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
                    RemoteData.map
                        (List.foldl
                            (\course map ->
                                Dict.insert course.id course map
                            )
                            model.courseMap
                        )
                        courses
                        |> RemoteData.withDefault model.courseMap

                employees =
                    RemoteData.map .employees data

                employeeMap =
                    RemoteData.map
                        (List.foldl
                            (\employee map ->
                                Dict.insert employee.email employee map
                            )
                            model.employeeMap
                        )
                        employees
                        |> RemoteData.withDefault model.employeeMap

                organisations =
                    RemoteData.map .organisations data

                organisationMap =
                    RemoteData.map
                        (List.foldl
                            (\organisation map ->
                                Dict.insert organisation.id organisation map
                            )
                            model.organisationMap
                        )
                        organisations
                        |> RemoteData.withDefault model.organisationMap
            in
                { model
                    | courses = courses
                    , courseMap = courseMap
                    , employees = employees
                    , employeeMap = employeeMap
                    , organisations = organisations
                    , organisationMap = organisationMap
                }
                    ! []

        EmployeeReportLoaded employee data ->
            let
                report =
                    model.report
                        |> Maybe.map
                            (\report ->
                                case report of
                                    ForEmployee selectedEmployee _ ->
                                        if selectedEmployee == employee then
                                            ForEmployee employee data
                                        else
                                            report

                                    _ ->
                                        report
                            )
            in
                { model
                    | report = report
                }
                    ! []

        OrganisationReportLoaded organisation data ->
            let
                report =
                    model.report
                        |> Maybe.map
                            (\report ->
                                case report of
                                    ForOrganisation selectedOrganisation _ ->
                                        if selectedOrganisation == organisation then
                                            ForOrganisation organisation data
                                        else
                                            report

                                    _ ->
                                        report
                            )
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
                    , search = ""
                }
                    ! []

        SelectEmployee employee ->
            { model
                | report =
                    ForEmployee employee Loading
                        |> Just
            }
                ! [ Api.loadEnrolmentData model.api employee
                  , Layout.scrollContentToTop
                  ]

        SelectOrganisation organisation ->
            let
                newReport =
                    case model.report of
                        Just (ForCourse _ organisation data) ->
                            ForOrganisation organisation data

                        _ ->
                            ForOrganisation organisation Loading
            in
                { model
                    | report =
                        Just newReport
                }
                    ! [ Api.loadOrganisationData model.api organisation
                      , Layout.scrollContentToTop
                      ]

        SelectCourse course ->
            let
                newModel =
                    case model.report of
                        Just (ForOrganisation organisation (Success data)) ->
                            { model
                                | report =
                                    ForCourse course organisation (Success data)
                                        |> Just
                            }

                        _ ->
                            model
            in
                newModel
                    ! [ Layout.scrollContentToTop
                      ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
