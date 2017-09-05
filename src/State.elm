module State exposing (..)

import Api
import Dict
import RemoteData exposing (RemoteData(..), WebData)
import Types exposing (..)


init : Flags -> ( Model, Cmd Msg )
init flags =
    { courses = NotAsked
    , courseMap = Dict.empty
    , employees = NotAsked
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
    case Debug.log "Msg" msg of
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
                    , organisations = organisations
                    , organisationMap = organisationMap
                }
                    ! []

        IndividualReportLoaded employee data ->
            let
                report =
                    model.report
                        |> Maybe.map
                            (\report ->
                                case report of
                                    ForIndividual selectedEmployee _ ->
                                        if selectedEmployee == employee then
                                            ForIndividual employee data
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
                    ForIndividual employee Loading
                        |> Just
            }
                ! [ Api.loadEnrolmentData model.api employee ]

        SelectCourse course ->
            model ! []

        SelectOrganisation organisation ->
            { model
                | report =
                    ForOrganisation organisation Loading
                        |> Just
            }
                ! [ Api.loadOrganisationData model.api organisation ]

        DeselectOrganisation ->
            model ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
