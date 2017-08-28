module State exposing (..)

import Api
import Dict
import RemoteData exposing (RemoteData(..), WebData)
import Types exposing (..)


init : Flags -> ( Model, Cmd Msg )
init flags =
    { employees = NotAsked
    , courses = NotAsked
    , organisations = NotAsked
    , organisationMap = Dict.empty
    , report = Nothing
    , sidebar = SearchIndividual
    , search = ""
    , api = flags.api
    }
        ! [ Api.loadInitialData flags.api ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitialDataLoaded data ->
            let
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
                    | employees = employees
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

        Search value ->
            { model | search = value } ! []

        ToggleSidebarMode ->
            let
                sidebar =
                    case model.sidebar of
                        SearchIndividual ->
                            SearchAggregate

                        SearchAggregate ->
                            SearchIndividual
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
            { model
                | report =
                    ForCourse course Loading
                        |> Just
            }
                ! [ Api.loadCourseData model.api course ]

        SelectOrganisation _ ->
            model ! []

        DeselectOrganisation ->
            model ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
