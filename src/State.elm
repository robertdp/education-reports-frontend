module State exposing (..)

import Api
import RemoteData exposing (RemoteData(..), WebData)
import Types exposing (..)


init : Flags -> ( Model, Cmd Msg )
init flags =
    { data = NotAsked
    , report = Nothing
    , sidebar = SearchIndividual
    , search = ""
    , api = flags.api
    }
        ! [ Api.loadInitialData flags.api ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitialDataResponse data ->
            { model | data = data } ! []

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
                    ForIndividual employee NotAsked
                        |> (Just << Individual)
            }
                ! [ Api.loadEmployeeData model.api employee ]

        SelectCourse course ->
            { model
                | report =
                    ForCourse course NotAsked
                        |> (Just << Aggregate)
            }
                ! [ Api.loadCourseData model.api course ]

        _ ->
            model ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
