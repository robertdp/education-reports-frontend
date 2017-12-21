module App.Page.Summary exposing (..)

import App.Component.Loading as Loading
import App.Component.Navigation as Navigation
import App.Data exposing (..)
import Color exposing (..)
import Data.Recursive as Recursive
import Data.Record as Record
import Dict exposing (Dict)
import Element exposing (..)
import Element.Attributes exposing (..)
import RemoteData exposing (..)
import Set exposing (Set)
import Style exposing (..)
import Style.Font as Font
import Style.Sheet as Sheet


type alias Model =
    { organisationNav : Navigation.Model Organisation
    , selectedOrganisation : Maybe Organisation
    , enrolments : WebData (List Enrolment)
    }


init : Model
init =
    { organisationNav = Navigation.init
    , selectedOrganisation = Nothing
    , enrolments = NotAsked
    }


type Msg
    = SelectOrganisation (Maybe Organisation)
    | OrganisationNavigationMsg (Navigation.Msg Organisation)
    | LoadedEnrolments Organisation (WebData (List Enrolment))


type Styles
    = None
    | Summary
    | DivisionName
    | CourseName
    | CourseCompletion
    | LoadingSpinner
    | NavigationStyle Navigation.Styles


styles : List (Style Styles variation)
styles =
    let
        map toStyle =
            Sheet.map toStyle identity >> Sheet.merge
    in
        [ style None []
        , style Summary
            [ Font.size 16
            , Font.light
            ]
        , style DivisionName
            [ Style.rotate (-pi / 4)
            ]
        , style CourseName
            []
        , style CourseCompletion
            []
        , map NavigationStyle Navigation.styles
        ]


sidebar : Model -> WebData (List Organisation) -> Element Styles variation Msg
sidebar model organisations =
    column None
        [ height fill
        ]
        [ organisations
            |> Loading.simpleSpinner LoadingSpinner
                (always empty)
                (\organisations ->
                    Navigation.view
                        { toId = .id
                        , toParentId = .parentId
                        , show = .name
                        , title = "Select an organisation"
                        }
                        organisations
                        model.organisationNav
                        |> mapAll OrganisationNavigationMsg NavigationStyle identity
                )
        ]


view : Model -> List Course -> Set Id -> List Organisation -> Dict Id (Dict.Dict Id OrganisationSummary) -> Element Styles variation msg
view model allCourses competingOrganisationIds allOrganisations competingDivisionSummaries =
    let
        courses : List Course
        courses =
            allCourses
                |> List.sortBy .shortName

        organisations : List Organisation
        organisations =
            case model.selectedOrganisation of
                Just organisation ->
                    let
                        children =
                            Recursive.getDirectDescendants .id .parentId allOrganisations organisation

                        effectiveParent =
                            if List.isEmpty children then
                                allOrganisations
                                    |> Record.toDict .id
                                    |> (\map -> Recursive.getParent .parentId map organisation)
                                    |> Maybe.withDefault organisation
                            else
                                organisation
                    in
                        Recursive.getDirectDescendants .id .parentId allOrganisations effectiveParent

                Nothing ->
                    allOrganisations
                        |> List.filter (\organisation -> Set.member organisation.id competingOrganisationIds)

        summaries : Dict Id (Dict.Dict Id OrganisationSummary)
        summaries =
            case model.selectedOrganisation of
                Just organisation ->
                    let
                        employeeEmailToOrganisationIdMap : Dict Email Id
                        employeeEmailToOrganisationIdMap =
                            organisations
                                |> List.foldl
                                    (\ancestor map ->
                                        ancestor
                                            |> Recursive.getWithAllDescendants .id .parentId allOrganisations
                                            |> List.foldl
                                                (\descendant map ->
                                                    descendant.employeeEmails
                                                        |> Set.foldl
                                                            (\email map -> Dict.insert email ancestor.id map)
                                                            map
                                                )
                                                map
                                    )
                                    Dict.empty

                        baseSummaries : Dict Id (Dict.Dict Id OrganisationSummary)
                        baseSummaries =
                            organisations
                                |> List.concatMap
                                    (\organisation ->
                                        courses
                                            |> List.map
                                                (\course ->
                                                    { organisationId = organisation.id
                                                    , courseId = course.id
                                                    , members =
                                                        employeeEmailToOrganisationIdMap
                                                            |> Dict.values
                                                            |> List.filter (\organisationId -> organisationId == organisation.id)
                                                            |> List.length
                                                    , enrolled = 0
                                                    , completed = 0
                                                    }
                                                )
                                    )
                                |> Record.toDict2 .organisationId .courseId

                        childrenSummaries : Dict Id (Dict.Dict Id OrganisationSummary)
                        childrenSummaries =
                            model.enrolments
                                |> RemoteData.map
                                    (\enrolments ->
                                        enrolments
                                            |> List.foldl
                                                (\enrolment summaries ->
                                                    employeeEmailToOrganisationIdMap
                                                        |> Dict.get enrolment.employeeEmail
                                                        |> Maybe.map
                                                            (\organisationId ->
                                                                summaries
                                                                    |> Dict.update organisationId
                                                                        (Maybe.map
                                                                            (Dict.update enrolment.courseId
                                                                                (Maybe.map
                                                                                    (\summary ->
                                                                                        case enrolment.status of
                                                                                            Enrolled ->
                                                                                                { summary | enrolled = summary.enrolled + 1 }

                                                                                            Completed _ ->
                                                                                                { summary | completed = summary.completed + 1 }

                                                                                            NotEnrolled ->
                                                                                                summary
                                                                                    )
                                                                                )
                                                                            )
                                                                        )
                                                            )
                                                        |> Maybe.withDefault summaries
                                                )
                                                baseSummaries
                                    )
                                |> RemoteData.withDefault Dict.empty
                    in
                        childrenSummaries

                Nothing ->
                    competingDivisionSummaries

        cells : List (OnGrid (Element Styles variation msg))
        cells =
            organisationHeaders ++ courseHeaders ++ results

        organisationHeaders : List (OnGrid (Element Styles variation msg))
        organisationHeaders =
            organisations
                |> List.indexedMap
                    (\x organisation ->
                        Element.text organisation.name
                            |> Element.el (None)
                                [ width <| px 300
                                , height <| px 50
                                ]
                            |> Element.el (DivisionName)
                                [ width <| px 210
                                , height <| px 50
                                ]
                            |> List.singleton
                            |> Element.row (None)
                                [ width <| px 50
                                , height <| px 210
                                , paddingTop 120
                                ]
                            |> (\content ->
                                    Element.cell
                                        { start = ( x + 1, 0 )
                                        , width = 1
                                        , height = 1
                                        , content = content
                                        }
                               )
                    )

        courseHeaders : List (OnGrid (Element Styles variation msg))
        courseHeaders =
            courses
                |> List.indexedMap
                    (\y course ->
                        Element.text course.shortName
                            |> Element.el (CourseName)
                                []
                            |> List.singleton
                            |> Element.row (None)
                                [ height <| px 50
                                , verticalCenter
                                , alignRight
                                ]
                            |> (\content ->
                                    Element.cell
                                        { start = ( 0, y + 1 )
                                        , width = 1
                                        , height = 1
                                        , content = content
                                        }
                               )
                    )

        results : List (OnGrid (Element Styles variation msg))
        results =
            organisations
                |> List.indexedMap
                    (\x organisation ->
                        courses
                            |> List.indexedMap
                                (\y course ->
                                    summaries
                                        |> Dict.get organisation.id
                                        |> Maybe.andThen (Dict.get course.id)
                                        |> Maybe.map
                                            (\summary ->
                                                let
                                                    percent =
                                                        (toFloat summary.completed / toFloat summary.members)
                                                            |> (*) 100
                                                            |> round

                                                    backgroundColor =
                                                        percent
                                                            |> toFloat
                                                            >> (*) 0.01
                                                            |> (\alpha ->
                                                                    Color.red
                                                                        |> Color.toRgb
                                                                        |> (\color ->
                                                                                "rgba("
                                                                                    ++ toString color.red
                                                                                    ++ ", "
                                                                                    ++ toString color.green
                                                                                    ++ ", "
                                                                                    ++ toString color.blue
                                                                                    ++ ", "
                                                                                    ++ toString alpha
                                                                                    ++ ")"
                                                                           )
                                                               )
                                                in
                                                    percent
                                                        |> toString
                                                        |> flip (++) "%"
                                                        |> Element.text
                                                        |> Element.el (CourseCompletion) []
                                                        |> List.singleton
                                                        |> Element.row (None)
                                                            [ verticalCenter
                                                            , center
                                                            , inlineStyle [ ( "backgroundColor", backgroundColor ) ]
                                                            ]
                                            )
                                        |> Maybe.withDefault Element.empty
                                        |> (\content ->
                                                Element.cell
                                                    { start = ( x + 1, y + 1 )
                                                    , width = 1
                                                    , height = 1
                                                    , content = content
                                                    }
                                           )
                                )
                    )
                |> List.concat
    in
        Element.row (None)
            []
            [ Element.grid (Summary)
                [ spacing 10
                ]
                { rows = []
                , columns = []
                , cells = cells
                }
            , Element.el (None) [ width fill ] empty
            ]


update : Url -> Msg -> Model -> ( Model, Cmd Msg )
update api msg model =
    case msg of
        OrganisationNavigationMsg msg_ ->
            let
                ( organisationNav, cmds ) =
                    Navigation.update SelectOrganisation msg_ model.organisationNav
            in
                { model | organisationNav = organisationNav } ! [ cmds ]

        SelectOrganisation organisation ->
            { model
                | selectedOrganisation = organisation
                , enrolments = Loading
            }
                ! [ organisation
                        |> Maybe.map (\organisation -> loadOrganisationEnrolments (LoadedEnrolments organisation) api organisation)
                        |> Maybe.withDefault Cmd.none
                  ]

        LoadedEnrolments organisation enrolments ->
            if model.selectedOrganisation == Just organisation then
                { model | enrolments = enrolments } ! []
            else
                model ! []
