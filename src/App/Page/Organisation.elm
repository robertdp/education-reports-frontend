module App.Page.Organisation exposing (..)

import App.Component.Loading as Loading
import App.Component.Navigation as Navigation
import App.Data exposing (..)
import Color exposing (..)
import Data.Record
import Data.Recursive
import Dict
import Element exposing (..)
import Element.Attributes exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Set
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Style.Sheet as Sheet


type alias Model =
    { enrolments : WebData (List Enrolment)
    , search : String
    , organisationNav : Navigation.Model Organisation
    , categoryNav : Navigation.Model Category
    , selectedOrganisation : Maybe Organisation
    , selectedCategory : Maybe Category
    }


type Msg
    = SelectCategory (Maybe Category)
    | SelectOrganisation (Maybe Organisation)
    | OrganisationNavigationMsg (Navigation.Msg Organisation)
    | CategoryNavigationMsg (Navigation.Msg Category)
    | LoadedEnrolments Organisation (WebData (List Enrolment))


type Styles
    = None
    | OrganisationReport
    | LoadingSpinner
    | EnrolmentStatus
    | EmployeeName
    | EmployeeNumber
    | CourseName
    | Blank
    | NavigationStyle Navigation.Styles


init : Model
init =
    { enrolments = NotAsked
    , search = ""
    , organisationNav = Navigation.init
    , categoryNav = Navigation.init
    , selectedCategory = Nothing
    , selectedOrganisation = Nothing
    }


styles : List (Style Styles variation)
styles =
    let
        map toStyle =
            Sheet.map toStyle identity >> Sheet.merge
    in
        [ style None []
        , style OrganisationReport
            [ Font.lineHeight 1.5
            , Color.background black
            ]
        , style EmployeeName
            [ Color.background white
            ]
        , style EmployeeNumber
            [ Color.background white ]
        , style CourseName
            [ Color.background white ]
        , style Blank
            [ Color.background white ]
        , style EnrolmentStatus
            [ Font.size 20
            , Font.typeface
                [ Font.monospace
                ]
            , Font.center
            , Color.background white
            ]
        , map NavigationStyle Navigation.styles
        ]


sidebar : Model -> WebData (List Organisation) -> WebData (List Category) -> WebData (List Course) -> Element Styles variation Msg
sidebar model organisations categories courses =
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
            |> el None
                [ height <| percent 50
                ]
        , categories
            |> Loading.simpleSpinner LoadingSpinner
                (always empty)
                (\categories ->
                    Navigation.view
                        { toId = .id
                        , toParentId = .parentId
                        , show = .name
                        , title = "Select a category"
                        }
                        categories
                        model.categoryNav
                        |> mapAll CategoryNavigationMsg NavigationStyle identity
                )
            |> el None
                [ height <| percent 50
                ]
        ]


showEnrolmentStatus : EnrolmentStatus -> Element Styles variation Msg
showEnrolmentStatus status =
    let
        wrap symbol =
            el EnrolmentStatus [] symbol
    in
        case status of
            Completed _ ->
                wrap <| text "○"

            Enrolled ->
                wrap <| text "△"

            _ ->
                wrap empty


view : Model -> List Organisation -> List Employee -> List Category -> List Course -> Element Styles variation Msg
view model organisations employees categories courses =
    let
        organisationMap =
            organisations
                |> Data.Record.toDict .id

        employeeMap =
            employees
                |> Data.Record.toDict .email

        categoyMap =
            categories
                |> Data.Record.toDict .id

        courseMap =
            courses
                |> Data.Record.toDict .id

        enrolmentMap =
            model.enrolments
                |> RemoteData.withDefault []
                |> Data.Record.toDict2 .employeeEmail .courseId

        showReport organisation category enrolments =
            let
                selectedEmployees =
                    organisation
                        |> Data.Recursive.getAllDescendants .id .parentId organisations
                        |> List.foldl (\organisation employeeEmails -> Set.union employeeEmails organisation.employeeEmails) organisation.employeeEmails
                        |> Set.toList
                        |> List.filterMap (\employeeEmail -> Dict.get employeeEmail employeeMap)
                        |> List.sortBy .name

                selectedCourses =
                    category
                        |> Data.Recursive.getWithAllDescendants .id .parentId categories
                        |> List.map (\category -> List.filter (.categoryId >> (==) category.id) courses)
                        |> List.concat
                        |> List.sortBy .name

                employeeDetails =
                    selectedEmployees
                        |> List.indexedMap
                            (\i employee ->
                                [ cell
                                    { start = ( 0, i + 1 )
                                    , width = 1
                                    , height = 1
                                    , content = el EmployeeName [ paddingXY 12 8 ] (text employee.name)
                                    }
                                , cell
                                    { start = ( 1, i + 1 )
                                    , width = 1
                                    , height = 1
                                    , content =
                                        employee.number
                                            |> Maybe.map (el EmployeeNumber [ paddingXY 12 8 ] << text)
                                            |> Maybe.withDefault empty
                                    }
                                ]
                            )
                        |> List.concat

                courseDetails =
                    selectedCourses
                        |> List.indexedMap
                            (\i course ->
                                cell
                                    { start = ( i + 2, 0 )
                                    , width = 1
                                    , height = 1
                                    , content =
                                        el CourseName
                                            [ paddingXY 12 8 ]
                                            (text course.name)
                                    }
                            )

                enrolmentDetails =
                    List.indexedMap
                        (\i employee ->
                            List.indexedMap
                                (\j course ->
                                    cell
                                        { start = ( j + 2, i + 1 )
                                        , width = 1
                                        , height = 1
                                        , content =
                                            enrolmentMap
                                                |> Dict.get employee.email
                                                |> Maybe.andThen (Dict.get course.id)
                                                |> Maybe.map (.status >> showEnrolmentStatus)
                                                |> Maybe.withDefault (showEnrolmentStatus NotEnrolled)
                                        }
                                )
                                selectedCourses
                        )
                        selectedEmployees
                        |> List.concat

                blankCells =
                    [ cell
                        { start = ( 0, 0 )
                        , width = 2
                        , height = 1
                        , content =
                            el Blank [] empty
                        }
                    ]
            in
                case ( selectedEmployees, selectedCourses ) of
                    ( [], _ ) ->
                        text "There are no employees in this organisation."

                    ( _, [] ) ->
                        text "There are no courses in this category."

                    _ ->
                        grid OrganisationReport
                            [ spacing 1
                            , padding 1
                            ]
                            { columns = []
                            , rows = []
                            , cells = employeeDetails ++ courseDetails ++ enrolmentDetails ++ blankCells
                            }
                            |> List.singleton
                            |> row None
                                [ width fill
                                , height fill
                                ]
    in
        Maybe.map2
            (\organisation category ->
                model.enrolments
                    |> Loading.simpleSpinner LoadingSpinner (toString >> text) (showReport organisation category)
            )
            model.selectedOrganisation
            model.selectedCategory
            |> Maybe.withDefault (text "Select an organisation and a category to begin.")


update : Url -> Msg -> Model -> ( Model, Cmd Msg )
update api msg model =
    case msg of
        OrganisationNavigationMsg msg_ ->
            let
                ( organisationNav, cmds ) =
                    Navigation.update SelectOrganisation msg_ model.organisationNav
            in
                { model | organisationNav = organisationNav } ! [ cmds ]

        CategoryNavigationMsg msg_ ->
            let
                ( categoryNav, cmds ) =
                    Navigation.update SelectCategory msg_ model.categoryNav
            in
                { model | categoryNav = categoryNav } ! [ cmds ]

        SelectOrganisation organisation ->
            { model
                | selectedOrganisation = organisation
                , enrolments = Loading
            }
                ! [ organisation
                        |> Maybe.map (\organisation -> loadOrganisationEnrolments (LoadedEnrolments organisation) api organisation)
                        |> Maybe.withDefault Cmd.none
                  ]

        SelectCategory category ->
            { model | selectedCategory = category } ! []

        LoadedEnrolments organisation enrolments ->
            if model.selectedOrganisation == Just organisation then
                { model | enrolments = enrolments } ! []
            else
                model ! []
