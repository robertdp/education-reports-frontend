module App.Page.Employee exposing (..)

import App.Component.Card as Card
import App.Component.Loading as Loading
import App.Data exposing (..)
import App.Util exposing (..)
import Color exposing (..)
import Date
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Element.Input as Input
import Html
import Html.Attributes
import RemoteData exposing (..)
import Style exposing (..)
import Style.Color as Color
import Style.Font as Font
import Style.Sheet as Sheet
import Tuple exposing (..)


type alias Model =
    { search : String
    , selectedEmployee : Maybe Employee
    , enrolments : WebData (List Enrolment)
    }


type Msg
    = NoOp
    | UpdateSearch String
    | SelectEmployee Employee
    | LoadedEnrolments Employee (WebData (List Enrolment))


type Styles
    = None
    | Search
    | LoadingSpinner
    | SearchResult
    | SearchEmployeeName
    | SearchEmployeeEmail
    | ReportEmployeeName
    | ReportEmployeeEmail
    | ResultRow
    | CardStyle Card.Styles


init : Model
init =
    { search = ""
    , selectedEmployee = Nothing
    , enrolments = NotAsked
    }


styles : List (Style Styles variation)
styles =
    let
        map toStyle =
            Sheet.map toStyle identity >> Sheet.merge
    in
        [ style None []
        , style Search
            [ Font.light
            , Font.size 16
            ]
        , style LoadingSpinner
            [ Color.text charcoal
            ]
        , style SearchResult
            [ cursor "pointer"
            , Font.lineHeight 1.5
            , hover
                [ Color.background lightYellow
                ]
            ]
        , style SearchEmployeeName
            [ Font.size 14
            ]
        , style SearchEmployeeEmail
            [ Color.text charcoal
            , Font.size 12
            ]
        , style ReportEmployeeName
            [ Font.size 18
            , Font.light
            , Font.lineHeight 1.5
            ]
        , style ReportEmployeeEmail
            [ Color.text charcoal
            , Font.size 14
            ]
        , style ResultRow
            [ cursor "default"
            , hover
                [ Color.background lightGrey
                ]
            ]
        , map CardStyle Card.styles
        ]


sidebar : Model -> WebData (List Employee) -> Element Styles variation Msg
sidebar model employees =
    let
        filter =
            case (String.trim >> String.toLower) model.search of
                "" ->
                    identity

                term ->
                    filterAny
                        [ .email >> String.toLower >> String.contains term
                        , .name >> String.toLower >> String.contains term
                        ]

        filteredEmployees =
            employees
                |> RemoteData.map (filter >> List.take 100)
    in
        column None
            [ height fill ]
            [ Input.search Search
                [ padding 5 ]
                { onChange = UpdateSearch
                , value = model.search
                , label =
                    Input.placeholder
                        { text = "Name or email"
                        , label = Input.hiddenLabel "Search"
                        }
                , options = []
                }
                |> el None [ paddingXY 8 12 ]
            , showSearchResults filteredEmployees
            ]


employeeIcon : Element style variation msg
employeeIcon =
    Html.i [ Html.Attributes.class ("fa fa-fw fa-2x fa-user") ] []
        |> html


showSearchResults : WebData (List Employee) -> Element Styles variation Msg
showSearchResults =
    Loading.simpleSpinner LoadingSpinner
        (toString >> text >> el None [])
        (List.map
            (\employee ->
                column SearchResult
                    [ paddingXY 12 8
                    , onClick <| SelectEmployee employee
                    , clip
                    ]
                    [ el SearchEmployeeName [] (text employee.name)
                    , el SearchEmployeeEmail [] (text employee.email)
                    ]
             -- |> (\item ->
             --         row SearchResult
             --             [ verticalCenter ]
             --             [ el EmployeeIcon [] employeeIcon
             --             , item
             --             ]
             --    )
            )
            >> column None
                [ height fill
                , yScrollbar
                ]
        )


view : List Course -> Model -> Element Styles variation Msg
view courses model =
    let
        enrolmentsWithCourses : WebData (List ( Enrolment, Course ))
        enrolmentsWithCourses =
            model.enrolments
                |> RemoteData.map
                    (List.filterMap
                        (\enrolment ->
                            courses
                                |> List.filter (.id >> (==) enrolment.courseId)
                                |> List.head
                                |> Maybe.map ((,) enrolment)
                        )
                        >> List.sortBy (second >> .shortName)
                    )

        courseResults =
            enrolmentsWithCourses
                |> Loading.simpleSpinner LoadingSpinner
                    (toString >> text >> el None [])
                    (\data ->
                        if List.isEmpty data then
                            text "No enrolments."
                        else
                            Card.view
                                { toStyle = CardStyle
                                , header = always <| resultRow None (text "Course") (text "Completed")
                                , content =
                                    List.map
                                        (\( enrolment, course ) ->
                                            resultRow ResultRow (text course.shortName) (formatEnrolment enrolment)
                                        )
                                        >> column None
                                            [ padding 5
                                            ]
                                }
                                data
                    )

        formatEnrolment enrolment =
            case enrolment.status of
                Completed date ->
                    [ Date.day date |> toString
                    , Date.month date |> toString
                    , Date.year date |> toString
                    ]
                        |> String.join " "
                        |> Element.text

                _ ->
                    Element.empty

        resultRow class left right =
            row class
                [ width fill
                , paddingXY 12 8
                ]
                [ el None [ width (fillPortion 3) ] left
                , el None [ width (fillPortion 1) ] right
                ]

        employeeReport employee =
            column None
                [ spacing 6 ]
                [ el ReportEmployeeName [] (text employee.name)
                , el ReportEmployeeEmail [ paddingBottom 16 ] (text employee.email)
                , courseResults
                ]
    in
        model.selectedEmployee
            |> Maybe.map employeeReport
            |> Maybe.withDefault empty


update : Url -> Msg -> Model -> ( Model, Cmd Msg )
update api msg model =
    case msg of
        NoOp ->
            model ! []

        UpdateSearch term ->
            if term /= model.search then
                { model | search = term } ! []
            else
                model ! []

        SelectEmployee employee ->
            if Just employee /= model.selectedEmployee then
                { model
                    | selectedEmployee = Just employee
                    , enrolments = Loading
                }
                    ! [ loadEmployeeEnrolments (LoadedEnrolments employee) api employee
                      ]
            else
                model ! []

        LoadedEnrolments employee enrolments ->
            if Just employee == model.selectedEmployee then
                { model | enrolments = enrolments } ! []
            else
                model ! []
