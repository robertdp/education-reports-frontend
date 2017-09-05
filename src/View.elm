module View exposing (..)

import Color
import Component.Button as Button
import Component.DashboardCard as DashboardCard
import Component.EmployeeSidebar as EmployeeSidebar
import Component.OrganisationSidebar as OrganisationSidebar
import Component.Layout as Layout
import Dict exposing (Dict)
import Element exposing (Element, button, column, el, empty, inputText, row, text, viewport)
import Element.Attributes exposing (center, fill, height, padding, paddingXY, placeholder, px, scrollbars, spacing, verticalCenter, width, yScrollbar)
import Element.Events exposing (onClick, onInput)
import Html
import RemoteData exposing (RemoteData(..), WebData)
import Style exposing (style)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Types exposing (..)


type Style
    = None
    | EmployeeName
    | EmployeeEmail
    | ReportHeading
    | CourseList
    | ButtonStyle Button.Style
    | DashboardCardStyle DashboardCard.Style
    | EmployeeSidebarStyle EmployeeSidebar.Style
    | LayoutStyle Layout.Style
    | OrganisationSidebarStyle OrganisationSidebar.Style


stylesheet : Style.StyleSheet Style variation
stylesheet =
    Style.styleSheet <|
        [ style None []
        , style EmployeeName
            [ Font.size 20
            ]
        , style EmployeeEmail
            [ Color.text Color.darkCharcoal
            ]
        , style ReportHeading
            [ Font.size 17
            ]
        , style CourseList
            [ Color.text Color.darkCharcoal
            ]
        ]
            ++ Button.styles ButtonStyle
            ++ DashboardCard.styles DashboardCardStyle
            ++ EmployeeSidebar.styles EmployeeSidebarStyle
            ++ Layout.styles LayoutStyle
            ++ OrganisationSidebar.styles OrganisationSidebarStyle


view : Model -> Html.Html Msg
view model =
    viewport stylesheet <|
        Layout.view
            { style = LayoutStyle
            , header = always <| el None [] (text "HUE Education Reports")
            , sidebar = sidebar
            , content = content
            }
            model


sidebar : Model -> Element Style variation Msg
sidebar model =
    case (model.sidebar) of
        SearchEmployee ->
            column None
                [ height <| fill 1 ]
                [ el None
                    [ padding 15 ]
                    (Button.view
                        { style = ButtonStyle
                        , onClick =
                            ToggleSidebarMode
                        }
                        (text "Individual Mode")
                    )
                , EmployeeSidebar.view EmployeeSidebarStyle model
                ]

        SearchOrganisation ->
            column None
                []
                [ el None
                    [ padding 15 ]
                    (Button.view
                        { style = ButtonStyle
                        , onClick =
                            ToggleSidebarMode
                        }
                        (text "Organisation Mode")
                    )
                , OrganisationSidebar.view OrganisationSidebarStyle model
                ]


content : Model -> Element Style variation Msg
content model =
    let
        show f a =
            case a of
                Loading ->
                    text "Loading..."

                Success data ->
                    f data model

                Failure error ->
                    text <| toString error

                NotAsked ->
                    empty
    in
        case model.report of
            Just (ForIndividual employee data) ->
                show (showIndividualReport employee) data

            _ ->
                empty


employeeImageUrl : Employee -> Maybe String
employeeImageUrl employee =
    Maybe.map (\number -> "http://kanlinux/WhosWho/images/" ++ number ++ ".jpg") employee.number


enrolledCourses : Dict Id Course -> List Enrolment -> List Course
enrolledCourses courseMap =
    List.filter (.status >> (==) Enrolled)
        >> List.filterMap (.courseId >> flip Dict.get courseMap)
        >> List.sortBy .name


showIndividualReport : Employee -> List Enrolment -> Model -> Element Style variation msg
showIndividualReport employee enrolments model =
    let
        employeeDetails =
            column None
                [ spacing 10 ]
                [ el EmployeeName [] (text employee.name)
                , el EmployeeEmail [] (text employee.email)
                ]

        getCoursesWhere predicate =
            enrolments
                |> List.filter predicate
                |> List.filterMap (.courseId >> flip Dict.get model.courseMap)
                |> List.sortBy .name

        enrolledCourses =
            getCoursesWhere (.status >> (==) Enrolled)

        completedCourses =
            getCoursesWhere
                (\enrolment ->
                    case enrolment.status of
                        Completed _ ->
                            True

                        _ ->
                            False
                )

        notEnrolledCourses =
            getCoursesWhere (.status >> (==) NotEnrolled)

        showCourses heading courses =
            if courses == [] then
                empty
            else
                DashboardCard.paddedView
                    { style = DashboardCardStyle
                    , header = always <| text heading
                    , content =
                        List.map (\course -> el None [] (text course.name))
                            >> column CourseList [ spacing 5 ]
                    }
                    courses

        -- column None
        --     [ spacing 10 ]
        --     [ el ReportHeading [] (text heading)
        --     , courses
        --         |> List.map (\course -> el None [] (text course.name))
        --         |> column CourseList [ spacing 5 ]
        --     ]
    in
        column None
            [ spacing 15 ]
            [ employeeDetails
            , showCourses "Enrolled" enrolledCourses
            , showCourses "Completed" completedCourses
            ]
