module View exposing (..)

import Component.Button as Button
import Component.CourseReport as CourseReport
import Component.DashboardCard as DashboardCard
import Component.EmployeeReport as EmployeeReport
import Component.EmployeeSidebar as EmployeeSidebar
import Component.Layout as Layout
import Component.OrganisationReport as OrganisationReport
import Component.OrganisationSidebar as OrganisationSidebar exposing (showOrganisation)
import Element exposing (Element, button, column, el, empty, inputText, row, text, viewport)
import Element.Attributes exposing (center, fill, height, padding, paddingXY, placeholder, px, scrollbars, spacing, verticalCenter, width, yScrollbar)
import Html
import RemoteData exposing (RemoteData(..), WebData)
import Style exposing (style)
import Types exposing (..)


type Style
    = None
    | ButtonStyle Button.Style
    | CourseReportStyle CourseReport.Style
    | DashboardCardStyle DashboardCard.Style
    | EmployeeReportStyle EmployeeReport.Style
    | EmployeeSidebarStyle EmployeeSidebar.Style
    | LayoutStyle Layout.Style
    | OrganisationReportStyle OrganisationReport.Style
    | OrganisationSidebarStyle OrganisationSidebar.Style


stylesheet : Style.StyleSheet Style variation
stylesheet =
    Style.styleSheet <|
        [ style None
            []
        ]
            ++ Button.styles ButtonStyle
            ++ CourseReport.styles CourseReportStyle
            ++ DashboardCard.styles DashboardCardStyle
            ++ EmployeeReport.styles EmployeeReportStyle
            ++ EmployeeSidebar.styles EmployeeSidebarStyle
            ++ Layout.styles LayoutStyle
            ++ OrganisationReport.styles OrganisationReportStyle
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
    let
        buttonText =
            case model.sidebar of
                SearchEmployee ->
                    "Employee mode"

                SearchOrganisation ->
                    "Organisation mode"

        sidebarView =
            case model.sidebar of
                SearchEmployee ->
                    EmployeeSidebar.view EmployeeSidebarStyle model

                SearchOrganisation ->
                    OrganisationSidebar.view OrganisationSidebarStyle model
    in
        column None
            [ height <| fill 1 ]
            [ el None
                [ padding 12 ]
                (Button.view
                    { style = ButtonStyle
                    , onClick =
                        ToggleSidebarMode
                    }
                    (text buttonText)
                )
            , sidebarView
            ]


content : Model -> Element Style variation Msg
content model =
    let
        showContent x =
            case x of
                Loading ->
                    text "Loading..."

                Success y ->
                    y

                Failure error ->
                    text <| toString error

                NotAsked ->
                    empty
    in
        case model.report of
            Just (ForEmployee employee data) ->
                RemoteData.map
                    (\enrolments ->
                        EmployeeReport.view EmployeeReportStyle
                            { courses = model.courseMap
                            , employee = employee
                            , enrolments = enrolments
                            }
                    )
                    data
                    |> showContent

            Just (ForOrganisation organisation data) ->
                RemoteData.map3
                    (\courses enrolments organisations ->
                        OrganisationReport.view OrganisationReportStyle
                            { courses = courses
                            , enrolments = enrolments
                            , organisations = organisations
                            , organisation = organisation
                            }
                    )
                    model.courses
                    data
                    model.organisations
                    |> showContent

            Just (ForCourse course organisation data) ->
                RemoteData.map
                    (\enrolments ->
                        CourseReport.view CourseReportStyle
                            { course = course
                            , employees = model.employeeMap
                            , enrolments = List.filter (\enrolment -> enrolment.courseId == course.id) enrolments
                            , organisation = organisation
                            , organisations = model.organisationMap
                            }
                    )
                    data
                    |> showContent

            _ ->
                empty


employeeImageUrl : Employee -> Maybe String
employeeImageUrl employee =
    Maybe.map (\number -> "http://kanlinux/WhosWho/images/" ++ number ++ ".jpg") employee.number
