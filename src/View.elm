module View exposing (..)

import Component.Button as Button
import Component.OrganisationCourseReport as OrganisationCourseReport
import Component.CardBlock as CardBlock
import Component.EmployeeReport as EmployeeReport
import Component.EmployeeSidebar as EmployeeSidebar
import Component.Layout as Layout
import Component.OrganisationReport as OrganisationReport
import Component.OrganisationSidebar as OrganisationSidebar
import Component.OrganisationSummaryReport as OrganisationSummaryReport
import Element exposing (Element, button, column, el, empty, inputText, row, text, viewport)
import Element.Attributes exposing (center, fill, height, padding, paddingXY, placeholder, px, scrollbars, spacing, verticalCenter, width, yScrollbar)
import Html
import RemoteData exposing (RemoteData(..), WebData)
import Style exposing (style)
import Types exposing (..)


type Style
    = None
    | ButtonStyle Button.Style
    | OrganisationCourseReportStyle OrganisationCourseReport.Style
    | CardBlockStyle CardBlock.Style
    | EmployeeReportStyle EmployeeReport.Style
    | EmployeeSidebarStyle EmployeeSidebar.Style
    | LayoutStyle Layout.Style
    | OrganisationReportStyle OrganisationReport.Style
    | OrganisationSidebarStyle OrganisationSidebar.Style
    | OrganisationSummaryReportStyle OrganisationSummaryReport.Style


stylesheet : Style.StyleSheet Style variation
stylesheet =
    Style.styleSheet <|
        [ style None
            []
        ]
            ++ Button.styles ButtonStyle
            ++ OrganisationCourseReport.styles OrganisationCourseReportStyle
            ++ CardBlock.styles CardBlockStyle
            ++ EmployeeReport.styles EmployeeReportStyle
            ++ EmployeeSidebar.styles EmployeeSidebarStyle
            ++ Layout.styles LayoutStyle
            ++ OrganisationReport.styles OrganisationReportStyle
            ++ OrganisationSidebar.styles OrganisationSidebarStyle
            ++ OrganisationSummaryReport.styles OrganisationSummaryReportStyle


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
                    model.employees
                        |> RemoteData.map
                            (\employees ->
                                EmployeeSidebar.view EmployeeSidebarStyle
                                    { employees = employees
                                    , search = model.search
                                    }
                            )
                        |> viewRemoteData

                SearchOrganisation ->
                    model.organisations
                        |> RemoteData.map
                            (\organisations ->
                                OrganisationSidebar.view OrganisationSidebarStyle
                                    { organisations = organisations }
                            )
                        |> viewRemoteData
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


viewRemoteData data =
    case data of
        Loading ->
            text "Loading..."

        Success y ->
            y

        Failure error ->
            text <| toString error

        NotAsked ->
            empty


content : Model -> Element Style variation Msg
content model =
    case model.report of
        EmployeeReport employee data ->
            RemoteData.map
                (\enrolments ->
                    EmployeeReport.view EmployeeReportStyle
                        { courses = model.courseMap
                        , employee = employee
                        , enrolments = enrolments
                        }
                )
                data
                |> viewRemoteData

        OrganisationReport organisation data ->
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
                |> viewRemoteData

        OrganisationCourseReport organisation course data ->
            RemoteData.map
                (\enrolments ->
                    OrganisationCourseReport.view OrganisationCourseReportStyle
                        { course = course
                        , employees = model.employeeMap
                        , enrolments = List.filter (\enrolment -> enrolment.courseId == course.id) enrolments
                        , organisation = organisation
                        , organisations = model.organisationMap
                        }
                )
                data
                |> viewRemoteData

        SummaryReport ->
            RemoteData.map3
                (\organisations courses summaries ->
                    OrganisationSummaryReport.view OrganisationSummaryReportStyle
                        { courses = courses
                        , organisations = organisations
                        , summaries = model.organisationSummaryMap
                        }
                )
                model.organisations
                model.courses
                model.organisationSummaries
                |> viewRemoteData


employeeImageUrl : Employee -> Maybe String
employeeImageUrl employee =
    Maybe.map (\number -> "http://kanlinux/WhosWho/images/" ++ number ++ ".jpg") employee.number
