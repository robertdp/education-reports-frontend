module App.View exposing (..)

import App.Component.Layout as Layout
import App.Component.Menu as Menu
import App.Page.Summary as Summary
import App.Page.Organisation as Organisation
import App.Page.Employee as Employee
import App.State exposing (..)
import App.Util exposing (makeLazy)
import Element exposing (..)
import Html exposing (Html)
import RemoteData
import Style exposing (..)
import Style.Sheet as Sheet


type Styles
    = None
    | LayoutStyle Layout.Styles
    | MenuStyle Menu.Styles
    | EmployeeStyle Employee.Styles
    | SummaryStyle Summary.Styles
    | OrganisationStyle Organisation.Styles


styles : List (Style Styles variation)
styles =
    let
        map toStyle =
            Sheet.map toStyle identity >> Sheet.merge
    in
        [ Style.style None []
        , map LayoutStyle Layout.styles
        , map MenuStyle Menu.styles
        , map EmployeeStyle Employee.styles
        , map SummaryStyle Summary.styles
        , map OrganisationStyle Organisation.styles
        ]


stylesheet : StyleSheet Styles variation
stylesheet =
    styleSheet styles


layout : Model -> Element Styles variation Msg
layout =
    Layout.view LayoutStyle
        { menu = menu
        , title = always "HUE Education Reports"
        , sidebar = sidebar
        , content = content
        }


menu : Model -> Element Styles variation Msg
menu =
    .menu
        >> Menu.view
        >> mapAll MenuMsg MenuStyle identity


organisationSidebar : Model -> Element Styles variation Msg
organisationSidebar model =
    Organisation.sidebar model.organisationPage model.organisations model.categories model.courses
        |> mapAll OrganisationMsg OrganisationStyle identity


employeeSidebar : Model -> Element Styles variation Msg
employeeSidebar model =
    Employee.sidebar model.employeePage model.employees
        |> mapAll EmployeeMsg EmployeeStyle identity


sidebar : Model -> Maybe (Element Styles variation Msg)
sidebar model =
    case model.menu of
        Menu.Organisation ->
            Just (organisationSidebar model)

        Menu.Employee ->
            Just (employeeSidebar model)

        _ ->
            Nothing


content : Model -> Element Styles variation Msg
content model =
    case model.menu of
        Menu.Summary ->
            Summary.view
                { courses = RemoteData.withDefault [] model.courses
                , organisations = RemoteData.withDefault [] model.organisations
                , summaries = model.organisationSummaryMap
                , courseIds = model.competingCourseIds
                , organisationIds = model.competingDivisionIds
                }
                |> mapAll identity SummaryStyle identity

        Menu.Employee ->
            model.courses
                |> RemoteData.map (flip Employee.view model.employeePage)
                |> RemoteData.withDefault empty
                |> mapAll EmployeeMsg EmployeeStyle identity

        _ ->
            empty


view : Model -> Html Msg
view =
    layout >> viewport stylesheet
