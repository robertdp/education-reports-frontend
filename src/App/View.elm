module App.View exposing (..)

import App.Component.Layout as Layout
import App.Component.Menu as Menu
import App.Page.Summary as Summary
import App.Page.Organisation as Organisation
import App.Page.Employee as Employee
import App.State exposing (..)
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
        , map SummaryStyle Summary.styles
        , map EmployeeStyle Employee.styles
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
        , sidebar = Just << sidebar
        , content = content
        }


menu : Model -> Element Styles variation Msg
menu =
    .menu
        >> Menu.view
        >> mapAll MenuMsg MenuStyle identity


sidebar : Model -> Element Styles variation Msg
sidebar model =
    case model.menu of
        Menu.Summary ->
            Summary.sidebar model.summaryPage model.organisations
                |> mapAll SummaryMsg SummaryStyle identity

        Menu.Organisation ->
            Organisation.sidebar model.organisationPage model.organisations model.categories model.courses
                |> mapAll OrganisationMsg OrganisationStyle identity

        Menu.Employee ->
            Employee.sidebar model.employeePage model.employees
                |> mapAll EmployeeMsg EmployeeStyle identity


content : Model -> Element Styles variation Msg
content model =
    case model.menu of
        Menu.Summary ->
            RemoteData.map (Summary.view model.summaryPage) model.courses
                |> RemoteData.andMap (RemoteData.succeed model.competingDivisionIds)
                |> RemoteData.andMap model.organisations
                |> RemoteData.andMap (RemoteData.succeed model.organisationSummaryMap)
                |> RemoteData.withDefault empty
                |> mapAll SummaryMsg SummaryStyle identity

        -- Summary.view
        --     { courses = RemoteData.withDefault [] model.courses
        --     , organisations = RemoteData.withDefault [] model.organisations
        --     , summaries = model.organisationSummaryMap
        --     , organisationIds = model.competingDivisionIds
        --     }
        --     |> mapAll identity SummaryStyle identity
        Menu.Employee ->
            model.courses
                |> RemoteData.map (Employee.view model.employeePage)
                |> RemoteData.withDefault empty
                |> mapAll EmployeeMsg EmployeeStyle identity

        Menu.Organisation ->
            RemoteData.map (Organisation.view model.organisationPage) model.organisations
                |> RemoteData.andMap model.employees
                |> RemoteData.andMap model.categories
                |> RemoteData.andMap model.courses
                |> RemoteData.withDefault empty
                |> mapAll OrganisationMsg OrganisationStyle identity


view : Model -> Html Msg
view =
    layout >> viewport stylesheet
