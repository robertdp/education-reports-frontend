module App.View exposing (..)

import App.Component.Layout as Layout
import App.Component.Menu as Menu
import App.Page.Summary as Summary
import App.Page.User as User
import App.State exposing (..)
import Element exposing (..)
import Html exposing (Html)
import Html.Lazy
import RemoteData
import Style exposing (..)


type Styles
    = None
    | LayoutStyle Layout.Styles
    | MenuStyle Menu.Styles
    | UserStyle User.Styles
    | SummaryStyle Summary.Styles


styles : List (Style Styles variation)
styles =
    [ style None [] ]
        ++ Layout.styles LayoutStyle
        ++ Menu.styles MenuStyle
        ++ User.styles UserStyle
        ++ Summary.styles SummaryStyle


combinedStyleSheet : StyleSheet Styles variation
combinedStyleSheet =
    styleSheet styles


layout : Model -> Element Styles variation Msg
layout =
    Layout.view
        { style = LayoutStyle
        , stylesheet = combinedStyleSheet
        , menu = menu
        , title = always "HUE Education Reports"
        , sidebar = sidebar
        , content = content
        }


menu : Model -> Element Styles variation Msg
menu =
    .menu
        >> Menu.view
            { style = MenuStyle
            , stylesheet = combinedStyleSheet
            , msg = MenuMsg
            }


userSidebar : Model -> Element Styles variation Msg
userSidebar =
    .userPage
        >> User.sidebar
            { style = UserStyle
            , msg = UserMsg
            }


sidebar : Model -> Maybe (Element Styles variation Msg)
sidebar model =
    case model.menu of
        Menu.Search ->
            Just (text "search")

        Menu.User ->
            Just (userSidebar model)

        _ ->
            Nothing


content : Model -> Element Styles variation Msg
content model =
    case model.menu of
        Menu.Summary ->
            Summary.view { style = SummaryStyle }
                { courses = RemoteData.withDefault [] model.courses
                , organisations = RemoteData.withDefault [] model.organisations
                , summaries = model.organisationSummaryMap
                , courseIds = model.competingCourseIds
                , organisationIds = model.competingDivisionIds
                }

        _ ->
            empty


view : Model -> Html Msg
view =
    Html.Lazy.lazy (layout >> viewport combinedStyleSheet)
