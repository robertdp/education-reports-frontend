module App.View exposing (..)

import App.Component.Layout as Layout
import App.Component.Menu as Menu
import App.Page.User as User
import App.Page.Summary as Summary
import App.State exposing (..)
import Element exposing (..)
import Html exposing (Html)
import Style exposing (..)


type Styles
    = None
    | LayoutStyle Layout.Styles
    | MenuStyle Menu.Styles
    | UserStyle User.Styles


styles : List (Style Styles variation)
styles =
    let
        style_ wrapper =
            wrapper >> style
    in
        [ style None [] ]
            ++ Layout.styles LayoutStyle
            ++ Menu.styles MenuStyle
            ++ User.styles UserStyle


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
            Summary.view model

        _ ->
            empty


view : Model -> Html Msg
view model =
    viewport combinedStyleSheet <|
        layout model
