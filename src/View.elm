module View exposing (..)

import Color
import Element exposing (Element, button, column, el, empty, inputText, row, text, viewport)
import Element.Attributes exposing (center, fill, height, padding, paddingXY, px, scrollbars, spacing, verticalCenter, width, yScrollbar)
import Element.Events exposing (onClick, onInput)
import Html
import RemoteData exposing (RemoteData(..), WebData)
import Style exposing (style)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Style.Shadow as Shadow
import Types exposing (..)


type Styles
    = None
    | Main
    | Sidebar
    | Heading
    | Body
    | Button
    | SearchInput
    | ResultList
    | ResultItem
    | EmployeeName
    | EmployeeEmail


stylesheet : Style.StyleSheet Styles variation
stylesheet =
    Style.styleSheet
        [ style None []
        , style Main
            [ Font.typeface [ "Roboto", "メイリオ", "Meiryo", "sans-serif" ]
            , Font.size 15
            ]
        , style Sidebar
            []
        , style Heading
            [ Color.background <| Color.rgb 58 74 82
            , Font.size 20
            , Color.text Color.white
            ]
        , style Body
            [ Color.background <| Color.rgb 240 241 242
            , Style.shadows
                [ Shadow.inset { offset = ( 5, 0 ), size = -6, blur = 6, color = Color.rgba 0 0 0 0.5 }
                ]
            ]
        , style Button
            [ Border.rounded 5
            , Color.border Color.blue
            , Color.background Color.blue
            , Color.text Color.white
            , Style.prop "border-style" "outset"
            , Style.cursor "pointer"
            ]
        , style SearchInput
            [ Border.all 1
            , Border.solid
            , Color.text Color.darkCharcoal
            , Font.size 20
            , Color.border Color.darkGrey
            ]
        , style ResultList
            [ Style.prop "overflow-x" "hidden"
            ]
        , style ResultItem
            [ Font.noWrap
            , Style.cursor "pointer"
            , Style.hover
                [ Color.background Color.lightYellow
                ]
            ]
        , style EmployeeName []
        , style EmployeeEmail
            [ Font.size 12
            , Color.text Color.darkCharcoal
            ]
        ]


view : Model -> Html.Html Msg
view model =
    case Debug.log "Report" model.report of
        _ ->
            viewport stylesheet <|
                row Main
                    [ height <| fill 1 ]
                    [ column Sidebar
                        [ width <| px 256 ]
                        [ -- row None
                          --     [ height <| px 60
                          --     , verticalCenter
                          --     , center
                          --     ]
                          --     [ toggleModeButton "Change modes"
                          --     ]
                          -- ,
                          sidebar model
                        ]
                    , column None
                        [ width <| fill 1 ]
                        [ row Heading
                            [ height <| px 60
                            , verticalCenter
                            , paddingXY 32 0
                            ]
                            [ el None [] (text "HUE Education Reports") ]
                        , el Body [ height <| fill 1 ] <| el None [ paddingXY 32 24 ] <| body model
                        ]
                    ]


toggleModeButton : String -> Element Styles variation Msg
toggleModeButton message =
    text message
        |> el Button
            [ onClick ToggleSidebarMode
            , paddingXY 10 5
            ]
        |> button


sidebar : Model -> Element Styles variation Msg
sidebar model =
    case (model.sidebar) of
        SearchIndividual ->
            searchIndividual model.search model.employees

        SearchAggregate ->
            searchAggregate model.search model.courses model.organisations


predicateFilterAny : List (a -> Bool) -> List a -> List a
predicateFilterAny predicates =
    List.filter (\x -> List.foldr (\f a -> a || f x) False predicates)


predicateFilterAll : List (a -> Bool) -> List a -> List a
predicateFilterAll predicates =
    List.filter (\x -> List.foldl (\f a -> a && f x) True predicates)


searchIndividual : String -> WebData (List Employee) -> Element Styles variation Msg
searchIndividual term employees =
    let
        filterEmployees =
            case String.toLower term of
                "" ->
                    identity

                term ->
                    predicateFilterAny
                        [ .email >> String.toLower >> String.contains term
                        , .name >> String.toLower >> String.contains term
                        ]

        results =
            case employees of
                NotAsked ->
                    empty

                Loading ->
                    text "Loading"

                Failure error ->
                    text <| toString error

                Success employees ->
                    employees
                        |> filterEmployees
                        |> List.take 100
                        |> List.map showEmployee
                        |> column ResultList
                            [ yScrollbar
                            , height <| fill 1
                            ]
    in
        column None
            [ padding 5
            , spacing 5
            , height <| fill 1
            ]
            [ inputText SearchInput
                [ onInput Search
                , padding 5
                ]
                term
            , results
            ]


showEmployee : Employee -> Element Styles variation Msg
showEmployee employee =
    column ResultItem
        [ padding 5
        , spacing 2
        , onClick <| SelectEmployee employee
        ]
        [ el EmployeeName [] (text employee.name)
        , el EmployeeEmail [] (text employee.email)
        ]


searchAggregate : String -> WebData (List Course) -> WebData (List Organisation) -> Element Styles variation msg
searchAggregate term courses organisations =
    empty


body : Model -> Element Styles variation msg
body model =
    let
        show f a =
            case a of
                Loading ->
                    text "Loading..."

                Success data ->
                    f data

                Failure error ->
                    text <| toString error

                NotAsked ->
                    empty
    in
        case model.report of
            Nothing ->
                empty

            Just (ForIndividual employee data) ->
                show (showIndividualReport employee) data

            _ ->
                empty


showIndividualReport : Employee -> List Enrolment -> Element Styles variation msg
showIndividualReport employee data =
    text <| toString data
