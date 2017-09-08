module Component.EmployeeSidebar exposing (..)

import Color
import Element
import Element.Attributes as Attributes
import Element.Events as Events
import RemoteData exposing (RemoteData(..), WebData)
import Style
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Types exposing (..)
import Utils


type Style
    = None
    | Button
    | SearchInput
    | ResultList
    | ResultItem
    | ResultItemProp EmployeeData


type EmployeeData
    = EmployeeName
    | EmployeeEmail


styles : (Style -> style) -> List (Style.Style style variation)
styles style =
    [ Style.style (style None) []
    , Style.style (style Button) []
    , Style.style (style SearchInput)
        [ Border.all 1
        , Border.solid
        , Color.text Color.darkCharcoal
        , Font.size 18
        , Color.border Color.darkGrey
        ]
    , Style.style (style ResultList)
        [ Font.lineHeight 1.5
        , Style.prop "overflow-x" "hidden"
        ]
    , Style.style (style ResultItem)
        [ Style.cursor "pointer"
        , Style.hover
            [ Color.background Color.lightYellow
            ]
        ]
    , Style.style (style <| ResultItemProp EmployeeName)
        []
    , Style.style (style <| ResultItemProp EmployeeEmail)
        [ Font.size 12
        , Color.text Color.darkCharcoal
        ]
    ]


view :
    (Style -> style)
    ->
        { a
            | search : String
            , employees : WebData (List Employee)
        }
    -> Element.Element style variation Msg
view style model =
    let
        filterEmployees =
            case (String.trim >> String.toLower) model.search of
                "" ->
                    identity

                term ->
                    Utils.predicateFilterAny
                        [ .email >> String.toLower >> String.contains term
                        , .name >> String.toLower >> String.contains term
                        ]

        results =
            case model.employees of
                NotAsked ->
                    Element.empty

                Loading ->
                    Element.text "Loading"

                Failure error ->
                    Element.text <| toString error

                Success employees ->
                    employees
                        |> filterEmployees
                        |> List.take 100
                        |> List.map (showEmployee style)
                        |> Element.column (style ResultList)
                            [ Attributes.yScrollbar
                            , Attributes.height <| Attributes.fill 1
                            ]
    in
        Element.column (style None)
            [ Attributes.padding 5
            , Attributes.spacing 5
            , Attributes.height <| Attributes.fill 1
            ]
            [ Element.inputText (style SearchInput)
                [ Events.onInput Search
                , Attributes.padding 5
                , Attributes.placeholder "Employee name or email"
                ]
                model.search
            , results
            ]


showEmployee : (Style -> style) -> Employee -> Element.Element style variation Msg
showEmployee style employee =
    Element.column (style ResultItem)
        [ Attributes.paddingXY 12 8
        , Attributes.spacing 2
        , Events.onClick <| SelectEmployee employee
        ]
        [ Element.el (style <| ResultItemProp EmployeeName) [] (Element.text employee.name)
        , Element.el (style <| ResultItemProp EmployeeEmail) [] (Element.text employee.email)
        ]
