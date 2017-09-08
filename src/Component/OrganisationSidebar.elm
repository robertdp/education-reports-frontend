module Component.OrganisationSidebar exposing (..)

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
    | ResultItemProp OrganisationData


type OrganisationData
    = OrganisationName


styles : (Style -> style) -> List (Style.Style style variation)
styles style =
    [ Style.style (style None) []
    , Style.style (style Button) []
    , Style.style (style SearchInput)
        [ Border.all 1
        , Border.solid
        , Color.text Color.darkCharcoal
        , Font.size 20
        , Color.border Color.darkGrey
        ]
    , Style.style (style ResultList)
        [ Style.prop "overflow-x" "hidden"
        , Font.lineHeight 1.5
        ]
    , Style.style (style ResultItem)
        [ Style.cursor "pointer"
        , Style.hover
            [ Color.background Color.lightYellow
            ]
        ]
    , Style.style (style <| ResultItemProp OrganisationName)
        []
    ]


view :
    (Style -> style)
    ->
        { a
            | search : String
            , organisations : WebData (List Organisation)
        }
    -> Element.Element style variation Msg
view style model =
    let
        filterOrganisations =
            case String.toLower model.search of
                "" ->
                    Utils.predicateFilterAll
                        [ .parentId >> (==) Nothing ]

                term ->
                    Utils.predicateFilterAll
                        [ -- .parentId >> (==) Nothing
                          -- ,
                          .name >> String.toLower >> String.contains term
                        ]

        results =
            case model.organisations of
                NotAsked ->
                    Element.empty

                Loading ->
                    Element.text "Loading"

                Failure error ->
                    Element.text <| toString error

                Success organisations ->
                    organisations
                        |> filterOrganisations
                        |> List.take 100
                        |> List.map (showOrganisation style)
                        |> Element.column (style ResultList)
                            [ Attributes.yScrollbar
                            , Attributes.height <| Attributes.fill 1
                            ]
    in
        Element.column (style None)
            [ Attributes.padding 5
            , Attributes.height <| Attributes.fill 1
            ]
            [ --  Element.inputText (style SearchInput)
              --     [ Events.onInput Search
              --     , Attributes.padding 5
              --     , Attributes.placeholder "Organisation name"
              --     ]
              --     model.search
              -- ,
              results
            ]


showOrganisation : (Style -> style) -> Organisation -> Element.Element style variation Msg
showOrganisation style organisation =
    Element.el (style ResultItem)
        [ Attributes.paddingXY 12 8
        , Events.onClick <| SelectOrganisation organisation
        ]
        (Element.el (style <| ResultItemProp OrganisationName)
            []
            (Element.text organisation.name)
        )
