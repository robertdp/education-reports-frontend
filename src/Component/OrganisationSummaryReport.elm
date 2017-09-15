module Component.OrganisationSummaryReport exposing (..)

import Color
import Component.CardBlock as CardBlock
import Data.Recursive as Recursive
import Date
import Dict
import Element
import Element.Attributes as Attributes
import Element.Events as Events
import Set
import Style
import Style.Background as Background
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Types exposing (..)


type Style
    = None
    | SummaryGrid
    | OrganisationName
    | OrganisationNameContainer
    | CourseName
    | CourseNameContainer


styles : (Style -> style) -> List (Style.Style style variation)
styles style =
    [ Style.style (style None)
        []
    , Style.style (style SummaryGrid)
        []
    , Style.style (style OrganisationName)
        [ Font.noWrap
        , Style.rotate (-pi / 2)
        ]
    , Style.style (style OrganisationNameContainer)
        [ Font.noWrap

        -- , Style.rotate (-pi / 2)
        ]
    , Style.style (style CourseName)
        [ Font.noWrap
        , Font.alignRight
        ]
    , Style.style (style CourseNameContainer)
        [ Font.noWrap ]
    ]


view :
    (Style -> style)
    ->
        { a
            | courses : List Course
            , organisations : List Organisation
            , summaries : Dict.Dict Id (Dict.Dict Id OrganisationSummary)
        }
    -> Element.Element style variation Msg
view style model =
    let
        cells =
            organisations ++ courses ++ results

        organisations =
            model.organisations
                |> List.filter (.parentId >> (==) Nothing)
                |> List.indexedMap
                    (\x organisation ->
                        Element.text organisation.name
                            |> Element.el (style OrganisationName)
                                [ Attributes.width <| Attributes.px 0
                                ]
                            |> List.singleton
                            |> Element.row (style OrganisationNameContainer)
                                [ Attributes.height <| Attributes.px 330
                                , Attributes.width <| Attributes.px 40
                                , Attributes.alignBottom
                                , Attributes.paddingLeft 20
                                ]
                            |> Element.area
                                { start = ( x + 1, 0 )
                                , width = 1
                                , height = 1
                                }
                    )

        courses =
            model.courses
                |> List.indexedMap
                    (\y course ->
                        Element.text course.name
                            |> Element.el (style CourseName)
                                []
                            |> List.singleton
                            |> Element.row (style CourseNameContainer)
                                [ Attributes.height <| Attributes.px 40
                                , Attributes.verticalCenter
                                , Attributes.alignRight
                                ]
                            |> Element.area
                                { start = ( 0, y + 1 )
                                , width = 1
                                , height = 1
                                }
                    )

        results =
            model.organisations
                |> List.filter (.parentId >> (==) Nothing)
                |> List.indexedMap
                    (\x organisation ->
                        model.courses
                            |> List.indexedMap
                                (\y course ->
                                    model.summaries
                                        |> Dict.get organisation.id
                                        |> Maybe.andThen (Dict.get course.id)
                                        |> Maybe.map
                                            (\summary ->
                                                let
                                                    percent =
                                                        (toFloat summary.completed / toFloat summary.members)
                                                            |> (*) 100
                                                            |> round

                                                    backgroundColor =
                                                        percent
                                                            |> toFloat
                                                            >> (*) 0.01
                                                            |> (\alpha ->
                                                                    Color.darkBlue
                                                                        |> Color.toRgb
                                                                        |> (\color ->
                                                                                "rgba("
                                                                                    ++ toString color.red
                                                                                    ++ ", "
                                                                                    ++ toString color.green
                                                                                    ++ ", "
                                                                                    ++ toString color.blue
                                                                                    ++ ", "
                                                                                    ++ toString alpha
                                                                                    ++ ")"
                                                                           )
                                                               )
                                                in
                                                    percent
                                                        |> toString
                                                        |> flip (++) "%"
                                                        |> Element.text
                                                        |> Element.el (style None) []
                                                        |> List.singleton
                                                        |> Element.row (style None)
                                                            [ Attributes.verticalCenter
                                                            , Attributes.center
                                                            , Attributes.inlineStyle [ ( "backgroundColor", backgroundColor ) ]
                                                            ]
                                            )
                                        |> Maybe.withDefault Element.empty
                                        |> Element.area
                                            { start = ( x + 1, y + 1 )
                                            , width = 1
                                            , height = 1
                                            }
                                )
                    )
                |> List.concat
    in
        Element.grid (style SummaryGrid)
            { rows = [ Attributes.px 330 ]
            , columns = []
            }
            [ Attributes.spacing 10 ]
            cells
