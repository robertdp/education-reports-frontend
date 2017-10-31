module Component.OrganisationSummaryReport exposing (..)

import Color
import Dict
import Element
import Element.Attributes as Attributes
import Style
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
        [ Style.rotate (-pi / 4)
        ]
    , Style.style (style OrganisationNameContainer)
        []
    , Style.style (style CourseName)
        [ Font.alignRight
        ]
    , Style.style (style CourseNameContainer)
        []
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
        courses =
            model.courses
                |> List.sortBy .shortName

        cells =
            organisationHeaders ++ courseHeaders ++ results

        organisationHeaders =
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
                                [ Attributes.height <| Attributes.px 200
                                , Attributes.width <| Attributes.px 40
                                , Attributes.alignBottom
                                , Attributes.paddingLeft 20
                                ]
                            |> (\content ->
                                    Element.cell
                                        { start = ( x + 1, 0 )
                                        , width = 1
                                        , height = 1
                                        , content = content
                                        }
                               )
                    )

        courseHeaders =
            courses
                |> List.indexedMap
                    (\y course ->
                        Element.text course.shortName
                            |> Element.el (style CourseName)
                                []
                            |> List.singleton
                            |> Element.row (style CourseNameContainer)
                                [ Attributes.height <| Attributes.px 40
                                , Attributes.verticalCenter
                                , Attributes.alignRight
                                ]
                            |> (\content ->
                                    Element.cell
                                        { start = ( 0, y + 1 )
                                        , width = 1
                                        , height = 1
                                        , content = content
                                        }
                               )
                    )

        results =
            model.organisations
                |> List.indexedMap
                    (\x organisation ->
                        courses
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
                                        |> (\content ->
                                                Element.cell
                                                    { start = ( x + 1, y + 1 )
                                                    , width = 1
                                                    , height = 1
                                                    , content = content
                                                    }
                                           )
                                )
                    )
                |> List.concat
    in
        Element.grid (style SummaryGrid)
            [ Attributes.spacing 10
            , Attributes.maxWidth <| Attributes.px 1
            ]
            { rows = [ Attributes.px 200 ]
            , columns = []
            , cells = cells
            }
