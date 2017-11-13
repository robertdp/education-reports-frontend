module App.Page.Summary exposing (..)

import App.Data exposing (..)
import Color exposing (..)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Attributes exposing (..)
import Set exposing (Set)
import Style exposing (..)
import Style.Font as Font


type Styles
    = None
    | Summary
    | DivisionName
    | CourseName
    | CourseCompletion


styles : List (Style Styles variation)
styles =
    [ style None []
    , style Summary
        [ Font.size 16
        , Font.light
        ]
    , style DivisionName
        [ Style.rotate (-pi / 4)
        ]
    , style CourseName
        []
    , style CourseCompletion
        []
    ]


view :
    { a
        | courseIds : Set Id
        , courses : List Course
        , organisationIds : Set Id
        , organisations : List Organisation
        , summaries : Dict Id (Dict.Dict Id OrganisationSummary)
    }
    -> Element.Element Styles variation msg
view model =
    let
        courses =
            model.courses
                -- |> List.filter (\course -> Set.member course.id model.courseIds)
                |> List.sortBy .shortName

        cells =
            organisationHeaders ++ courseHeaders ++ results

        organisations =
            model.organisations
                |> List.filter (\organisation -> Set.member organisation.id model.organisationIds)

        organisationHeaders =
            organisations
                |> List.indexedMap
                    (\x organisation ->
                        Element.text organisation.name
                            |> Element.el (DivisionName)
                                [ width <| px 250
                                , height <| px 50
                                ]
                            |> List.singleton
                            |> Element.row (None)
                                [ width <| px 50
                                , height <| px 200
                                , paddingTop 100
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
                            |> Element.el (CourseName)
                                []
                            |> List.singleton
                            |> Element.row (None)
                                [ height <| px 50
                                , verticalCenter
                                , alignRight
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
            organisations
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
                                                                    Color.red
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
                                                        |> Element.el (CourseCompletion) []
                                                        |> List.singleton
                                                        |> Element.row (None)
                                                            [ verticalCenter
                                                            , center
                                                            , inlineStyle [ ( "backgroundColor", backgroundColor ) ]
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
        Element.row (None)
            []
            [ Element.el (None) [ width fill ] empty
            , Element.grid (Summary)
                [ spacing 10
                , width fill
                ]
                { rows = []
                , columns = []
                , cells = cells
                }
            , Element.el (None) [ width fill ] empty
            ]
