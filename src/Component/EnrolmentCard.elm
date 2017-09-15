module Component.EnrolmentCard exposing (..)

import Component.CardBlock as CardBlock
import Data.Record as Record
import Date
import Dict
import Element
import Element.Attributes as Attributes
import Types exposing (..)
import Utils
import Style


type Style
    = None
    | CardBlockStyle CardBlock.Style


styles : (Style -> style) -> List (Style.Style style variation)
styles style =
    [ Style.style (style None)
        []
    ]
        ++ CardBlock.styles (style << CardBlockStyle)


viewEmployeeEnrolments :
    (Style -> style)
    ->
        { a
            | courses : List Course
            , employee : Employee
            , enrolments : List Enrolment
        }
    -> Element.Element style variation msg
viewEmployeeEnrolments style model =
    let
        enrolmentMap =
            model.enrolments
                |> List.filter (.employeeEmail >> (==) model.employee.email)
                |> Record.toDict .courseId

        getCourseEnrolmentStatus courseId =
            enrolmentMap
                |> Dict.get courseId
                |> Maybe.map .status
                |> Maybe.withDefault NotEnrolled

        courses =
            model.courses
                |> List.sortBy .name
                |> List.map (\course -> ( course, getCourseEnrolmentStatus course.id ))

        ( completedCourses, notCompletedCourses ) =
            courses
                |> List.partition (Tuple.second >> isCompleted)

        ( enrolledCourses, notEnrolledCourses ) =
            notCompletedCourses
                |> List.partition (Tuple.second >> isEnrolled)

        viewEnrolmentRow course status =
            Element.row (style None)
                [ Attributes.paddingXY 12 8
                , Attributes.justify
                ]
                [ course.name |> Element.text |> Element.el (style None) []
                , viewCompletionDate status |> Element.el (style None) []
                ]
    in
        case model.enrolments of
            [] ->
                Element.empty

            _ ->
                CardBlock.view
                    { style = style << CardBlockStyle
                    , header = always <| Element.text <| cardTitle NotEnrolled
                    , content =
                        List.map (\( course, enrolment ) -> viewEnrolmentRow course enrolment)
                            >> Element.column (style None) []
                    }
                    courses


cardTitle : EnrolmentStatus -> String
cardTitle status =
    case status of
        NotEnrolled ->
            "Not Enrolled"

        Enrolled ->
            "Enrolled"

        Completed _ ->
            "Completed"



-- showCourses f heading courses =
--     if courses == [] then
--         Element.empty
--     else
--         CardBlock.view
--             { style = style << CardBlockStyle
--             , header = always <| Element.text heading
--             , content =
--                 List.map
--                     (\course ->
--                         Element.row (style None)
--                             [ Attributes.paddingXY 12 8
--                             , Attributes.justify
--                             ]
--                             (f course)
--                     )
--                     >> Element.column (style CourseList) [ Attributes.padding 5 ]
--             }
--             courses


isNotEnrolled : EnrolmentStatus -> Bool
isNotEnrolled =
    (==) NotEnrolled


isEnrolled : EnrolmentStatus -> Bool
isEnrolled =
    (==) Enrolled


isCompleted : EnrolmentStatus -> Bool
isCompleted status =
    case status of
        Completed _ ->
            True

        _ ->
            False


viewCompletionDate : EnrolmentStatus -> Element.Element style variation msg
viewCompletionDate status =
    let
        format date =
            [ Date.day date |> toString
            , Date.month date |> toString
            , Date.year date |> toString
            ]
                |> String.join " "
    in
        case status of
            Completed date ->
                format date
                    |> Element.text

            _ ->
                Element.empty
