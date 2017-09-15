module Component.EmployeeReport exposing (..)

import Color
import Component.CardBlock as CardBlock
import Dict
import Element
import Element.Attributes as Attributes
import Style
import Style.Color as Color
import Style.Font as Font
import Types exposing (..)
import Date


type Style
    = None
    | EmployeeName
    | EmployeeEmail
    | CourseList
    | CardBlockStyle CardBlock.Style


styles : (Style -> style) -> List (Style.Style style variation)
styles style =
    [ Style.style (style None)
        []
    , Style.style (style EmployeeName)
        [ Font.size 20
        ]
    , Style.style (style EmployeeEmail)
        [ Color.text Color.darkCharcoal
        ]
    ]
        ++ CardBlock.styles (style << CardBlockStyle)


view :
    (Style -> style)
    ->
        { a
            | employee : Employee
            , enrolments : List Enrolment
            , courses : Dict.Dict Id Course
        }
    -> Element.Element style variation msg
view style model =
    let
        employeeDetails =
            Element.column (style None)
                [ Attributes.spacing 10 ]
                [ Element.el (style EmployeeName) [] (Element.text model.employee.name)
                , Element.el (style EmployeeEmail) [] (Element.text model.employee.email)
                ]

        courseEnrolmentStatuses =
            model.enrolments
                |> List.foldl (\enrolment store -> Dict.insert enrolment.courseId enrolment.status store) Dict.empty

        getCourseEnrolment course =
            Dict.get course.id courseEnrolmentStatuses
                |> Maybe.withDefault NotEnrolled

        getCoursesWhere predicate =
            model.enrolments
                |> List.filter predicate
                |> List.filterMap (.courseId >> flip Dict.get model.courses)
                |> List.sortBy .name

        enrolledCourses =
            getCoursesWhere (.status >> (==) Enrolled)

        completedCourses =
            getCoursesWhere
                (\enrolment ->
                    case enrolment.status of
                        Completed _ ->
                            True

                        _ ->
                            False
                )

        notEnrolledCourses =
            getCoursesWhere (.status >> (==) NotEnrolled)

        showCourses f heading courses =
            if courses == [] then
                Element.empty
            else
                CardBlock.view
                    { style = style << CardBlockStyle
                    , header = always <| Element.text heading
                    , content =
                        List.map
                            (\course ->
                                Element.row (style None)
                                    [ Attributes.paddingXY 12 8
                                    , Attributes.justify
                                    ]
                                    (f course)
                            )
                            >> Element.column (style CourseList) [ Attributes.padding 5 ]
                    }
                    courses

        getFormattedCompletionDate course =
            case getCourseEnrolment course of
                Completed date ->
                    [ Date.dayOfWeek date |> toString
                    , Date.day date |> toString
                    , Date.month date |> toString
                    , Date.year date |> toString
                    ]
                        |> String.join " "
                        |> Element.text

                _ ->
                    Element.empty
    in
        Element.column (style None)
            [ Attributes.spacing 16 ]
            [ employeeDetails
            , showCourses
                (\course ->
                    [ course.name |> Element.text |> Element.el (style None) []
                    , getFormattedCompletionDate course |> Element.el (style None) []
                    ]
                )
                ((toString << List.length) completedCourses ++ " completed")
                completedCourses
            , showCourses (.name >> Element.text >> List.singleton) ((toString << List.length) enrolledCourses ++ " enrolled") enrolledCourses
            ]
