module Component.EmployeeReport exposing (..)

import Color
import Component.DashboardCard as DashboardCard
import Dict
import Element
import Element.Attributes as Attributes
import Style
import Style.Color as Color
import Style.Font as Font
import Types exposing (..)


type Style
    = None
    | EmployeeName
    | EmployeeEmail
    | CourseList
    | DashboardCardStyle DashboardCard.Style


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
        ++ DashboardCard.styles (style << DashboardCardStyle)


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

        showCourses heading courses =
            if courses == [] then
                Element.empty
            else
                DashboardCard.view
                    { style = style << DashboardCardStyle
                    , header = always <| Element.text heading
                    , content =
                        List.map
                            (\course ->
                                Element.el (style None)
                                    [ Attributes.paddingXY 12 8 ]
                                    (Element.text course.name)
                            )
                            >> Element.column (style CourseList) [ Attributes.padding 5 ]
                    }
                    courses
    in
        Element.column (style None)
            [ Attributes.spacing 16 ]
            [ employeeDetails
            , showCourses ((toString << List.length) enrolledCourses ++ " enrolled") enrolledCourses
            , showCourses ((toString << List.length) completedCourses ++ " completed") completedCourses
            ]
