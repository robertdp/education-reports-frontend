module Component.OrganisationCourseReport exposing (..)

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
    | CourseName
    | EmployeeList
    | EmployeeListItem
    | EmployeeName
    | EmployeeEmail
    | EnrolmentCompletionDate
    | BackButton
    | BackButtonIcon
    | BackButtonText
    | CardBlockStyle CardBlock.Style


styles : (Style -> style) -> List (Style.Style style variation)
styles style =
    [ Style.style (style None)
        []
    , Style.style (style CourseName)
        [ Font.size 20 ]
    , Style.style (style EmployeeList)
        [ Font.lineHeight 1.5 ]
    , Style.style (style EmployeeListItem)
        [ Style.hover
            [ Color.background Color.lightGray
            ]
        ]
    , Style.style (style EmployeeName)
        []
    , Style.style (style EmployeeEmail)
        []
    , Style.style (style EnrolmentCompletionDate)
        [ Font.alignRight
        ]
    , Style.style (style BackButton)
        [ Border.none
        , Color.text Color.darkCharcoal
        , Border.rounded 3
        , Font.size 16
        , Background.gradient 0
            [ Background.step <| Color.rgb 205 208 211
            , Background.step <| Color.rgb 230 232 234
            ]
        , Style.cursor "pointer"
        ]
    , Style.style (style BackButtonIcon)
        [ Font.size 24
        ]
    , Style.style (style BackButtonText)
        [ Font.size 16
        ]
    ]
        ++ CardBlock.styles (style << CardBlockStyle)


view :
    (Style -> style)
    ->
        { a
            | course : Course
            , employees : Dict.Dict Email Employee
            , enrolments : List Enrolment
            , organisation : Organisation
            , organisations : Dict.Dict Id Organisation
        }
    -> Element.Element style variation Msg
view style model =
    let
        allMemberEmails =
            model.organisation
                |> Recursive.getWithAllDescendants model.organisations
                |> List.foldl (.employeeEmails >> Set.union) Set.empty

        allMembers =
            allMemberEmails
                |> Set.toList
                |> List.filterMap (flip Dict.get model.employees)
                |> List.sortBy .name

        memberEnrolmentStatuses =
            model.enrolments
                |> List.foldl (\enrolment store -> Dict.insert enrolment.employeeEmail enrolment.status store) Dict.empty

        getMemberEnrolment employee =
            Dict.get employee.email memberEnrolmentStatuses
                |> Maybe.withDefault NotEnrolled

        notEnrolledMembers =
            allMembers
                |> List.filter (getMemberEnrolment >> (==) NotEnrolled)

        enrolledMembers =
            allMembers
                |> List.filter (getMemberEnrolment >> (==) Enrolled)

        completedMembers =
            allMembers
                |> List.filter
                    (\employee ->
                        case getMemberEnrolment employee of
                            Completed _ ->
                                True

                            _ ->
                                False
                    )

        showMemberEnrolments f heading employees =
            if employees == [] then
                Element.empty
            else
                CardBlock.view
                    { style = style << CardBlockStyle
                    , header = always <| Element.text heading
                    , content =
                        List.map
                            (\employee ->
                                Element.row (style EmployeeListItem)
                                    [ Attributes.paddingXY 12 8
                                    , Attributes.width <| Attributes.fill 1
                                    , Attributes.minWidth <| Attributes.px 670
                                    ]
                                    (f employee)
                            )
                            >> Element.column (style EmployeeList)
                                [ Attributes.padding 5
                                ]
                    }
                    employees

        courseDetails =
            Element.column (style None)
                [ Attributes.spacing 10 ]
                [ Element.el (style CourseName) [] (Element.text model.course.name)
                ]

        backButton =
            Element.button <|
                Element.row (style BackButton)
                    [ Attributes.paddingXY 8 4
                    , Attributes.alignLeft
                    , Attributes.verticalCenter
                    , Events.onClick <| SelectOrganisation model.organisation
                    ]
                    [ Element.el (style BackButtonIcon)
                        [ Attributes.paddingXY 4 0
                        , Attributes.paddingBottom 2
                        ]
                        (Element.text "⇤")
                    , Element.el (style BackButtonText)
                        [ Attributes.paddingXY 4 0
                        ]
                        (Element.text <| "Back to " ++ model.organisation.name)
                    ]

        getFormattedCompletionDate employee =
            case getMemberEnrolment employee of
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
            [ backButton
            , courseDetails
            , showMemberEnrolments
                (\employee ->
                    [ employee.name
                        |> Element.text
                        |> Element.el (style EmployeeName)
                            [ Attributes.width <| Attributes.percent 40
                            ]
                    , employee.email
                        |> Element.text
                        |> Element.el (style EmployeeEmail)
                            [ Attributes.width <| Attributes.percent 40
                            ]
                    , getFormattedCompletionDate employee
                        |> Element.el (style EnrolmentCompletionDate)
                            [ Attributes.width <| Attributes.percent 20
                            ]
                    ]
                )
                "Completed"
                completedMembers
            , showMemberEnrolments
                (\employee ->
                    [ employee.name
                        |> Element.text
                        |> Element.el (style EmployeeName)
                            [ Attributes.width <| Attributes.percent 40
                            ]
                    , employee.email
                        |> Element.text
                        |> Element.el (style EmployeeEmail)
                            [ Attributes.width <| Attributes.percent 60
                            ]
                    ]
                )
                "Enrolled"
                enrolledMembers
            , showMemberEnrolments
                (\employee ->
                    [ employee.name
                        |> Element.text
                        |> Element.el (style EmployeeName)
                            [ Attributes.width <| Attributes.percent 40
                            ]
                    , employee.email
                        |> Element.text
                        |> Element.el (style EmployeeEmail)
                            [ Attributes.width <| Attributes.percent 60
                            ]
                    ]
                )
                "Not enrolled"
                notEnrolledMembers
            ]
