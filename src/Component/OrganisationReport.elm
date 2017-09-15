module Component.OrganisationReport exposing (..)

import Color
import Component.CardBlock as CardBlock
import Element
import Element.Attributes as Attributes
import Element.Events as Events
import Set
import Style
import Style.Color as Color
import Style.Font as Font
import Types exposing (..)


type alias Model a =
    { a
        | organisation : Organisation
        , enrolments : List Enrolment
        , courses : List Course
        , organisations : List Organisation
    }


type Style
    = None
    | OrganisationName
    | OrganisationList
    | OrganisationListItem
    | CourseSummaryList
    | CourseSummaryListItem
    | CourseSummaryStars
    | CourseSummaryCompleted
    | CardBlockStyle CardBlock.Style


styles : (Style -> style) -> List (Style.Style style variation)
styles style =
    [ Style.style (style None)
        []
    , Style.style (style OrganisationName)
        [ Font.size 20 ]
    , Style.style (style OrganisationList)
        []
    , Style.style (style OrganisationListItem)
        [ Font.lineHeight 1.5
        , Style.cursor "pointer"
        , Style.hover
            [ Color.background Color.lightYellow
            ]
        ]
    , Style.style (style CourseSummaryList)
        [ Font.lineHeight 1.5 ]
    , Style.style (style CourseSummaryListItem)
        [ Style.cursor "pointer"
        , Style.hover
            [ Color.background Color.lightYellow
            ]
        ]
    , Style.style (style CourseSummaryCompleted)
        [ Font.alignRight
        ]
    ]
        ++ CardBlock.styles (style << CardBlockStyle)


view :
    (Style -> style)
    -> Model a
    -> Element.Element style variation Msg
view style model =
    let
        memberCount =
            memberEmails model.organisation
                |> Set.size

        organisationDetails =
            Element.column (style None)
                [ Attributes.spacing 10 ]
                [ Element.el (style OrganisationName) [] (Element.text model.organisation.name)
                ]

        getChildOrganisationList organisation =
            model.organisations
                |> List.filter (\organisation_ -> organisation_.parentId == Just organisation.id)

        memberEmails organisation =
            getChildOrganisationList organisation
                |> List.foldl (\org emails -> Set.union emails <| memberEmails org) organisation.employeeEmails

        childOrganisationRow organisation =
            Element.row (style OrganisationListItem)
                [ Attributes.paddingXY 12 8
                , Events.onClick <| SelectOrganisation organisation
                ]
                [ Element.el (style None)
                    [ Attributes.width <| Attributes.fill 1 ]
                    (Element.text organisation.name)
                , Element.el (style None)
                    []
                    (showMemberCount <| Set.size <| memberEmails organisation)
                ]

        childOrganisationDetails =
            case getChildOrganisationList model.organisation of
                [] ->
                    Element.empty

                children ->
                    CardBlock.view
                        { style = style << CardBlockStyle
                        , header =
                            always <|
                                Element.row
                                    (style None)
                                    [ Attributes.justify
                                    , Attributes.width <| Attributes.fill 1
                                    ]
                                    [ Element.el (style None) [] (Element.text "Organisation")
                                    , Element.el (style None)
                                        []
                                        (List.length children
                                            |> toString
                                            |> flip (++) " sub-divisions"
                                            |> Element.text
                                        )
                                    ]
                        , content =
                            List.map childOrganisationRow
                                >> Element.column (style OrganisationList)
                                    [ Attributes.padding 5 ]
                        }
                        children

        courseSummaryData :
            List
                { course : Course
                , enrolled : Int
                , completed : Int
                }
        courseSummaryData =
            model.courses
                |> List.map
                    (\course ->
                        model.enrolments
                            |> List.filter (\enrolment -> enrolment.courseId == course.id)
                            |> List.foldl
                                (\enrolment summary ->
                                    case enrolment.status of
                                        Enrolled ->
                                            { summary | enrolled = summary.enrolled + 1 }

                                        Completed _ ->
                                            { summary | completed = summary.completed + 1 }

                                        _ ->
                                            summary
                                )
                                { course = course
                                , enrolled = 0
                                , completed = 0
                                }
                    )

        showCourseSummaryRow :
            { course : Course
            , enrolled : Int
            , completed : Int
            }
            -> Element.Element style variation Msg
        showCourseSummaryRow summary =
            Element.row (style CourseSummaryListItem)
                [ Attributes.width <| Attributes.fill 1
                , Attributes.paddingXY 12 8
                , Events.onClick <| SelectCourse summary.course
                ]
                [ Element.el (style None)
                    [ Attributes.width <| Attributes.fill 1 ]
                    (Element.text summary.course.name)
                , Element.el (style CourseSummaryCompleted)
                    [ Attributes.width <| Attributes.fill 1
                    ]
                    (Element.text <| (toString (100 * toFloat summary.completed / toFloat memberCount |> round)) ++ "%")
                ]

        allCourseEnrolmentDetails =
            case memberCount of
                0 ->
                    Element.empty

                _ ->
                    CardBlock.view
                        { style = style << CardBlockStyle
                        , header =
                            always <|
                                Element.row (style None)
                                    [ Attributes.justify
                                    , Attributes.width <| Attributes.fill 1
                                    ]
                                    [ Element.el (style None) [] (Element.text "Course completion")
                                    , Element.el (style None) [] (showMemberCount memberCount)
                                    ]
                        , content =
                            List.map showCourseSummaryRow
                                >> Element.column (style CourseSummaryList)
                                    [ Attributes.padding 5
                                    ]
                        }
                        courseSummaryData
    in
        Element.column (style None)
            [ Attributes.spacing 16
            ]
            [ organisationDetails
            , childOrganisationDetails
            , allCourseEnrolmentDetails
            ]


showMemberCount : Int -> Element.Element style variation msg
showMemberCount n =
    case n of
        0 ->
            Element.empty

        1 ->
            "1 member"
                |> Element.text

        n ->
            toString n
                ++ " members"
                |> Element.text
