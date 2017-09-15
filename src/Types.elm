module Types exposing (..)

import Date exposing (Date)
import Dict exposing (Dict)
import RemoteData exposing (WebData)
import Set exposing (Set)


type Msg
    = NoOp
    | InitialDataLoaded (WebData InitialData)
    | EmployeeReportLoaded Employee (WebData (List Enrolment))
    | OrganisationReportLoaded Organisation (WebData (List Enrolment))
    | Search String
    | SelectOrganisationSummary
    | SelectEmployee Employee
    | SelectOrganisation Organisation
    | SelectCourse Course
    | DeselectCourse
    | SelectParentOrganisation
    | ToggleSidebarMode


type alias Email =
    String


type alias Id =
    Int


type alias Url =
    String


type alias Category =
    { id : Id
    , name : String
    , parentId : Maybe Id
    }


type alias Course =
    { id : Id
    , name : String
    , shortName : String
    }


type alias Employee =
    { email : Email
    , name : String
    , number : Maybe String
    , organisationId : Id
    }


type alias Organisation =
    { id : Id
    , name : String
    , managerEmails : Set Email
    , employeeEmails : Set Email
    , parentId : Maybe Id
    }


type alias Enrolment =
    { employeeEmail : Email
    , courseId : Id
    , status : EnrolmentStatus
    }


type alias OrganisationSummary =
    { organisationId : Id
    , courseId : Id
    , members : Int
    , enrolled : Int
    , completed : Int
    }


type EnrolmentStatus
    = NotEnrolled
    | Enrolled
    | Completed Date


type Report
    = SummaryReport
    | EmployeeReport Employee (WebData (List Enrolment))
    | OrganisationReport Organisation (WebData (List Enrolment))
    | OrganisationCourseReport Organisation Course (WebData (List Enrolment))


type alias InitialData =
    { courses : List Course
    , employees : List Employee
    , organisations : List Organisation
    , organisationSummaries : List OrganisationSummary
    }


type alias Model =
    { employees : WebData (List Employee)
    , employeeMap : Dict Email Employee
    , courses : WebData (List Course)
    , courseMap : Dict Id Course
    , organisations : WebData (List Organisation)
    , organisationMap : Dict Id Organisation
    , organisationSummaries : WebData (List OrganisationSummary)
    , organisationSummaryMap : Dict Id (Dict Id OrganisationSummary)
    , report : Report
    , sidebar : Sidebar
    , search : String
    , api : String
    }


type Sidebar
    = SearchEmployee
    | SearchOrganisation


type alias Flags =
    { api : String }
