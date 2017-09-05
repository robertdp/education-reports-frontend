module Types exposing (..)

import Date exposing (Date)
import Dict exposing (Dict)
import RemoteData exposing (WebData)


type Msg
    = InitialDataLoaded (WebData InitialData)
    | IndividualReportLoaded Employee (WebData (List Enrolment))
    | OrganisationReportLoaded Organisation (WebData (List Enrolment))
    | Search String
    | SelectEmployee Employee
    | SelectCourse Course
    | SelectOrganisation Organisation
    | DeselectOrganisation
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
    , managerEmails : List Email
    , employeeEmails : List Email
    , parentId : Maybe Id
    }


type alias Enrolment =
    { employeeEmail : Email
    , courseId : Id
    , status : EnrolmentStatus
    }


type EnrolmentStatus
    = NotEnrolled
    | Enrolled
    | Completed Date


type Report
    = ForIndividual Employee (WebData (List Enrolment))
    | ForCourse Course (WebData (List Enrolment))
    | ForOrganisation Organisation (WebData (List Enrolment))


type alias InitialData =
    { courses : List Course
    , employees : List Employee
    , organisations : List Organisation
    }


type alias Model =
    { employees : WebData (List Employee)
    , courses : WebData (List Course)
    , courseMap : Dict Id Course
    , organisations : WebData (List Organisation)
    , organisationMap : Dict Id Organisation
    , report : Maybe Report
    , sidebar : Sidebar
    , search : String
    , api : String
    }


type Sidebar
    = SearchEmployee
    | SearchOrganisation


type alias Flags =
    { api : String }
