module Types exposing (..)

import RemoteData exposing (WebData)


type Msg
    = InitialDataResponse (WebData InitialData)
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
    , fullName : String
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
    , parentId : Maybe Id
    }


type CompletionStatus
    = Completed
    | Incomplete


type IndividualReport
    = ForIndividual Employee (WebData (List Id))


type AggregateReport
    = ForCourse Course (WebData (List Email))
    | ForOrganisation Organisation Course (WebData (List Email))


type Report
    = Individual IndividualReport
    | Aggregate AggregateReport


type alias InitialData =
    { courses : List Course
    , employees : List Employee
    , organisations : List Organisation
    }


type alias Model =
    { data : WebData InitialData
    , report : Maybe Report
    , sidebar : Sidebar
    , search : String
    , api : String
    }


type Sidebar
    = SearchIndividual
    | SearchAggregate


type alias Flags =
    { api : String }
