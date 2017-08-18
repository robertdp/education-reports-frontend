module Types exposing (..)

import RemoteData exposing (WebData)


type Msg
    = InitialDataResponse (WebData InitialData)
    | SearchIndividual String
    | SearchAggregate String
    | SelectEmployee Employee
    | SelectCourse Course
    | SelectOrganisation Organisation
    | ToggleSidebarMode


type alias Email =
    String


type alias Id =
    Int


type alias Category =
    { id : Id
    , name : String
    , parentId : Maybe Id
    }


type alias Course =
    { id : Id
    , fullName : String
    , shortName : String
    , categoryId : Id
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
    , sidebarMode : Bool
    , api : String
    }


type alias Flags =
    { api : String }
