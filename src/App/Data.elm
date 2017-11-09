module App.Data exposing (..)

import Date exposing (Date)
import Http
import Json.Decode exposing (..)
import RemoteData exposing (WebData)
import Set exposing (Set)


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


type alias InitialData =
    { courses : List Course
    , employees : List Employee
    , organisations : List Organisation
    , organisationSummaries : List OrganisationSummary
    }


loadInitialData : (WebData InitialData -> msg) -> Url -> Cmd msg
loadInitialData toMsg api =
    let
        endpoint =
            api ++ "/"
    in
        Http.get endpoint initialDataDecoder
            |> RemoteData.sendRequest
            |> Cmd.map toMsg


loadEmployeeEnrolments : (WebData (List Enrolment) -> msg) -> Url -> Employee -> Cmd msg
loadEmployeeEnrolments toMsg api employee =
    let
        endpoint =
            api ++ "/enrolment?email=" ++ Http.encodeUri employee.email
    in
        Http.get endpoint (list enrolmentDecoder)
            |> RemoteData.sendRequest
            |> Cmd.map toMsg


loadOrganisationEnrolments : (WebData (List Enrolment) -> msg) -> Url -> Maybe Organisation -> Maybe Course -> Cmd msg
loadOrganisationEnrolments toMsg api organisation course =
    if organisation == Nothing && course == Nothing then
        Cmd.none
    else
        let
            endpoint =
                [ organisation |> Maybe.map (.id >> toString >> (++) "organisation_id=")
                , course |> Maybe.map (.id >> toString >> (++) "course_id=")
                ]
                    |> List.filterMap identity
                    |> String.join "&"
                    |> (\query ->
                            api
                                ++ "/organisation?"
                                ++ query
                       )
        in
            Http.get endpoint (list enrolmentDecoder)
                |> RemoteData.sendRequest
                |> Cmd.map toMsg


initialDataDecoder : Decoder InitialData
initialDataDecoder =
    map4 InitialData
        (field "courses" (list courseDecoder |> map (List.sortBy .name)))
        (field "employees" (list employeeDecoder |> map (List.sortBy .name)))
        (field "organisations" (list organisationDecoder |> map (List.sortBy .name)))
        (field "organisation_summaries" (list organisationSummaryDecoder))


courseDecoder : Decoder Course
courseDecoder =
    map3 Course
        (field "id" int)
        (field "name" string)
        (field "short_name" string)


employeeDecoder : Decoder Employee
employeeDecoder =
    map4 Employee
        (field "email" string)
        (field "name" string)
        (field "employee_number" (maybe string))
        (field "organisation_id" int)


organisationDecoder : Decoder Organisation
organisationDecoder =
    map5 Organisation
        (field "id" int)
        (field "name" string)
        (field "manager_emails" (list string |> map Set.fromList))
        (field "employee_emails" (list string |> map Set.fromList))
        (field "parent_id" (maybe int))


enrolmentDecoder : Decoder Enrolment
enrolmentDecoder =
    map3 Enrolment
        (field "employee_email" string)
        (field "course_id" int)
        (map2
            (\isCompleted time ->
                if isCompleted then
                    Completed time
                else
                    Enrolled
            )
            (field "is_completed" bool)
            (field "completed_at"
                (maybe string
                    |> map
                        (Maybe.andThen (Date.fromString >> Result.toMaybe)
                            >> Maybe.withDefault (Date.fromTime 0)
                        )
                )
            )
        )


organisationSummaryDecoder : Decoder OrganisationSummary
organisationSummaryDecoder =
    map5 OrganisationSummary
        (field "organisation_id" int)
        (field "course_id" int)
        (field "member_count" int)
        (field "enrolled" int)
        (field "completed" int)