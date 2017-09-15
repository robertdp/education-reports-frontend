module Api exposing (..)

import Date
import Http
import Json.Decode exposing (..)
import RemoteData
import Set
import Types exposing (..)


loadInitialData : Url -> Cmd Msg
loadInitialData api =
    let
        endpoint =
            api ++ "/"
    in
        Http.get endpoint initialDataDecoder
            |> RemoteData.sendRequest
            |> Cmd.map InitialDataLoaded


loadEnrolmentData : Url -> Employee -> Cmd Msg
loadEnrolmentData api employee =
    let
        endpoint =
            api ++ "/enrolment?email=" ++ Http.encodeUri employee.email
    in
        Http.get endpoint (list enrolmentDecoder)
            |> RemoteData.sendRequest
            |> Cmd.map (EmployeeReportLoaded employee)


loadOrganisationData : Url -> Organisation -> Cmd Msg
loadOrganisationData api organisation =
    let
        endpoint =
            api ++ "/organisation?organisation_id=" ++ (Http.encodeUri <| toString organisation.id)
    in
        Http.get endpoint (list enrolmentDecoder)
            |> RemoteData.sendRequest
            |> Cmd.map (OrganisationReportLoaded organisation)



-- loadCourseAndOrganisationData : Url -> Course -> Organisation -> Cmd Msg
-- loadCourseAndOrganisationData url course organisation =
--     Cmd.none


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
