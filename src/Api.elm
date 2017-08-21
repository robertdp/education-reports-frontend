module Api exposing (..)

import Http
import Json.Decode exposing (..)
import RemoteData
import Types exposing (..)


loadInitialData : Url -> Cmd Msg
loadInitialData api =
    let
        endpoint =
            api ++ "/initial"
    in
        Http.get endpoint initialDataDecoder
            |> RemoteData.sendRequest
            |> Cmd.map InitialDataResponse


loadEmployeeData : Url -> Employee -> Cmd Msg
loadEmployeeData api employee =
    Cmd.none


loadCourseData : Url -> Course -> Cmd Msg
loadCourseData api course =
    Cmd.none


loadCourseAndOrganisationData : Url -> Course -> Organisation -> Cmd Msg
loadCourseAndOrganisationData url course organisation =
    Cmd.none


initialDataDecoder : Decoder InitialData
initialDataDecoder =
    map3 InitialData
        (field "courses" (list courseDecoder))
        (field "employees" (list employeeDecoder))
        (field "organisations" (list organisationDecoder))


courseDecoder : Decoder Course
courseDecoder =
    map3 Course
        (field "id" int)
        (field "fullName" string)
        (field "shortName" string)


employeeDecoder : Decoder Employee
employeeDecoder =
    map4 Employee
        (field "email" string)
        (field "name" string)
        (field "number" (maybe string))
        (field "organisationId" int)


organisationDecoder : Decoder Organisation
organisationDecoder =
    map4 Organisation
        (field "id" int)
        (field "name" string)
        (field "managerEmails" (list string))
        (field "parentId" (maybe int))
