module Api exposing (..)

import Http
import Json.Decode exposing (..)
import RemoteData
import Types exposing (..)


loadInitialData : String -> Cmd Msg
loadInitialData api =
    let
        endpoint =
            api ++ "/initial"
    in
        Http.get endpoint decodeInitialData
            |> RemoteData.sendRequest
            |> Cmd.map InitialDataResponse


decodeInitialData : Decoder InitialData
decodeInitialData =
    map3 InitialData
        (field "courses" (list decodeCourse))
        (field "employees" (list decodeEmployee))
        (field "organisations" (list decodeOrganisation))


decodeCourse : Decoder Course
decodeCourse =
    map4 Course
        (field "id" int)
        (field "fullName" string)
        (field "shortName" string)
        (succeed 0)


decodeEmployee : Decoder Employee
decodeEmployee =
    map4 Employee
        (field "email" string)
        (field "name" string)
        (field "number" (maybe string))
        (field "organisationId" int)


decodeOrganisation : Decoder Organisation
decodeOrganisation =
    map4 Organisation
        (field "id" int)
        (field "name" string)
        (field "managerEmails" (list string))
        (field "parentId" (maybe int))
