module App.Component.Loading exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
import Html
import Html.Attributes
import RemoteData exposing (..)


icon : Element style variation msg
icon =
    Html.i [ Html.Attributes.class ("fa fa-refresh fa-spin fa-3x fa-fw") ] []
        |> html


centeredIcon : style -> Element style variation msg
centeredIcon style =
    el style
        [ paddingXY 36 24
        , center
        ]
        icon


simpleSpinner : style -> (e -> Element style variation msg) -> (a -> Element style variation msg) -> RemoteData e a -> Element style variation msg
simpleSpinner toStyle onFailure onSuccess data =
    case data of
        NotAsked ->
            empty

        Loading ->
            centeredIcon toStyle

        Failure error ->
            onFailure error

        Success data ->
            onSuccess data
