module App.Page.User exposing (..)

import Element exposing (..)
import Style exposing (..)


type alias Model =
    ()


type Msg
    = Msg Msg


type Styles
    = None


init : Model
init =
    ()


styles : (Styles -> style) -> List (Style style variation)
styles style_ =
    let
        style =
            style_ >> Style.style
    in
        []


sidebar :
    { a
        | style : Styles -> style
        , msg : Msg -> msg
    }
    -> Model
    -> Element styles variation msg
sidebar config model =
    text "user sidebar"


update msg model =
    model ! []
