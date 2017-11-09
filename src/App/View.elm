module App.View exposing (..)

-- import App.Component.Layout as Layout

import App.State exposing (..)
import Element exposing (..)
import Html exposing (Html)


type Style
    = None


view : Model -> Html Msg
view model =
    viewport stylesheet <| el None (text "test")
