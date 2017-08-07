module Main exposing (..)

import Html


main : Program Never () msg
main =
    Html.beginnerProgram
        { model = ()
        , update = \_ _ -> ()
        , view = \_ -> Html.text "Hi"
        }
