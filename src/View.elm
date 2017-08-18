module View exposing (..)

import Color
import Element exposing (button, column, el, empty, row, text, viewport)
import Element.Attributes exposing (center, fill, height, paddingXY, px, verticalCenter, width)
import Html
import Style exposing (style)
import Style.Color as Color
import Style.Font as Font
import Style.Shadow as Shadow
import Types exposing (..)


type Styles
    = None
    | Main
    | Sidebar
    | Heading
    | Body


stylesheet : Style.StyleSheet Styles variation
stylesheet =
    Style.styleSheet
        [ style None []
        , style Main
            [ Font.typeface [ "Roboto", "メイリオ", "Meiryo", "sans-serif" ]
            ]
        , style Sidebar
            []
        , style Heading
            [ Color.background <| Color.rgb 58 74 82
            , Font.size 20
            , Color.text Color.white
            ]
        , style Body
            [ Color.background <| Color.rgb 240 241 242
            , Style.shadows
                [ Shadow.inset { offset = ( 5, 0 ), size = -6, blur = 6, color = Color.rgba 0 0 0 0.5 }
                ]
            ]
        ]


view : Model -> Html.Html Msg
view model =
    viewport stylesheet <|
        row Main
            [ height <| fill 1 ]
            [ column Sidebar
                [ width <| px 256 ]
                [ row None
                    [ height <| px 60
                    , verticalCenter
                    , center
                    ]
                    [ el None [] (button (text "Change modes")) ]
                , empty
                ]
            , column None
                [ width <| fill 1 ]
                [ row Heading
                    [ height <| px 60
                    , verticalCenter
                    , paddingXY 32 0
                    ]
                    [ el None [] (text "HUE Education Reports") ]
                , el Body [ height <| fill 1 ] (el None [ paddingXY 32 24 ] (text "hi"))
                ]
            ]
