module View exposing (..)

import Color
import Element exposing (Element, button, column, el, empty, row, text, viewport)
import Element.Attributes exposing (center, fill, height, paddingXY, px, verticalCenter, width)
import Element.Events exposing (onClick)
import Html
import RemoteData exposing (WebData)
import Style exposing (style)
import Style.Border as Border
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
    | Button


stylesheet : Style.StyleSheet Styles variation
stylesheet =
    Style.styleSheet
        [ style None []
        , style Main
            [ Font.typeface [ "Roboto", "メイリオ", "Meiryo", "sans-serif" ]
            , Font.size 15
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
        , style Button
            [ Border.rounded 5
            , Color.border Color.blue
            , Color.background Color.blue
            , Color.text Color.white
            , Style.prop "border-style" "outset"
            , Style.cursor "pointer"
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
                    [ button <|
                        el Button
                            [ onClick ToggleSidebarMode
                            , paddingXY 10 5
                            ]
                        <|
                            text "Change modes"
                    ]
                , sidebar model
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


sidebar : Model -> Element Styles variation msg
sidebar model =
    let
        getData f =
            model.data |> RemoteData.map f

        employees =
            getData .employees

        courses =
            getData .courses

        organisations =
            getData .organisations
    in
        case model.sidebar of
            SearchIndividual ->
                searchIndividual model.search employees

            SearchAggregate ->
                searchAggregate model.search courses organisations


searchIndividual : String -> WebData (List Employee) -> Element Styles variation msg
searchIndividual term employees =
    row None [] []


searchAggregate : String -> WebData (List Course) -> WebData (List Organisation) -> Element Styles variation msg
searchAggregate term courses organisations =
    empty
