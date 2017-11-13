module App.Component.Layout exposing (..)

import Color exposing (..)
import Element exposing (..)
import Element.Attributes exposing (..)
import RemoteData exposing (..)
import Style exposing (..)
import Style.Background as Background
import Style.Color as Color
import Style.Font as Font
import Style.Shadow as Shadow


type Styles
    = None
    | Main
    | Header
    | Sidebar
    | Content


styles : List (Style Styles variation)
styles =
    [ style None []
    , style Main
        [ cursor "default"
        , Font.typeface
            [ Font.font "Roboto"
            , Font.font "メイリオ"
            , Font.font "Meiryo"
            , Font.sansSerif
            ]
        , Font.size 14
        , Color.background (rgb 230 232 234)
        ]
    , style Header
        [ -- Color.background blue
          Background.gradient 1 [ Background.step blue, Background.step red ]
        , Font.size 24
        , Color.text white
        , Shadow.text { offset = ( 0, 0.5 ), blur = 1, color = black }
        , Font.light
        ]
    , style Sidebar
        [ Color.background white
        , Shadow.box { offset = ( 5, 0 ), size = -6, blur = 6, color = rgba 0 0 0 0.5 }
        ]
    , style Content []
    ]


view :
    (Styles -> style)
    ->
        { a
            | menu : model -> Element style variation msg
            , title : model -> String
            , sidebar : model -> Maybe (Element style variation msg)
            , content : model -> Element style variation msg
        }
    -> model
    -> Element style variation msg
view toStyle { menu, title, sidebar, content } model =
    column (toStyle Main)
        [ width fill
        , height fill
        , scrollbars
        ]
        [ row (toStyle Header)
            [ height (px 60)
            ]
            [ el (toStyle None)
                [ width (px 256)
                ]
                (menu model)
            , el (toStyle None)
                [ verticalCenter
                , paddingXY 32 0
                ]
                (text (title model))
            ]
        , row (toStyle None)
            [ height fill
            ]
            [ sidebar model
                |> Maybe.map
                    (el (toStyle Sidebar)
                        [ width (px 256)
                        , height fill
                        , yScrollbar
                        ]
                    )
                |> Maybe.withDefault empty
            , content model
                |> el (toStyle Content)
                    [ paddingXY 32 24
                    , width fill
                    , height fill
                    , yScrollbar
                    ]
            ]
        ]
