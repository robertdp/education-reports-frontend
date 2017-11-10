module App.Component.Layout exposing (..)

import Color exposing (..)
import Element exposing (..)
import Element.Attributes exposing (..)
import Style exposing (..)
import Style.Color as Color
import Style.Font as Font
import Style.Shadow as Shadow


type Styles
    = None
    | Main
    | Header
    | Sidebar
    | Content


styles : (Styles -> class) -> List (Style class variation)
styles style_ =
    let
        style =
            style_ >> Style.style
    in
        [ style None []
        , style Main
            [ Font.typeface
                [ Font.font "Roboto"
                , Font.font "メイリオ"
                , Font.font "Meiryo"
                , Font.sansSerif
                ]
            , Font.size 14
            , Color.background (rgb 230 232 234)
            ]
        , style Header
            [ Color.background blue
            , Font.size 24
            , Color.text white
            , Shadow.text { offset = ( 0, 0.5 ), blur = 1, color = black }
            ]
        , style Sidebar
            [ Color.background white
            , Shadow.box { offset = ( 5, 0 ), size = -6, blur = 6, color = rgba 0 0 0 0.5 }
            ]
        , style Content []
        ]


view :
    { a
        | style : Styles -> style
        , menu : model -> Element style variation msg
        , title : model -> String
        , sidebar : model -> Maybe (Element style variation msg)
        , content : model -> Element style variation msg
    }
    -> model
    -> Element style variation msg
view config model =
    let
        style =
            config.style
    in
        column (style Main)
            [ width fill
            , height fill
            , spread
            ]
            [ row (style Header)
                [ height (px 60)
                ]
                [ el (style None)
                    [ width (px 256)
                    ]
                    (config.menu model)
                , el (style None)
                    [ verticalCenter
                    , paddingXY 32 0
                    ]
                    (text (config.title model))
                ]
            , row (style None)
                [ height fill
                ]
                [ config.sidebar model
                    |> Maybe.map
                        (el (style Sidebar)
                            [ width (px 256)
                            ]
                        )
                    |> Maybe.withDefault empty
                , config.content model
                    |> el (style Content) [ paddingXY 32 24 ]
                ]
            ]
