module Component.Layout exposing (..)

import Color
import Dom
import Dom.Scroll
import Element
import Element.Attributes as Attributes
import Style
import Style.Background as Background
import Style.Color as Color
import Style.Font as Font
import Style.Shadow as Shadow
import Task
import Types exposing (..)


type Style
    = None
    | Main
    | Header
    | Sidebar
    | Content


styles : (Style -> style) -> List (Style.Style style variation)
styles style =
    [ Style.style (style None) []
    , Style.style (style Main)
        [ Font.typeface
            [ Font.font "Roboto"
            , Font.font "メイリオ"
            , Font.font "Meiryo"
            , Font.sansSerif
            ]
        , Font.size 14
        ]
    , Style.style (style Header)
        [ Background.gradient 0
            [ Background.step <| Color.rgb 39 50 56
            , Background.step <| Color.rgb 58 74 82
            ]
        , Font.size 20
        , Color.text Color.white
        ]
    , Style.style (style Sidebar)
        []
    , Style.style (style Content)
        [ Color.background <| Color.rgb 240 241 242
        , Shadow.inset { offset = ( 5, 0 ), size = -6, blur = 6, color = Color.rgba 0 0 0 0.5 }
        ]
    ]


view :
    { a
        | style : Style -> style
        , header : model -> Element.Element style variation msg
        , sidebar : model -> Element.Element style variation msg
        , content : model -> Element.Element style variation msg
    }
    -> model
    -> Element.Element style variation msg
view config model =
    Element.row (config.style Main)
        [ Attributes.height <| Attributes.fillPortion 1 ]
        [ Element.column (config.style Sidebar)
            [ Attributes.width <| Attributes.px 256 ]
            [ config.sidebar model ]
        , Element.column (config.style None)
            [ Attributes.width <| Attributes.fillPortion 1 ]
            [ Element.row (config.style Header)
                [ Attributes.height <| Attributes.px 60
                , Attributes.verticalCenter
                , Attributes.paddingXY 32 0
                ]
                [ config.header model ]
            , Element.el (config.style Content)
                [ Attributes.height <| Attributes.fillPortion 1
                , Attributes.yScrollbar
                , Attributes.id "layout-content"
                ]
              <|
                Element.el (config.style None)
                    [ Attributes.paddingXY 32 24
                    ]
                <|
                    config.content model
            ]
        ]


scrollContentToTop : Cmd Msg
scrollContentToTop =
    Dom.Scroll.toTop "layout-content"
        |> Task.attempt (always NoOp)
