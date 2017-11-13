module App.Component.Card exposing (..)

import Color exposing (..)
import Element exposing (..)
import Element.Attributes exposing (..)
import Style exposing (..)
import Style.Color as Color
import Style.Font as Font
import Style.Shadow as Shadow


type Styles
    = None
    | Heading
    | Content


styles : List (Style Styles variation)
styles =
    [ style None []
    , style Heading
        [ Color.text <| Color.rgb 99 99 99
        , Font.size 17
        ]
    , style Content
        [ Color.background Color.white
        , Shadow.box { offset = ( 0, 1 ), size = 0, blur = 1, color = Color.rgba 0 0 0 0.15 }
        , Shadow.box { offset = ( 0, 0 ), size = 0, blur = 1, color = Color.rgba 0 0 0 0.2 }
        ]
    ]


view :
    { a
        | toStyle : Styles -> style
        , header : model -> Element.Element style variation msg
        , content : model -> Element.Element style variation msg
    }
    -> model
    -> Element.Element style variation msg
view { toStyle, header, content } model =
    column (toStyle None)
        [ spacing 4 ]
        [ row (toStyle Heading)
            [ height (px 32)
            , verticalCenter
            ]
            [ header model ]
        , content model
            |> el (toStyle Content) []
        ]


padding : Attribute variation msg
padding =
    paddingXY 24 16


paddedView :
    { a
        | toStyle : Styles -> style
        , header : model -> Element style variation msg
        , content : model -> Element style variation msg
    }
    -> model
    -> Element style variation msg
paddedView ({ toStyle, content } as config) =
    view
        { config
            | content =
                content
                    >> (el (toStyle None)
                            [ padding
                            ]
                       )
        }
