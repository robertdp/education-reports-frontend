module Component.CardBlock exposing (..)

import Color
import Element
import Element.Attributes as Attributes
import Style
import Style.Color as Color
import Style.Font as Font
import Style.Shadow as Shadow


type Style
    = None
    | Heading
    | Body


styles : (Style -> style) -> List (Style.Style style variation)
styles style =
    [ Style.style (style None) []
    , Style.style (style Heading)
        [ Color.text <| Color.rgb 99 99 99
        , Font.size 17
        ]
    , Style.style (style Body)
        [ Color.background Color.white
        , Shadow.box { offset = ( 0, 1 ), size = 0, blur = 1, color = Color.rgba 0 0 0 0.15 }
        , Shadow.box { offset = ( 0, 0 ), size = 0, blur = 1, color = Color.rgba 0 0 0 0.2 }
        ]
    ]


view :
    { a
        | style : Style -> style
        , header : model -> Element.Element style variation msg
        , content : model -> Element.Element style variation msg
    }
    -> model
    -> Element.Element style variation msg
view config model =
    Element.column (config.style None)
        [ Attributes.spacing 4 ]
        [ Element.row (config.style Heading)
            [ Attributes.height <| Attributes.px 32
            , Attributes.verticalCenter
            ]
            [ config.header model ]
        , Element.el (config.style Body) [] <|
            config.content model
        ]


padding : Element.Attribute variation msg
padding =
    Attributes.paddingXY 24 16


spacing : Element.Attribute variation msg
spacing =
    Attributes.spacing 16


paddedView :
    { a
        | style : Style -> style
        , header : model -> Element.Element style variation msg
        , content : model -> Element.Element style variation msg
    }
    -> model
    -> Element.Element style variation msg
paddedView config =
    view
        { config
            | content =
                config.content >> (Element.el (config.style None) [ padding ])
        }
