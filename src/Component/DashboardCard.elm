module Component.DashboardCard exposing (..)

import Color
import Element
import Element.Attributes
import Style
import Style.Color
import Style.Font
import Style.Shadow


type Style
    = None
    | Heading
    | Body


styles : (Style -> style) -> List (Style.Style style variation)
styles style =
    [ Style.style (style None) []
    , Style.style (style Heading)
        [ Style.Color.text <| Color.rgb 99 99 99
        , Style.Font.size 17
        ]
    , Style.style (style Body)
        [ Style.Color.background Color.white
        , Style.shadows
            [ Style.Shadow.box { offset = ( 0, 1 ), size = 0, blur = 1, color = Color.rgba 0 0 0 0.15 }
            , Style.Shadow.box { offset = ( 0, 0 ), size = 0, blur = 1, color = Color.rgba 0 0 0 0.2 }
            ]
        ]
    ]


view : (Style -> style) -> { a | heading : String, body : Element.Element style variation msg } -> Element.Element style variation msg
view style { heading, body } =
    Element.column (style None)
        [ Element.Attributes.spacing 4 ]
        [ Element.row (style Heading)
            [ Element.Attributes.height <| Element.Attributes.px 32
            , Element.Attributes.verticalCenter
            ]
            [ Element.text heading ]
        , Element.el (style Body) [] body
        ]


padding : Element.Attribute variation msg
padding =
    Element.Attributes.paddingXY 24 16


spacing : Element.Attribute variation msg
spacing =
    Element.Attributes.spacing 16


paddedView : (Style -> style) -> { a | heading : String, body : Element.Element style variation msg } -> Element.Element style variation msg
paddedView style config =
    view style
        { config
            | body = Element.el (style None) [ padding ] config.body
        }
