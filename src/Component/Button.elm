module Component.Button exposing (..)

import Color
import Element
import Element.Attributes as Attributes
import Element.Events as Events
import Style
import Style.Background as Background
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Style.Shadow as Shadow


type Style
    = None
    | Button


styles : (Style -> style) -> List (Style.Style style variation)
styles style =
    [ Style.style (style None)
        []
    , Style.style (style Button)
        [ Border.none
        , Color.text Color.white
        , Border.rounded 3
        , Font.lineHeight 1.5
        , Font.size 16
        , Shadow.text { offset = ( 1, 1 ), blur = 0.5, color = Color.rgba 0 0 0 0.6 }
        , Background.gradient 0
            [ Background.step <| Color.rgb 59 119 219
            , Background.step <| Color.rgb 66 133 244
            ]
        , Style.cursor "pointer"
        ]
    ]


view :
    { a
        | style : Style -> style
        , onClick : msg
    }
    -> Element.Element style variation msg
    -> Element.Element style variation msg
view config content =
    let
        attributes =
            [ Attributes.width Attributes.fill
            , Attributes.paddingXY 12 6
            ]

        events =
            [ Events.onClick config.onClick ]
    in
        Element.button
            (config.style Button)
            (attributes ++ events)
            content
