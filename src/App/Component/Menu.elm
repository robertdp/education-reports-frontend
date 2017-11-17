module App.Component.Menu exposing (..)

import Color exposing (..)
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Html
import Html.Attributes
import Style exposing (..)
import Style.Color as Color
import Style.Font as Font
import Style.Shadow as Shadow


type Styles
    = None
    | Tabs
    | InactiveTab
    | ActiveTab


type alias Model =
    Tab


type Msg
    = SelectTab Tab


type Tab
    = Summary
    | Organisation
    | Employee


init : Model
init =
    Organisation


styles : List (Style Styles variation)
styles =
    [ style None []
    , style Tabs
        [ cursor "pointer"
        , Font.center
        ]
    , style InactiveTab
        [ Color.text white
        , hover
            [ Color.background darkGrey
            ]
        ]
    , style ActiveTab
        [ Color.text charcoal
        , Color.background white
        , Shadow.text { offset = ( 0.0, 0.0 ), blur = 0.0, color = white }
        ]
    ]


icon : Tab -> Element style variation msg
icon tab =
    let
        class =
            case tab of
                Summary ->
                    "fa-th"

                Organisation ->
                    "fa-sitemap"

                Employee ->
                    "fa-users"
    in
        Html.i [ Html.Attributes.class ("fa fa-fw fa-1x " ++ class) ] []
            |> html


view : Model -> Element Styles variation Msg
view model =
    let
        tabs =
            [ Summary, Organisation, Employee ]

        class tab =
            if tab == model then
                ActiveTab
            else
                InactiveTab
    in
        row Tabs
            [ width fill
            , height fill
            , spread
            ]
            (tabs
                |> List.map
                    (\tab ->
                        icon tab
                            |> el None
                                [ verticalCenter
                                , center
                                ]
                            |> el (class tab)
                                [ width fill
                                , height (px 60)
                                , onClick <| SelectTab tab
                                , toAttr <| Html.Attributes.title <| toString tab
                                ]
                    )
            )


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectTab tab ->
            tab
