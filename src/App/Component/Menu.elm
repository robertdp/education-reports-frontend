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
    | Tab Tab
    | ActiveTab


type alias Model =
    Tab


type Msg
    = NoOp
    | SelectTab Tab


type Tab
    = Summary
    | Search
    | User


styles : (Styles -> class) -> List (Style class variation)
styles style_ =
    let
        style =
            style_ >> Style.style
    in
        [ style None []
        , style Tabs
            [ cursor "pointer"
            , Font.center
            ]
        , style (Tab Summary)
            [ Color.text white
            , hover
                [ Color.background darkGrey
                ]
            ]
        , style (Tab Search)
            [ Color.text white
            , hover
                [ Color.background darkGrey
                ]
            ]
        , style (Tab User)
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


init : Model
init =
    Summary


icon : Tab -> Element style variation msg
icon tab =
    let
        class =
            case tab of
                Summary ->
                    "fa-th-large"

                Search ->
                    "fa-search"

                User ->
                    "fa-user"
    in
        Html.i [ Html.Attributes.class ("fa fa-fw fa-1x " ++ class) ] []
            |> html


view :
    { a
        | style : Styles -> style
        , msg : Msg -> msg
    }
    -> Model
    -> Element style variation msg
view config model =
    let
        tabs =
            [ Summary, Search, User ]

        class tab =
            if tab == model then
                ActiveTab
            else
                Tab tab

        click tab =
            config.msg
                (if tab == model then
                    NoOp
                 else
                    (SelectTab tab)
                )
                |> onClick
    in
        row (config.style Tabs)
            [ width fill
            , height fill
            , spread
            ]
            (tabs
                |> List.map
                    (\tab ->
                        icon tab
                            |> el (config.style None)
                                [ verticalCenter
                                , center
                                ]
                            |> el (config.style <| class tab)
                                [ width fill
                                , height (px 60)
                                , click tab
                                ]
                    )
            )


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        SelectTab tab ->
            tab
