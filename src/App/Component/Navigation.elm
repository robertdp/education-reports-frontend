module App.Component.Navigation exposing (..)

import Color exposing (..)
import Data.Record as Record
import Data.Recursive exposing (..)
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Style exposing (..)
import Style.Color as Color
import Style.Font as Font
import Task


type Styles
    = None
    | Navigation
    | Heading
    | Item


type Msg a
    = Select (Maybe a)


type alias Model a =
    Maybe a


init : Model a
init =
    Nothing


initWithSelected : a -> Model a
initWithSelected =
    Just


styles : List (Style Styles variation)
styles =
    [ style None []
    , style Navigation
        [ Font.lineHeight 1.5
        ]
    , style Heading
        [ Color.background lightGray
        , Font.size 16
        , Font.light
        ]
    , style Item
        [ cursor "pointer"
        , hover
            [ Color.background lightYellow
            ]
        ]
    ]


view :
    { toId : a -> comparable
    , toParentId : a -> Maybe comparable
    , show : a -> String
    , title : String
    }
    -> List a
    -> Model a
    -> Element Styles variation (Msg a)
view { toId, toParentId, show, title } items selected =
    let
        itemMap =
            Record.toDict toId items

        children =
            selected
                |> Maybe.map (getDirectDescendants toId toParentId items)
                |> Maybe.withDefault (List.filter (toParentId >> (==) Nothing) items)

        navigateUpMsg =
            let
                parent =
                    selected
                        |> Maybe.andThen (getParent toParentId itemMap)
            in
                case ( selected, parent ) of
                    ( Nothing, _ ) ->
                        Nothing

                    ( _, Just parent ) ->
                        Just (Select <| Just parent)

                    _ ->
                        Just (Select Nothing)
    in
        column Navigation
            [ height fill ]
            [ selected
                |> Maybe.map show
                |> Maybe.withDefault title
                |> text
                |> el Heading
                    [ paddingXY 12 8
                    ]
            , navigateUpMsg
                |> Maybe.map
                    (\msg ->
                        el Item
                            [ onClick msg
                            , paddingXY 12 8
                            ]
                            (text "← Go back")
                    )
                |> Maybe.withDefault empty
            , if List.isEmpty children then
                empty
              else
                children
                    |> List.map
                        (\item ->
                            el Item
                                [ onClick (Select <| Just item)
                                , paddingXY 12 8
                                ]
                                (text <| show item)
                        )
                    |> column None [ yScrollbar ]
            ]


update : (Maybe a -> msg) -> Msg a -> Model a -> ( Model a, Cmd msg )
update onChange msg model =
    case msg of
        Select item ->
            if model /= item then
                item
                    ! [ Task.succeed item
                            |> Task.perform onChange
                      ]
            else
                model ! []
