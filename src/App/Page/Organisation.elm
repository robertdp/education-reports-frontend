module App.Page.Organisation exposing (..)

import App.Component.Loading as Loading
import App.Component.Navigation as Navigation
import App.Data exposing (..)
import Element exposing (..)
import Element.Attributes exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Style exposing (..)
import Style.Font as Font
import Style.Sheet as Sheet


type alias Model =
    { enrolments : WebData (List Enrolment)
    , search : String
    , organisationNav : Navigation.Model Organisation
    , categoryNav : Navigation.Model Category
    }


type Msg
    = SelectCategory (Maybe Category)
    | SelectOrganisation (Maybe Organisation)
    | LoadedEnrolments (WebData (List Enrolment))
    | OrganisationNavigationMsg (Navigation.Msg Organisation)
    | CategoryNavigationMsg (Navigation.Msg Category)


type Styles
    = None
    | Search
    | LoadingSpinner
    | NavigationStyle Navigation.Styles


init : Model
init =
    { enrolments = NotAsked
    , search = ""
    , organisationNav = Navigation.init
    , categoryNav = Navigation.init
    }


styles : List (Style Styles variation)
styles =
    let
        map toStyle =
            Sheet.map toStyle identity >> Sheet.merge
    in
        [ style None []
        , style Search
            [ Font.light
            , Font.size 16
            ]
        , map NavigationStyle Navigation.styles
        ]


sidebar : Model -> WebData (List Organisation) -> WebData (List Category) -> WebData (List Course) -> Element Styles variation Msg
sidebar model organisations categories courses =
    column None
        [ height fill
        ]
        [ organisations
            |> Loading.simpleSpinner LoadingSpinner
                (always empty)
                (\organisations ->
                    Navigation.view
                        { toId = .id
                        , toParentId = .parentId
                        , show = .name
                        , title = "Select an organisation"
                        }
                        organisations
                        model.organisationNav
                        |> mapAll OrganisationNavigationMsg NavigationStyle identity
                )
            |> el None
                [ height <| percent 50
                ]
        , categories
            |> Loading.simpleSpinner LoadingSpinner
                (always empty)
                (\categories ->
                    Navigation.view
                        { toId = .id
                        , toParentId = .parentId
                        , show = .name
                        , title = "Select a category"
                        }
                        categories
                        model.categoryNav
                        |> mapAll CategoryNavigationMsg NavigationStyle identity
                )
            |> el None
                [ height <| percent 50
                ]
        ]


update : Url -> Msg -> Model -> ( Model, Cmd Msg )
update api msg model =
    case msg of
        OrganisationNavigationMsg msg_ ->
            let
                ( organisationNav, cmds ) =
                    Navigation.update SelectOrganisation msg_ model.organisationNav
            in
                { model | organisationNav = organisationNav } ! [ cmds ]

        CategoryNavigationMsg msg_ ->
            let
                ( categoryNav, cmds ) =
                    Navigation.update SelectCategory msg_ model.categoryNav
            in
                { model | categoryNav = categoryNav } ! [ cmds ]

        SelectOrganisation organisation ->
            model ! []

        _ ->
            model ! []
