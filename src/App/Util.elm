module App.Util exposing (..)

import Element
import Style
import Html.Lazy


matchAny : List (a -> Bool) -> a -> Bool
matchAny predicates item =
    List.foldl (\f a -> a || f item) False predicates


matchAll : List (a -> Bool) -> a -> Bool
matchAll predicates item =
    List.foldl (\f a -> a && f item) True predicates


matchNone : List (a -> Bool) -> (a -> Bool)
matchNone predicates =
    not << matchAny predicates


filterAny : List (a -> Bool) -> List a -> List a
filterAny =
    matchAny >> List.filter


filterAll : List (a -> Bool) -> List a -> List a
filterAll =
    matchAll >> List.filter


filterNone : List (a -> Bool) -> List a -> List a
filterNone =
    matchNone >> List.filter


makeLazy :
    Style.StyleSheet style variation
    -> (model -> Element.Element style variation msg)
    -> (model -> Element.Element style variation msg)
makeLazy stylesheet view =
    let
        view_ model =
            view model
                |> Element.toHtml stylesheet
    in
        Html.Lazy.lazy view_
            >> Element.html
