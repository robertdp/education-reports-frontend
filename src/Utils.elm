module Utils exposing (..)


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
