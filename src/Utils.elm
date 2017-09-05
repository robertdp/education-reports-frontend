module Utils exposing (..)


predicateFilterAny : List (a -> Bool) -> List a -> List a
predicateFilterAny predicates =
    List.filter (\x -> List.foldr (\f a -> a || f x) False predicates)


predicateFilterAll : List (a -> Bool) -> List a -> List a
predicateFilterAll predicates =
    List.filter (\x -> List.foldl (\f a -> a && f x) True predicates)
