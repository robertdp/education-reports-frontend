module Data.Recursive exposing (..)

import Dict


type alias Recursive id a =
    { a
        | id : id
        , parentId : Maybe id
    }


getDirectDescendants :
    Dict.Dict comparable (Recursive comparable a)
    -> Recursive comparable a
    -> List (Recursive comparable a)
getDirectDescendants store parent =
    store
        |> Dict.values
        |> List.filter (.parentId >> ((==) <| Just parent.id))


getAllDescendants :
    Dict.Dict comparable (Recursive comparable a)
    -> Recursive comparable a
    -> List (Recursive comparable a)
getAllDescendants store parent =
    let
        descendants =
            getDirectDescendants store parent
    in
        descendants ++ List.foldl (getAllDescendants store >> (++)) [] descendants


getWithAllDescendants :
    Dict.Dict comparable (Recursive comparable a)
    -> Recursive comparable a
    -> List (Recursive comparable a)
getWithAllDescendants store parent =
    [ parent ] ++ getAllDescendants store parent


getParent :
    Dict.Dict comparable (Recursive comparable a)
    -> Recursive comparable a
    -> Maybe (Recursive comparable a)
getParent store child =
    child.parentId
        |> Maybe.andThen (flip Dict.get store)


getAncestry :
    Dict.Dict comparable (Recursive comparable a)
    -> Recursive comparable a
    -> List (Recursive comparable a)
getAncestry store child =
    getParent store child
        |> Maybe.map (\parent -> parent :: getAncestry store parent)
        |> Maybe.withDefault []


getWithAncestry :
    Dict.Dict comparable (Recursive comparable a)
    -> Recursive comparable a
    -> List (Recursive comparable a)
getWithAncestry store child =
    child :: getAncestry store child
