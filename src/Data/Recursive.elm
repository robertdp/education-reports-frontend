module Data.Recursive exposing (..)

import Dict


getDirectDescendants :
    (a -> comparable)
    -> (a -> Maybe comparable)
    -> Dict.Dict comparable a
    -> a
    -> List a
getDirectDescendants toId toParentId store parent =
    store
        |> Dict.values
        |> List.filter (toParentId >> ((==) <| Just <| toId parent))


getAllDescendants :
    (a -> comparable)
    -> (a -> Maybe comparable)
    -> Dict.Dict comparable a
    -> a
    -> List a
getAllDescendants toId toParentId store parent =
    let
        descendants =
            getDirectDescendants toId toParentId store parent
    in
        descendants ++ List.foldl (getAllDescendants toId toParentId store >> (++)) [] descendants


getWithAllDescendants :
    (a -> comparable)
    -> (a -> Maybe comparable)
    -> Dict.Dict comparable a
    -> a
    -> List a
getWithAllDescendants toId toParentId store parent =
    parent :: getAllDescendants toId toParentId store parent


getParent :
    (a -> Maybe comparable)
    -> Dict.Dict comparable a
    -> a
    -> Maybe a
getParent toParentId store child =
    child
        |> toParentId
        |> Maybe.andThen (flip Dict.get store)


getAncestry :
    (a -> Maybe comparable)
    -> Dict.Dict comparable a
    -> a
    -> List a
getAncestry toParentId store child =
    getParent toParentId store child
        |> Maybe.map (\parent -> parent :: getAncestry toParentId store parent)
        |> Maybe.withDefault []


getWithAncestry :
    (a -> Maybe comparable)
    -> Dict.Dict comparable a
    -> a
    -> List a
getWithAncestry toParentId store child =
    child :: getAncestry toParentId store child
