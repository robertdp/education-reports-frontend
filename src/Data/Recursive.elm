module Data.Recursive exposing (..)

import Dict


getDirectDescendants :
    (a -> comparable)
    -> (a -> Maybe comparable)
    -> List a
    -> a
    -> List a
getDirectDescendants toId toParentId items parent =
    items
        |> List.filter (toParentId >> ((==) <| Just <| toId parent))


getAllDescendants :
    (a -> comparable)
    -> (a -> Maybe comparable)
    -> List a
    -> a
    -> List a
getAllDescendants toId toParentId items parent =
    let
        descendants =
            getDirectDescendants toId toParentId items parent
    in
        descendants ++ List.foldl (getAllDescendants toId toParentId items >> (++)) [] descendants


getWithAllDescendants :
    (a -> comparable)
    -> (a -> Maybe comparable)
    -> List a
    -> a
    -> List a
getWithAllDescendants toId toParentId items parent =
    parent :: getAllDescendants toId toParentId items parent


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
