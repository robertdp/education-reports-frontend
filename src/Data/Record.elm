module Data.Record exposing (..)

import Dict


toDict : (record -> comparable) -> List record -> Dict.Dict comparable record
toDict toKey =
    List.foldl (\record dict -> Dict.insert (toKey record) record dict) Dict.empty


toDict2 : (record -> comparable1) -> (record -> comparable2) -> List record -> Dict.Dict comparable1 (Dict.Dict comparable2 record)
toDict2 toKey1 toKey2 =
    List.foldl
        (\record dict1 ->
            Dict.insert (toKey1 record)
                (Dict.get (toKey1 record) dict1
                    |> Maybe.withDefault Dict.empty
                    |> Dict.insert (toKey2 record) record
                )
                dict1
        )
        Dict.empty
