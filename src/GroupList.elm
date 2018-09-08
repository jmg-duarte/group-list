module GroupList exposing (group, interleave)

import List exposing (..)


{-| Group elements from a list in smaller groups of the given size

    group 3 <| List.range 0 9 == [ [ 0, 1, 2 ], [ 3, 4, 5 ], [ 6, 7, 8 ], [ 9 ] ]

    group 3 <| String.toList "Hello, World!" == [ [ 'H', 'e', 'l' ], [ 'l', 'o', ',' ], [ ' ', 'W', 'o' ], [ 'r', 'l', 'd' ], [ '!' ] ]

-}
group : Int -> List a -> List (List a)
group n l =
    if n <= 0 then
        case l of
            [] ->
                []

            _ ->
                [ l ]

    else
        case l of
            [] ->
                []

            _ ->
                List.take n l :: (group n <| List.drop n l)


{-| Interleave elements in a list at a specified interval

    interleave 3 -1 <| List.range 0 9 == [ [ 0, 1, 2 ], [ 3, 4, 5 ], [ 6, 7, 8 ], [ 9 ] ]

    interleave 3 '.' "Hello, World!" == [ 'H', 'e', 'l', '.', 'l', 'o', ',', '.', ' ', 'W', 'o', '.', 'r', 'l', 'd', '.', '!' ]

-}
interleave : Int -> a -> List a -> List a
interleave n val l =
    group n l |> List.intersperse [ val ] |> List.concat
