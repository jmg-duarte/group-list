module GroupList exposing (group, groupInterleave)

import List exposing (..)


group : Int -> List a -> List (List a)
group n l =
    case l of
        [] ->
            []

        _ ->
            List.take n l :: (group n <| List.drop n l)


groupInterleave : Int -> a -> List a -> List a
groupInterleave n val l =
    group n l |> List.intersperse [ val ] |> List.concat
