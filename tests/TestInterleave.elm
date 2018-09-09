module TestInterleave exposing (testInterleave)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import GroupList exposing (..)
import List exposing (intersperse)
import Test exposing (..)


testInterleave : Test
testInterleave =
    describe "GroupList.interleave"
        [ testEmptyLists, testNonEmptyLists ]


testEmptyLists : Test
testEmptyLists =
    describe "GroupList.interleave for empty lists"
        [ test "interleave on an empty list with n < 0 has no effect" <|
            \_ -> Expect.equal [] <| interleave -1 0 []
        , test "interleave on an empty list with n == 0 has no effect" <|
            \_ -> Expect.equal [] <| interleave 0 0 []
        , test "interleave on an empty list with n > 0 has no effect" <|
            \_ -> Expect.equal [] <| interleave 1 0 []
        ]


testNonEmptyLists : Test
testNonEmptyLists =
    describe "GroupList.interleave for non empty lists"
        [ test "interleave on an non empty list with n < 0 has no effect" <|
            \_ ->
                let
                    input =
                        [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 ]

                    output =
                        input
                in
                Expect.equal output <| interleave -1 -1 input
        , test "interleave on a non empty list with n == 0 has no effect" <|
            \_ ->
                let
                    input =
                        [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 ]

                    output =
                        input
                in
                Expect.equal output <| interleave 0 -1 input
        , test "interleave on a non empty list with n == 1 has the same effect as List.intersperse" <|
            \_ ->
                let
                    value =
                        -1

                    input =
                        [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 ]

                    output =
                        List.intersperse value input
                in
                Expect.equal output <| interleave 1 value input
        , test "interleave on an non empty list with n > 1 interleaves value between the elements with intervals of size n" <|
            \_ ->
                let
                    value =
                        -1

                    input =
                        [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 ]

                    output =
                        [ 0, 1, 2, -1, 3, 4, 5, -1, 6, 7, 8, -1, 9 ]
                in
                Expect.equal output <| interleave 3 value input
        ]
