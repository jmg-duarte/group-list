module TestGroup exposing (testGroup)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import GroupList exposing (..)
import Test exposing (..)


testGroup : Test
testGroup =
    describe "GroupList.group"
        [ testEmptyLists, testNonEmptyLists ]


testEmptyLists : Test
testEmptyLists =
    describe "GroupList.group for empty lists"
        [ test "n > 0 on empty list has no effect" <|
            \_ -> Expect.equal [] <| group 1 []
        , test "n == 0 on empty list has no effect" <|
            \_ -> Expect.equal [] <| group 0 []
        , test "n < 0 on empty list has no effect" <|
            \_ -> Expect.equal [] <| group -1 []
        ]


testNonEmptyLists : Test
testNonEmptyLists =
    describe "GroupList.group for non empty lists"
        [ describe "n == 0"
            [ test "non empty list returns l inside a list" <|
                \_ ->
                    let
                        input =
                            [ 1, 2, 3, 4, 5 ]

                        output =
                            [ input ]
                    in
                    Expect.equal output <| group 0 input
            ]
        , describe "n < 0"
            [ test "non empty list of integers returns l inside a list" <|
                \_ ->
                    let
                        input =
                            [ 1, 2, 3, 4, 5 ]

                        output =
                            [ input ]
                    in
                    Expect.equal output <| group -1 input
            , test "non empty list of characters returns l inside a list" <|
                \_ ->
                    let
                        input =
                            [ 'A', 'B', 'C', 'D', 'E' ]

                        output =
                            [ input ]
                    in
                    Expect.equal output <| group -1 input
            ]
        , describe "n > 0"
            [ test "non empty list of integers returns a list of lists sized n" <|
                \_ ->
                    let
                        input =
                            [ 1, 2, 3, 4, 5 ]

                        output =
                            [ [ 1, 2, 3 ], [ 4, 5 ] ]
                    in
                    Expect.equal output <| group 3 input
            , test "non empty list of characters returns a list of lists sized n" <|
                \_ ->
                    let
                        input =
                            [ 'A', 'B', 'C', 'D', 'E' ]

                        output =
                            [ [ 'A', 'B' ], [ 'C', 'D' ], [ 'E' ] ]
                    in
                    Expect.equal output <| group 2 input
            ]
        ]
