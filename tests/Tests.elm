module Tests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import GroupList exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "The GroupList"
        [ describe "GroupList.group"
            [ describe "n == 0"
                [ test "n == 0 on non empty list returns l inside a list" <|
                    \_ ->
                        let
                            l =
                                [ 1, 2, 3, 4, 5 ]
                        in
                        Expect.equal [ l ] <| group 0 l
                , test "n == 0 on empty list has no effect" <|
                    \_ -> Expect.equal [] <| group 0 []
                ]
            , describe "n < 0"
                [ test "n < 0 on non empty list returns l inside a list" <|
                    \_ ->
                        let
                            l =
                                [ 1, 2, 3, 4, 5 ]
                        in
                        Expect.equal [ l ] <| group -1 l
                , test "n < 0 on empty list has no effect" <|
                    \_ -> Expect.equal [] <| group -1 []
                ]
            , describe "n > 0"
                [ test "n > 0 on non empty list returns a list of lists sized n" <|
                    \_ ->
                        let
                            input =
                                [ 1, 2, 3, 4, 5 ]

                            output =
                                [ [ 1, 2, 3 ], [ 4, 5 ] ]
                        in
                        Expect.equal output <| group 3 input
                , test "n > 0 on non empty list returns a list of lists sized n" <|
                    \_ ->
                        let
                            input =
                                [ 'A', 'B', 'C', 'D', 'E' ]

                            output =
                                [ [ 'A', 'B' ], [ 'C', 'D' ], [ 'E' ] ]
                        in
                        Expect.equal output <| group 3 input
                , test "n > 0 on empty list has no effect" <|
                    \_ -> Expect.equal [] <| group 1 []
                ]
            ]
        ]
