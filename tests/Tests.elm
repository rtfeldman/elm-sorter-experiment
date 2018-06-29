module Tests exposing (tests)

import Basics exposing (..)
import Dict as BaseDict
import Expect
import Fuzz exposing (Fuzzer)
import Internal.Dict exposing (validateInvariants)
import List
import Maybe exposing (..)
import Sort
import Sort.Dict as Dict exposing (Dict)
import Test exposing (..)


animals : Dict.Dict String String
animals =
    Dict.fromList Sort.alphabetical [ ( "Tom", "cat" ), ( "Jerry", "mouse" ) ]


pairRange : Fuzzer Int
pairRange =
    Fuzz.intRange 0 1000


fuzzPairs : Fuzzer (List ( Int, Int ))
fuzzPairs =
    ( pairRange, pairRange )
        |> Fuzz.tuple
        |> Fuzz.list


fuzzDict : Fuzzer (Dict Int Int)
fuzzDict =
    Fuzz.map (Dict.fromList Sort.increasing) fuzzPairs


tests : Test
tests =
    let
        buildTests =
            describe "build Tests"
                [ test "empty numbers" <| \() -> Expect.equal (Dict.fromList Sort.increasing []) (Dict.empty Sort.increasing)
                , test "empty strings" <| \() -> Expect.equal (Dict.fromList Sort.alphabetical []) (Dict.empty Sort.alphabetical)
                , test "singleton" <| \() -> Expect.equal (Dict.fromList Sort.alphabetical [ ( "k", "v" ) ]) (Dict.singleton Sort.alphabetical "k" "v")
                , test "insert" <| \() -> Expect.equal (Dict.fromList Sort.alphabetical [ ( "k", "v" ) ]) (Dict.insert "k" "v" (Dict.empty Sort.alphabetical))
                , test "insert replace" <| \() -> Expect.equal (Dict.fromList Sort.alphabetical [ ( "k", "vv" ) ]) (Dict.insert "k" "vv" (Dict.singleton Sort.alphabetical "k" "v"))
                , test "update" <| \() -> Expect.equal (Dict.fromList Sort.alphabetical [ ( "k", "vv" ) ]) (Dict.update "k" (\v -> Just "vv") (Dict.singleton Sort.alphabetical "k" "v"))
                , test "update Nothing" <| \() -> Expect.equal (Dict.empty Sort.alphabetical) (Dict.update "k" (\v -> Nothing) (Dict.singleton Sort.alphabetical "k" "v"))
                , test "remove" <| \() -> Expect.equal (Dict.empty Sort.alphabetical) (Dict.remove "k" (Dict.singleton Sort.alphabetical "k" "v"))
                , test "remove not found" <| \() -> Expect.equal (Dict.singleton Sort.alphabetical "k" "v") (Dict.remove "kk" (Dict.singleton Sort.alphabetical "k" "v"))
                , test "fromList excludes duplicates" <| \() -> Expect.equal (Dict.singleton Sort.increasing 1 1) (Dict.fromList Sort.increasing [ ( 1, 1 ), ( 1, 1 ) ])
                , describe "fromList builds a valid Dict"
                    (List.range 1 100
                        |> List.map
                            (\n ->
                                test ("of size " ++ toString n) <|
                                    \() ->
                                        let
                                            list =
                                                List.range 1 n |> List.indexedMap (,)

                                            dict =
                                                Dict.fromList Sort.increasing list
                                        in
                                        Expect.equal ( "", list ) ( validateInvariants dict, Dict.toList dict )
                            )
                    )
                ]

        queryTests =
            describe "query Tests"
                [ test "member 1" <| \() -> Expect.equal True (Dict.member "Tom" animals)
                , test "member 2" <| \() -> Expect.equal False (Dict.member "Spike" animals)
                , test "get 1" <| \() -> Expect.equal (Just "cat") (Dict.get "Tom" animals)
                , test "get 2" <| \() -> Expect.equal Nothing (Dict.get "Spike" animals)
                , test "size of empty dictionary" <| \() -> Expect.equal 0 (Dict.size (Dict.empty Sort.increasing))
                , test "size of example dictionary" <| \() -> Expect.equal 2 (Dict.size animals)
                ]

        combineTests =
            describe "combine Tests"
                [ test "insertAll" <| \() -> Expect.equal animals (Dict.insertAll { from = Dict.singleton Sort.alphabetical "Jerry" "mouse", into = Dict.singleton Sort.alphabetical "Tom" "cat" })
                , test "insertAll collison" <| \() -> Expect.equal (Dict.singleton Sort.alphabetical "Tom" "cat") (Dict.insertAll { from = Dict.singleton Sort.alphabetical "Tom" "cat", into = Dict.singleton Sort.alphabetical "Tom" "mouse" })
                , test "intersect" <| \() -> Expect.equal (Dict.singleton Sort.alphabetical "Tom" "cat") (Dict.intersect { preferred = animals, other = Dict.singleton Sort.alphabetical "Tom" "cat" })
                , test "intersect collision" <| \() -> Expect.equal (Dict.singleton Sort.alphabetical "Tom" "wolf") (Dict.intersect { preferred = Dict.singleton Sort.alphabetical "Tom" "wolf", other = animals })
                , test "diff" <| \() -> Expect.equal (Dict.singleton Sort.alphabetical "Jerry" "mouse") (Dict.diff { original = animals, other = Dict.singleton Sort.alphabetical "Tom" "cat" })
                ]

        transformTests =
            describe "transform Tests"
                [ test "filter" <| \() -> Expect.equal (Dict.singleton Sort.alphabetical "Tom" "cat") (Dict.filter (\k v -> k == "Tom") animals)
                , test "filter (numbers)" <| \() -> Expect.equal [ 2, 4, 6, 8, 10 ] (List.range 1 10 |> List.indexedMap (,) |> Dict.fromList Sort.increasing |> Dict.filter (\_ v -> v % 2 == 0) |> Dict.values)
                , test "partition" <| \() -> Expect.equal ( Dict.singleton Sort.alphabetical "Tom" "cat", Dict.singleton Sort.alphabetical "Jerry" "mouse" ) (Dict.partition (\k v -> k == "Tom") animals)
                , test "partition (numbers)" <| \() -> Expect.equal ( [ 2, 4, 6, 8, 10 ], [ 1, 3, 5, 7, 9 ] ) (List.range 1 10 |> List.indexedMap (,) |> Dict.fromList Sort.increasing |> Dict.partition (\_ v -> v % 2 == 0) |> (\( a, b ) -> ( Dict.values a, Dict.values b )))
                ]

        mergeTests =
            let
                insertBoth key leftVal rightVal dict =
                    Dict.insert key (leftVal ++ rightVal) dict

                s1 =
                    Dict.empty Sort.alphabetical |> Dict.insert "u1" [ 1 ]

                s2 =
                    Dict.empty Sort.alphabetical |> Dict.insert "u2" [ 2 ]

                s23 =
                    Dict.empty Sort.alphabetical |> Dict.insert "u2" [ 3 ]

                b1 =
                    List.map (\i -> ( i, [ i ] )) (List.range 1 10) |> Dict.fromList Sort.increasing

                b2 =
                    List.map (\i -> ( i, [ i ] )) (List.range 5 15) |> Dict.fromList Sort.increasing

                bExpected =
                    [ ( 1, [ 1 ] ), ( 2, [ 2 ] ), ( 3, [ 3 ] ), ( 4, [ 4 ] ), ( 5, [ 5, 5 ] ), ( 6, [ 6, 6 ] ), ( 7, [ 7, 7 ] ), ( 8, [ 8, 8 ] ), ( 9, [ 9, 9 ] ), ( 10, [ 10, 10 ] ), ( 11, [ 11 ] ), ( 12, [ 12 ] ), ( 13, [ 13 ] ), ( 14, [ 14 ] ), ( 15, [ 15 ] ) ]
            in
            describe "merge Tests"
                [ test "merge empties" <|
                    \() ->
                        Expect.equal (Dict.empty Sort.alphabetical)
                            (Dict.merge Sort.alphabetical Dict.insert insertBoth Dict.insert (Dict.empty Sort.alphabetical) (Dict.empty Sort.alphabetical) (Dict.empty Sort.alphabetical))
                , test "merge singletons in order" <|
                    \() ->
                        Expect.equal [ ( "u1", [ 1 ] ), ( "u2", [ 2 ] ) ]
                            (Dict.merge Sort.alphabetical Dict.insert insertBoth Dict.insert s1 s2 (Dict.empty Sort.alphabetical) |> Dict.toList)
                , test "merge singletons out of order" <|
                    \() ->
                        Expect.equal [ ( "u1", [ 1 ] ), ( "u2", [ 2 ] ) ]
                            (Dict.merge Sort.alphabetical Dict.insert insertBoth Dict.insert s2 s1 (Dict.empty Sort.alphabetical) |> Dict.toList)
                , test "merge with duplicate key" <|
                    \() ->
                        Expect.equal [ ( "u2", [ 2, 3 ] ) ]
                            (Dict.merge Sort.alphabetical Dict.insert insertBoth Dict.insert s2 s23 (Dict.empty Sort.alphabetical) |> Dict.toList)
                , test "partially overlapping" <|
                    \() ->
                        Expect.equal bExpected
                            (Dict.merge Sort.increasing Dict.insert insertBoth Dict.insert b1 b2 (Dict.empty Sort.increasing) |> Dict.toList)
                ]

        fuzzTests =
            describe "Fuzz tests"
                [ fuzz2 fuzzPairs pairRange "Get works" <|
                    \pairs num ->
                        Dict.get num (Dict.fromList Sort.increasing pairs)
                            |> Expect.equal (BaseDict.get num (BaseDict.fromList pairs))
                , fuzz fuzzPairs "Converting to/from list works" <|
                    \pairs ->
                        Dict.toList (Dict.fromList Sort.increasing pairs)
                            |> Expect.equal (BaseDict.toList (BaseDict.fromList pairs))
                , fuzz2 fuzzPairs pairRange "Insert works" <|
                    \pairs num ->
                        Dict.toList (Dict.insert num num (Dict.fromList Sort.increasing pairs))
                            |> Expect.equal (BaseDict.toList (BaseDict.insert num num (BaseDict.fromList pairs)))
                , fuzz2 fuzzPairs pairRange "Insert maintains invariant" <|
                    \pairs num ->
                        validateInvariants (Dict.insert num num (Dict.fromList Sort.increasing pairs))
                            |> Expect.equal ""
                , fuzz2 fuzzPairs pairRange "Removal works" <|
                    \pairs num ->
                        Dict.toList (Dict.remove num (Dict.fromList Sort.increasing pairs))
                            |> Expect.equal (BaseDict.toList (BaseDict.remove num (BaseDict.fromList pairs)))
                , fuzz2 fuzzPairs pairRange "Remove maintains invariant" <|
                    \pairs num ->
                        validateInvariants (Dict.remove num (Dict.fromList Sort.increasing pairs))
                            |> Expect.equal ""
                , fuzz2 fuzzPairs fuzzPairs "insertAll maintains invariant" <|
                    \pairs pairs2 ->
                        validateInvariants (Dict.insertAll { from = Dict.fromList Sort.increasing pairs, into = Dict.fromList Sort.increasing pairs2 })
                            |> Expect.equal ""
                , fuzz2 fuzzPairs fuzzPairs "insertAll works" <|
                    \pairs pairs2 ->
                        Dict.toList (Dict.insertAll { from = Dict.fromList Sort.increasing pairs, into = Dict.fromList Sort.increasing pairs2 })
                            |> Expect.equal (BaseDict.toList (BaseDict.union (BaseDict.fromList pairs) (BaseDict.fromList pairs2)))
                , fuzz2 fuzzPairs fuzzPairs "Intersect maintains invariant" <|
                    \pairs pairs2 ->
                        validateInvariants (Dict.intersect { preferred = Dict.fromList Sort.increasing pairs, other = Dict.fromList Sort.increasing pairs2 })
                            |> Expect.equal ""
                , fuzz2 fuzzPairs fuzzPairs "Intersect works" <|
                    \pairs pairs2 ->
                        Dict.toList (Dict.intersect { preferred = Dict.fromList Sort.increasing pairs, other = Dict.fromList Sort.increasing pairs2 })
                            |> Expect.equal (BaseDict.toList (BaseDict.intersect (BaseDict.fromList pairs) (BaseDict.fromList pairs2)))
                , fuzz2 fuzzPairs fuzzPairs "Diff maintains invariant" <|
                    \pairs pairs2 ->
                        validateInvariants (Dict.diff { original = Dict.fromList Sort.increasing pairs, other = Dict.fromList Sort.increasing pairs2 })
                            |> Expect.equal ""
                , fuzz2 fuzzPairs fuzzPairs "Diff works" <|
                    \pairs pairs2 ->
                        Dict.toList (Dict.diff { original = Dict.fromList Sort.increasing pairs, other = Dict.fromList Sort.increasing pairs2 })
                            |> Expect.equal (BaseDict.toList (BaseDict.diff (BaseDict.fromList pairs) (BaseDict.fromList pairs2)))
                ]
    in
    describe "Dict Tests"
        [ buildTests
        , queryTests
        , combineTests
        , transformTests
        , mergeTests
        , fuzzTests
        ]
