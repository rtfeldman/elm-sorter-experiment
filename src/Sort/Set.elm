module Sort.Set
    exposing
        ( Set
        , add
        , addAll
        , empty
        , filter
        , foldl
        , foldr
        , fromList
        , isEmpty
        , map
        , member
        , partition
        , remove
        , removeAll
        , singleton
        , size
        , toList
        )

{-| A set of unique values.

Insert, remove, and query operations all take _O(log n)_ time.

This implementation is based on
[`Skinney/elm-dict-exploration`](http://package.elm-lang.org/packages/Skinney/elm-dict-exploration),
except that it uses `Sorter` instead of `comparable`, so it permits keys
that are not `comparable`.


# Sets

@docs Set


# Build

@docs empty, singleton, add, remove


# Query

@docs isEmpty, member, size


# Combine

@docs addAll, removeAll


# Lists

@docs toList, fromList


# Transform

@docs map, foldl, foldr, filter, partition

-}

import Internal.Dict exposing (Color(..), Dict(..), fromSortedList, getRange, intersectAccumulator, unionAccumulator)
import List exposing ((::))
import Sort exposing (Sorter)
import Sort.Dict as Dict


{-| Represents a set of unique values. So `(Set Int)` is a set of integers and
`(Set String)` is a set of strings.
-}
type Set t
    = Set_elm_builtin (Dict t ())


{-| Create an empty set.
-}
empty : Sorter a -> Set a
empty sorter =
    Set_elm_builtin (Dict.empty sorter)


{-| Create a set with one value.
-}
singleton : Sorter a -> a -> Set a
singleton sorter key =
    Set_elm_builtin (Dict.singleton sorter key ())


{-| Add a value to a set.
-}
add : a -> Set a -> Set a
add key (Set_elm_builtin dict) =
    Set_elm_builtin (Dict.store key () dict)


{-| Remove a value from a set. If the value is not found, no changes are made.
-}
remove : a -> Set a -> Set a
remove key (Set_elm_builtin dict) =
    Set_elm_builtin (Dict.remove key dict)


{-| Determine if a set is empty.
-}
isEmpty : Set a -> Bool
isEmpty (Set_elm_builtin dict) =
    Dict.isEmpty dict


{-| Determine if a value is in a set.
-}
member : a -> Set a -> Bool
member key (Set_elm_builtin dict) =
    Dict.member key dict


{-| Determine the number of elements in a set.
-}
size : Set a -> Int
size (Set_elm_builtin dict) =
    Dict.size dict


{-| Get the union of two sets. Keep all values.
-}
addAll : { from : Set a, to : Set a } -> Set a
addAll { from, to } =
    case ( from, to ) of
        ( set, Set_elm_builtin (Leaf _) ) ->
            set

        ( Set_elm_builtin (Leaf _), set ) ->
            set

        ( Set_elm_builtin ((Node _ _ _ _ _ _) as fromDict), Set_elm_builtin ((Node sorter _ _ _ _ _) as toDict) ) ->
            let
                ( lt, gt ) =
                    Dict.foldl (unionAccumulator sorter) ( [], Dict.toList fromDict ) toDict
            in
            fromSortedList sorter False (List.foldl (\e acc -> e :: acc) lt gt)
                |> Set_elm_builtin


{-| Remove all values in the `remove` set from the `from` set.
-}
removeAll : { remove : Set a, from : Set a } -> Set a
removeAll record =
    let
        (Set_elm_builtin fromDict) =
            record.from

        (Set_elm_builtin removeDict) =
            record.remove
    in
    Set_elm_builtin (Dict.removeAll { from = fromDict, remove = removeDict })


{-| Convert a set into a list, sorted from lowest to highest.
-}
toList : Set a -> List a
toList (Set_elm_builtin dict) =
    Dict.keys dict


{-| Convert a list into a set, removing any duplicates.
-}
fromList : Sorter a -> List a -> Set a
fromList sorter list =
    List.foldl add (empty sorter) list


{-| Fold over the values in a set, in order from lowest to highest.
-}
foldl : (a -> b -> b) -> b -> Set a -> b
foldl func initialState (Set_elm_builtin dict) =
    Dict.foldl (\key _ state -> func key state) initialState dict


{-| Fold over the values in a set, in order from highest to lowest.
-}
foldr : (a -> b -> b) -> b -> Set a -> b
foldr func initialState (Set_elm_builtin dict) =
    Dict.foldr (\key _ state -> func key state) initialState dict


{-| Map a function onto a set, creating a new set with no duplicates.
-}
map : Sorter b -> (a -> b) -> Set a -> Set b
map sorter func set =
    fromList sorter (foldl (\x xs -> func x :: xs) [] set)


{-| Only keep elements that pass the given test.

    import Set exposing (Set)

    numbers : Set Int
    numbers =
        Set.fromList [ -2, -1, 0, 1, 2 ]

    positives : Set Int
    positives =
        Set.filter (\x -> x > 0) numbers

    evens : Set Int
    evens =
        Set.filter (\x -> x % 2 == 0) numbers

    positiveEvens =
        -- Intersection
        Set.filter (Set.member positives) evens


    -- positives == Set.fromList [1,2]

-}
filter : (a -> Bool) -> Set a -> Set a
filter isGood (Set_elm_builtin dict) =
    Set_elm_builtin (Dict.filter (\key _ -> isGood key) dict)


{-| Create two new sets. The first contains all the elements that passed the
given test, and the second contains all the elements that did not.
-}
partition : (a -> Bool) -> Set a -> ( Set a, Set a )
partition isGood (Set_elm_builtin dict) =
    let
        ( dict1, dict2 ) =
            Dict.partition (\key _ -> isGood key) dict
    in
    ( Set_elm_builtin dict1, Set_elm_builtin dict2 )
