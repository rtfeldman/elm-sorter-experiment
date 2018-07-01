module Sort.Set
    exposing
        ( Set
        , dropIf
        , empty
        , foldl
        , foldr
        , fromList
        , insert
        , insertAll
        , isEmpty
        , keepIf
        , map
        , member
        , partition
        , remove
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

@docs empty, singleton, insert, insertAll, remove


# Query

@docs isEmpty, member, size


# Lists

@docs toList, fromList


# Transform

@docs map, keepIf, dropIf, foldl, foldr, partition

-}

import Internal.Dict exposing (Color(..), Dict(..), fromSortedList, getRange, unionAccumulator)
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


{-| Insert a value into a set.
-}
insert : a -> Set a -> Set a
insert key (Set_elm_builtin dict) =
    Set_elm_builtin (Dict.insert key () dict)


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


{-| Returns `True` if the given value is in the given set.
-}
member : Set a -> a -> Bool
member (Set_elm_builtin dict) key =
    Dict.member dict key


{-| Determine the number of elements in a set.
-}
size : Set a -> Int
size (Set_elm_builtin dict) =
    Dict.size dict


{-| Take all the elements in the first set and [`insert`](#insert) them into the second set.

This returns the **union** of the sets.

-}
insertAll : Set a -> Set a -> Set a
insertAll newElems original =
    case ( newElems, original ) of
        ( set, Set_elm_builtin (Leaf _) ) ->
            set

        ( Set_elm_builtin (Leaf _), set ) ->
            set

        ( Set_elm_builtin ((Node _ _ _ _ _ _) as newElems), Set_elm_builtin ((Node sorter _ _ _ _ _) as original) ) ->
            let
                ( lt, gt ) =
                    Dict.foldl (unionAccumulator sorter) ( [], Dict.toList newElems ) original
            in
            fromSortedList sorter False (List.foldl (\e acc -> e :: acc) lt gt)
                |> Set_elm_builtin


{-| Convert a set into a list, sorted from lowest to highest.
-}
toList : Set a -> List a
toList (Set_elm_builtin dict) =
    Dict.keys dict


{-| Convert a list into a set, removing any duplicates.
-}
fromList : Sorter a -> List a -> Set a
fromList sorter list =
    List.foldl insert (empty sorter) list


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

    numbers =
        Set.fromList [ -2, -1, 0, 1, 2, 3, 4, 5 ]

    positives =
        Set.keepIf (\num -> num > 0) numbers

    evens =
        Set.keepIf (\num -> num % 2 == 0) numbers

    positiveEvens =
        -- Intersection
        Set.keepIf (Set.member evens) positives

-}
keepIf : (a -> Bool) -> Set a -> Set a
keepIf shouldKeep (Set_elm_builtin dict) =
    Set_elm_builtin (Dict.keepIf (\key _ -> shouldKeep key) dict)


{-| Remove elements that pass the given test.

    numbers =
        Set.fromList [ -2, -1, 0, 1, 2, 3, 4, 5 ]

    positives =
        Set.dropIf (\num -> num <= 0) numbers

    evens =
        Set.dropIf (\num -> num % 2 == 1) numbers

    positiveOdds =
        -- Difference
        Set.dropIf (Set.member evens) positives

-}
dropIf : (a -> Bool) -> Set a -> Set a
dropIf shouldDrop (Set_elm_builtin dict) =
    Set_elm_builtin (Dict.dropIf (\key _ -> shouldDrop key) dict)


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
