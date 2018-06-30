module Sort.Dict
    exposing
        ( Dict
        , empty
        , filter
        , foldl
        , foldr
        , fromList
        , get
        , isEmpty
        , keys
        , map
        , member
        , merge
        , partition
        , remove
        , removeAll
        , singleton
        , size
        , store
        , storeAll
        , toList
        , update
        , values
        )

{-| A dictionary mapping unique keys to values.

Insert, remove, and query operations all take _O(log n)_ time.

This implementation is based on
[`Skinney/elm-dict-exploration`](http://package.elm-lang.org/packages/Skinney/elm-dict-exploration),
except that it uses `Sorter` instead of `comparable`, so it permits keys
that are not `comparable`.


# Dictionaries

@docs Dict


# Build

@docs empty, singleton, store, remove, update


# Query

@docs isEmpty, size, get, member


# Transform

@docs map, filter, foldl, foldr, partition


# Combine

@docs storeAll, removeAll, merge


# Lists

@docs keys, values, toList, fromList

-}

import Internal.Dict exposing (Color(..), Dict(..), fromSortedList, getRange, getSorter, intersectAccumulator, unionAccumulator)
import List exposing (..)
import Maybe exposing (..)
import Sort exposing (Sorter)



{-
   The following is an implementation of Left-Leaning Red Black Trees (LLRB Tree).
   More information about this implementation can be found at the following links:

   http://www.cs.princeton.edu/~rs/talks/LLRB/LLRB.pdf
   http://www.cs.princeton.edu/~rs/talks/LLRB/RedBlack.pdf

   The short of it is, that in addition to the regular rules for RB trees, the following rule
   applies: No right references can be red.
-}


{-| A dictionary of keys and values. So a `(Dict String User)` is a dictionary
that lets you look up a `String` (such as user names) and find the associated
`User`.
-}
type alias Dict key value =
    Internal.Dict.Dict key value


{-| Create an empty dictionary.
-}
empty : Sorter k -> Dict k v
empty sorter =
    Leaf sorter


{-| Determine if a dictionary is empty.
isEmpty empty == True
-}
isEmpty : Dict k v -> Bool
isEmpty dict =
    case dict of
        Leaf _ ->
            True

        Node _ _ _ _ _ _ ->
            False


{-| Create a dictionary with one key-value pair.
-}
singleton : Sorter k -> k -> v -> Dict k v
singleton sorter key value =
    -- Root is always black
    Node sorter Black key value (Leaf sorter) (Leaf sorter)


{-| Determine the number of key-value pairs in the dictionary.
-}
size : Dict k v -> Int
size dict =
    sizeHelp 0 dict


sizeHelp : Int -> Dict k v -> Int
sizeHelp n dict =
    case dict of
        Leaf _ ->
            n

        Node _ _ _ _ left right ->
            sizeHelp (sizeHelp (n + 1) right) left


{-| Get the value associated with a key. If the key is not found, return
`Nothing`. This is useful when you are not sure if a key will be in the
dictionary.

    animals = fromList [ ("Tom", Cat), ("Jerry", Mouse) ]
    get "Tom" animals == Just Cat
    get "Jerry" animals == Just Mouse
    get "Spike" animals == Nothing

-}
get : k -> Dict k v -> Maybe v
get targetKey dict =
    case dict of
        Leaf _ ->
            Nothing

        Node sorter _ key value left right ->
            case Sort.toOrder sorter targetKey key of
                LT ->
                    get targetKey left

                GT ->
                    get targetKey right

                EQ ->
                    Just value


{-| Determine if a key is in a dictionary.
-}
member : k -> Dict k v -> Bool
member key dict =
    case get key dict of
        Just _ ->
            True

        Nothing ->
            False


{-| Store a key-value pair in a dictionary. Replaces value when that key is
already present.
-}
store : k -> v -> Dict k v -> Dict k v
store key value dict =
    case storeHelp key value dict of
        Node sorter Red k v l r ->
            Node sorter Black k v l r

        x ->
            x


storeHelp : k -> v -> Dict k v -> Dict k v
storeHelp key value dict =
    case dict of
        Leaf sorter ->
            -- New nodes are always red. If it violates the rules, it will be fixed
            -- when balancing.
            Node sorter Red key value (Leaf sorter) (Leaf sorter)

        Node sorter nColor nKey nValue nLeft nRight ->
            case Sort.toOrder sorter key nKey of
                LT ->
                    balance sorter nColor nKey nValue (storeHelp key value nLeft) nRight

                GT ->
                    balance sorter nColor nKey nValue nLeft (storeHelp key value nRight)

                EQ ->
                    Node sorter nColor nKey value nLeft nRight


balance : Sorter k -> Color -> k -> v -> Dict k v -> Dict k v -> Dict k v
balance sorter color key value left right =
    case right of
        Node _ Red rK rV rLeft rRight ->
            case left of
                Node _ Red lK lV lLeft lRight ->
                    Node
                        sorter
                        Red
                        key
                        value
                        (Node sorter Black lK lV lLeft lRight)
                        (Node sorter Black rK rV rLeft rRight)

                _ ->
                    Node sorter color rK rV (Node sorter Red key value left rLeft) rRight

        _ ->
            case left of
                Node _ Red lK lV (Node _ Red llK llV llLeft llRight) lRight ->
                    Node
                        sorter
                        Red
                        lK
                        lV
                        (Node sorter Black llK llV llLeft llRight)
                        (Node sorter Black key value lRight right)

                _ ->
                    Node sorter color key value left right


{-| Remove a key-value pair from a dictionary. If the key is not found,
no changes are made.
-}
remove : k -> Dict k v -> Dict k v
remove targetKey dict =
    case removeHelp targetKey dict of
        Node sorter Red k v l r ->
            Node sorter Black k v l r

        x ->
            x


{-| The easiest thing to remove from the tree, is a red node. However, when searching for the
node to remove, we have no way of knowing if it will be red or not. This remove implementation
makes sure that the bottom node is red by moving red colors down the tree through rotation
and color flips. Any violations this will cause, can easily be fixed by balancing on the way
up again.
-}
removeHelp : k -> Dict k v -> Dict k v
removeHelp targetKey dict =
    case dict of
        (Leaf _) as leaf ->
            leaf

        Node sorter color key value left right ->
            case Sort.toOrder sorter targetKey key of
                LT ->
                    case left of
                        Node _ Black _ _ lLeft _ ->
                            case lLeft of
                                Node _ Red _ _ _ _ ->
                                    Node sorter color key value (removeHelp targetKey left) right

                                _ ->
                                    case moveRedLeft dict of
                                        Node _ movedColor movedKey movedValue movedLeft movedRight ->
                                            balance sorter movedColor movedKey movedValue (removeHelp targetKey movedLeft) movedRight

                                        (Leaf _) as leaf ->
                                            leaf

                        _ ->
                            Node sorter color key value (removeHelp targetKey left) right

                _ ->
                    removeHelpEQGT targetKey (removeHelpPrepEQGT targetKey dict color key value left right)


removeHelpPrepEQGT : k -> Dict k v -> Color -> k -> v -> Dict k v -> Dict k v -> Dict k v
removeHelpPrepEQGT targetKey dict color key value left right =
    case left of
        Node sorter Red lK lV lLeft lRight ->
            Node
                sorter
                color
                lK
                lV
                lLeft
                (Node sorter Red key value lRight right)

        _ ->
            case right of
                Node _ Black _ _ (Node _ Black _ _ _ _) _ ->
                    moveRedRight dict

                Node _ Black _ _ (Leaf _) _ ->
                    moveRedRight dict

                _ ->
                    dict


{-| When we find the node we are looking for, we can remove by replacing the key-value
pair with the key-value pair of the left-most node on the right side (the closest pair).
-}
removeHelpEQGT : k -> Dict k v -> Dict k v
removeHelpEQGT targetKey dict =
    case dict of
        Node sorter color key value left right ->
            if targetKey == key then
                case getMin right of
                    Node _ _ minKey minValue _ _ ->
                        balance sorter color minKey minValue left (removeMin right)

                    (Leaf _) as leaf ->
                        leaf

            else
                balance sorter color key value left (removeHelp targetKey right)

        (Leaf _) as leaf ->
            leaf


getMin : Dict k v -> Dict k v
getMin dict =
    case dict of
        Node _ _ _ _ ((Node _ _ _ _ _ _) as left) _ ->
            getMin left

        _ ->
            dict


removeMin : Dict k v -> Dict k v
removeMin dict =
    case dict of
        Node sorter color key value ((Node _ lColor _ _ lLeft _) as left) right ->
            case lColor of
                Black ->
                    case lLeft of
                        Node _ Red _ _ _ _ ->
                            Node sorter color key value (removeMin left) right

                        _ ->
                            case moveRedLeft dict of
                                Node _ movedColor movedKey movedValue movedLeft movedRight ->
                                    balance sorter movedColor movedKey movedValue (removeMin movedLeft) movedRight

                                (Leaf _) as leaf ->
                                    leaf

                _ ->
                    Node sorter color key value (removeMin left) right

        Node _ _ _ _ ((Leaf _) as leaf) _ ->
            leaf

        (Leaf _) as leaf ->
            leaf


moveRedLeft : Dict k v -> Dict k v
moveRedLeft dict =
    case dict of
        Node sorter clr k v (Node _ lClr lK lV lLeft lRight) (Node _ rClr rK rV ((Node _ Red rlK rlV rlL rlR) as rLeft) rRight) ->
            Node
                sorter
                Red
                rlK
                rlV
                (Node sorter Black k v (Node sorter Red lK lV lLeft lRight) rlL)
                (Node sorter Black rK rV rlR rRight)

        Node sorter clr k v (Node _ lClr lK lV lLeft lRight) (Node _ rClr rK rV rLeft rRight) ->
            case clr of
                Black ->
                    Node
                        sorter
                        Black
                        k
                        v
                        (Node sorter Red lK lV lLeft lRight)
                        (Node sorter Red rK rV rLeft rRight)

                Red ->
                    Node
                        sorter
                        Black
                        k
                        v
                        (Node sorter Red lK lV lLeft lRight)
                        (Node sorter Red rK rV rLeft rRight)

        _ ->
            dict


moveRedRight : Dict k v -> Dict k v
moveRedRight dict =
    case dict of
        Node sorter clr k v (Node _ lClr lK lV (Node _ Red llK llV llLeft llRight) lRight) (Node _ rClr rK rV rLeft rRight) ->
            Node
                sorter
                Red
                lK
                lV
                (Node sorter Black llK llV llLeft llRight)
                (Node sorter Black k v lRight (Node sorter Red rK rV rLeft rRight))

        Node sorter clr k v (Node _ lClr lK lV lLeft lRight) (Node _ rClr rK rV rLeft rRight) ->
            case clr of
                Black ->
                    Node
                        sorter
                        Black
                        k
                        v
                        (Node sorter Red lK lV lLeft lRight)
                        (Node sorter Red rK rV rLeft rRight)

                Red ->
                    Node
                        sorter
                        Black
                        k
                        v
                        (Node sorter Red lK lV lLeft lRight)
                        (Node sorter Red rK rV rLeft rRight)

        _ ->
            dict


{-| Update the value of a dictionary for a specific key with a given function.
The given function gets the current value as a parameter and its return value
determines if the value is updated or removed. New key-value pairs can be
stored too.
-}
update : k -> (Maybe v -> Maybe v) -> Dict k v -> Dict k v
update key alter dict =
    case alter (get key dict) of
        Nothing ->
            remove key dict

        Just value ->
            store key value dict



-- TRANSFORM


{-| Apply a function to all values in a dictionary.
-}
map : (k -> a -> b) -> Dict k a -> Dict k b
map f dict =
    case dict of
        Leaf sorter ->
            Leaf sorter

        Node sorter color key value left right ->
            Node sorter color key (f key value) (map f left) (map f right)


{-| Keep a key-value pair when it satisfies a predicate.
-}
filter : (k -> v -> Bool) -> Dict k v -> Dict k v
filter predicate dict =
    let
        helper key value list =
            if predicate key value then
                ( key, value ) :: list

            else
                list
    in
    fromSortedList (getSorter dict) True (foldr helper [] dict)


{-| Fold over the key-value pairs in a dictionary, in order from lowest
key to highest key.
-}
foldl : (k -> v -> b -> b) -> b -> Dict k v -> b
foldl f acc dict =
    case dict of
        Leaf _ ->
            acc

        Node _ _ key value left right ->
            foldl f (f key value (foldl f acc left)) right


{-| Fold over the key-value pairs in a dictionary, in order from highest
key to lowest key.
-}
foldr : (k -> v -> b -> b) -> b -> Dict k v -> b
foldr f acc dict =
    Internal.Dict.foldr f acc dict


{-| Partition a dictionary according to a predicate. The first dictionary
contains all key-value pairs which satisfy the predicate, and the second
contains the rest.
-}
partition : (k -> v -> Bool) -> Dict k v -> ( Dict k v, Dict k v )
partition predicate dict =
    let
        helper key value ( trues, falses ) =
            if predicate key value then
                ( ( key, value ) :: trues, falses )

            else
                ( trues, ( key, value ) :: falses )

        ( finalTrues, finalFalses ) =
            foldr helper ( [], [] ) dict

        sorter =
            getSorter dict
    in
    ( fromSortedList sorter True finalTrues, fromSortedList sorter True finalFalses )



-- COMBINE


{-| Take all the key-value pairs in the `from` dictionary and
`store` them into the `into` dictionary.
-}
storeAll : { from : Dict k v, into : Dict k v } -> Dict k v
storeAll { from, into } =
    case ( from, into ) of
        ( _, Leaf _ ) ->
            from

        ( Leaf _, _ ) ->
            into

        ( Node sorter _ _ _ _ _, _ ) ->
            let
                ( lt, gt ) =
                    foldl (unionAccumulator sorter) ( [], toList into ) from
            in
            fromSortedList sorter False (List.foldl (\e acc -> e :: acc) lt gt)


{-| Remove all keys in the `remove` dictionary from the `from` dictionary.
-}
removeAll : { remove : Dict k v, from : Dict k v } -> Dict k v
removeAll record =
    foldl (\key _ acc -> remove key acc) record.remove record.from


{-| The most general way of combining two dictionaries. You provide three
accumulators for when a given key appears:

1.  Only in the left dictionary.
2.  In both dictionaries.
3.  Only in the right dictionary.
    You then traverse all the keys from lowest to highest, building up whatever
    you want.

-}
merge :
    Sorter k
    -> (k -> a -> result -> result)
    -> (k -> a -> b -> result -> result)
    -> (k -> b -> result -> result)
    -> Dict k a
    -> Dict k b
    -> result
    -> result
merge sorter leftStep bothStep rightStep leftDict rightDict initialResult =
    let
        stepState rKey rValue ( list, result ) =
            case list of
                [] ->
                    ( list, rightStep rKey rValue result )

                ( lKey, lValue ) :: rest ->
                    case Sort.toOrder sorter lKey rKey of
                        LT ->
                            stepState rKey rValue ( rest, leftStep lKey lValue result )

                        GT ->
                            ( list, rightStep rKey rValue result )

                        EQ ->
                            ( rest, bothStep lKey lValue rValue result )

        ( leftovers, intermediateResult ) =
            foldl stepState ( toList leftDict, initialResult ) rightDict
    in
    List.foldl (\( k, v ) result -> leftStep k v result) intermediateResult leftovers



-- LISTS


{-| Get all of the keys in a dictionary, sorted from lowest to highest.
keys (fromList [(0,"Alice"),(1,"Bob")]) == [0,1]
-}
keys : Dict k v -> List k
keys dict =
    Internal.Dict.keys dict


{-| Get all of the values in a dictionary, in the order of their keys.
values (fromList [(0,"Alice"),(1,"Bob")]) == ["Alice", "Bob"]
-}
values : Dict k v -> List v
values dict =
    foldr (\_ value valueList -> value :: valueList) [] dict


{-| Convert a dictionary into an association list of key-value pairs, sorted by keys.
-}
toList : Dict k v -> List ( k, v )
toList dict =
    foldr (\key value list -> ( key, value ) :: list) [] dict


{-| Convert an association list into a dictionary.
-}
fromList : Sorter k -> List ( k, v ) -> Dict k v
fromList sorter list =
    case list of
        pair :: rest ->
            let
                ( sorted, remainder ) =
                    splitSortedHelp sorter [] pair rest
            in
            List.foldl
                (\( k, v ) dict -> store k v dict)
                (fromSortedList sorter False sorted)
                remainder

        [] ->
            Leaf sorter


{-| Split a list into its sorted prefix and the remainder. The sorted prefix
is returned in reversed order.
-}
splitSortedHelp : Sorter k -> List ( k, v ) -> ( k, v ) -> List ( k, v ) -> ( List ( k, v ), List ( k, v ) )
splitSortedHelp sorter sorted (( k1, _ ) as p1) list =
    case list of
        (( k2, _ ) as p2) :: rest ->
            case Sort.toOrder sorter k1 k2 of
                LT ->
                    splitSortedHelp sorter (p1 :: sorted) p2 rest

                _ ->
                    ( sorted, p1 :: list )

        [] ->
            ( p1 :: sorted, [] )
