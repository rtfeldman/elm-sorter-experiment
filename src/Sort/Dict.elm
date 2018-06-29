module Dict.LLRB
    exposing
        ( Dict
        , diff
        , empty
        , filter
        , foldl
        , foldr
        , fromList
          -- , validateInvariants
        , get
        , insert
        , intersect
        , isEmpty
        , keys
        , map
        , member
        , merge
        , partition
        , remove
        , singleton
        , size
        , toList
        , union
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

@docs empty, singleton, insert, remove, update


# Query

@docs isEmpty, size, get, member


# Transform

@docs map, filter, foldl, foldr, partition


# Combine

@docs union, intersect, diff, merge


# Lists

@docs keys, values, toList, fromList

-}

import Basics exposing (..)
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
type Dict key value
    = Leaf (Sorter key)
    | Node (Sorter key) Color key value (Dict key value) (Dict key value)


{-| The color of a Node. Leafs are considered black.
-}
type Color
    = Black
    | Red


{-| Create an empty dictionary.
-}
empty : Dict k v
empty =
    Leaf


{-| Determine if a dictionary is empty.
isEmpty empty == True
-}
isEmpty : Dict k v -> Bool
isEmpty dict =
    dict == empty


{-| Create a dictionary with one key-value pair.
-}
singleton : Sorter k -> k -> v -> Dict k v
singleton sorter key value =
    -- Root is always black
    Node sorter Black key value Leaf Leaf


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


{-| Insert a key-value pair into a dictionary. Replaces value when there is
a collision.
-}
insert : k -> v -> Dict k v -> Dict k v
insert key value dict =
    case insertHelp key value dict of
        Node sorter Red k v l r ->
            Node sorter Black k v l r

        x ->
            x


insertHelp : k -> v -> Dict k v -> Dict k v
insertHelp key value dict =
    case dict of
        Leaf sorter ->
            -- New nodes are always red. If it violates the rules, it will be fixed
            -- when balancing.
            Node sorter Red key value Leaf Leaf

        Node sorter nColor nKey nValue nLeft nRight ->
            case Sort.toOrder sorter key nKey of
                LT ->
                    balance sorter nColor nKey nValue (insertHelp key value nLeft) nRight

                GT ->
                    balance sorter nColor nKey nValue nLeft (insertHelp key value nRight)

                EQ ->
                    Node nColor nKey value nLeft nRight


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
                        (Node Black lK lV lLeft lRight)
                        (Node Black rK rV rLeft rRight)

                _ ->
                    Node sorter color rK rV (Node Red key value left rLeft) rRight

        _ ->
            case left of
                Node _ Red lK lV (Node _ Red llK llV llLeft llRight) lRight ->
                    Node
                        sorter
                        Red
                        lK
                        lV
                        (Node Black llK llV llLeft llRight)
                        (Node Black key value lRight right)

                _ ->
                    Node color key value left right


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
            if targetKey < key then
                case left of
                    Node _ Black _ _ lLeft _ ->
                        case lLeft of
                            Node sorter Red _ _ _ _ ->
                                Node color key value (removeHelp targetKey left) right

                            _ ->
                                case moveRedLeft dict of
                                    Node sorter color key value left right ->
                                        balance sorter color key value (removeHelp targetKey left) right

                                    (Leaf _) as leaf ->
                                        leaf

                    _ ->
                        Node sorter color key value (removeHelp targetKey left) right

            else
                removeHelpEQGT sorter targetKey (removeHelpPrepEQGT targetKey dict color key value left right)


removeHelpPrepEQGT : Sorter k -> k -> Dict k v -> Color -> k -> v -> Dict k v -> Dict k v -> Dict k v
removeHelpPrepEQGT sorter targetKey dict color key value left right =
    case left of
        Node sorter Red lK lV lLeft lRight ->
            Node
                sorter
                color
                lK
                lV
                lLeft
                (Node Red key value lRight right)

        _ ->
            case right of
                Node sorter Black _ _ (Node _ Black _ _ _ _) _ ->
                    moveRedRight dict

                Node sorter Black _ _ (Leaf _) _ ->
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
                                Node _ color key value left right ->
                                    balance sorter color key value (removeMin left) right

                                (Leaf _) as leaf ->
                                    leaf

                _ ->
                    Node sorter color key value (removeMin left) right

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
                (Node sorter Black k v lRight (Node Red rK rV rLeft rRight))

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
inserted too.
-}
update : k -> (Maybe v -> Maybe v) -> Dict k v -> Dict k v
update key alter dict =
    case alter (get key dict) of
        Nothing ->
            remove key dict

        Just value ->
            insert key value dict



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
    fromSortedList True (foldr helper [] dict)


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
    case dict of
        Leaf _ ->
            acc

        Node _ _ key value left right ->
            foldr f (f key value (foldr f acc right)) left


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

        ( trues, falses ) =
            foldr helper ( [], [] ) dict
    in
    ( fromSortedList True trues, fromSortedList True falses )



-- COMBINE


{-| Combine two dictionaries. If there is a collision, preference is given
to the first dictionary.
-}
union : Dict k v -> Dict k v -> Dict k v
union left right =
    case ( left, right ) of
        ( _, Leaf _ ) ->
            left

        ( Leaf _, _ ) ->
            right

        _ ->
            let
                ( lt, gt ) =
                    foldl unionAccumulator ( [], toList right ) left
            in
            fromSortedList False (List.foldl (\e acc -> e :: acc) lt gt)


unionAccumulator : Sorter k -> k -> v -> ( List ( k, v ), List ( k, v ) ) -> ( List ( k, v ), List ( k, v ) )
unionAccumulator sorter lKey lVal ( result, rList ) =
    case rList of
        [] ->
            ( ( lKey, lVal ) :: result, [] )

        ( rKey, rVal ) :: rRest ->
            case Sort.toOrder sorter lKey rKey of
                LT ->
                    ( ( lKey, lVal ) :: result, rList )

                GT ->
                    unionAccumulator lKey lVal ( ( rKey, rVal ) :: result, rRest )

                EQ ->
                    ( ( lKey, lVal ) :: result, rRest )


{-| Keep a key-value pair when its key appears in the second dictionary.
Preference is given to values in the first dictionary.
-}
intersect : Dict k v -> Dict k v -> Dict k v
intersect left right =
    case ( getRange left, getRange right ) of
        ( _, Nothing ) ->
            empty

        ( Nothing, _ ) ->
            empty

        ( Just ( lMin, lMax ), Just ( rMin, rMax ) ) ->
            if lMax < rMin || rMax < lMin then
                -- disjoint ranges
                empty

            else
                fromSortedList False
                    (Tuple.first (foldl intersectAccumulator ( [], toList right ) left))


intersectAccumulator : k -> v -> ( List ( k, v ), List ( k, v ) ) -> ( List ( k, v ), List ( k, v ) )
intersectAccumulator lKey lVal (( result, rList ) as return) =
    case rList of
        [] ->
            return

        ( rKey, rVal ) :: rRest ->
            case compare lKey rKey of
                LT ->
                    return

                GT ->
                    intersectAccumulator lKey lVal ( result, rRest )

                EQ ->
                    ( ( lKey, lVal ) :: result, rRest )


{-| Keep a key-value pair when its key does not appear in the second dictionary.
-}
diff : Dict k v -> Dict k v -> Dict k v
diff left right =
    case ( getRange left, getRange right ) of
        ( _, Nothing ) ->
            left

        ( Nothing, _ ) ->
            empty

        ( Just ( lMin, lMax ), Just ( rMin, rMax ) ) ->
            if lMax < rMin || rMax < lMin then
                -- disjoint ranges
                left

            else
                fromSortedList False
                    (Tuple.first (foldl diffAccumulator ( [], toList right ) left))


diffAccumulator : k -> v -> ( List ( k, v ), List ( k, v ) ) -> ( List ( k, v ), List ( k, v ) )
diffAccumulator lKey lVal ( result, rList ) =
    case rList of
        [] ->
            ( ( lKey, lVal ) :: result, [] )

        ( rKey, rVal ) :: rRest ->
            case compare lKey rKey of
                LT ->
                    ( ( lKey, lVal ) :: result, rList )

                GT ->
                    diffAccumulator lKey lVal ( result, rRest )

                EQ ->
                    ( result, rRest )


getRange : Dict k v -> Maybe ( k, k )
getRange dict =
    case dict of
        Leaf _ ->
            Nothing

        Node _ _ key _ left right ->
            Just ( getMinKeyHelp key left, getMaxKeyHelp key right )


getMinKeyHelp : k -> Dict k v -> k
getMinKeyHelp minKey dict =
    case dict of
        Leaf _ ->
            minKey

        Node _ _ newMinKey _ left _ ->
            getMinKeyHelp newMinKey left


getMaxKeyHelp : k -> Dict k v -> k
getMaxKeyHelp maxKey dict =
    case dict of
        Leaf _ ->
            maxKey

        Node _ _ newMaxKey _ _ right ->
            getMaxKeyHelp newMaxKey right


{-| The most general way of combining two dictionaries. You provide three
accumulators for when a given key appears:

1.  Only in the left dictionary.
2.  In both dictionaries.
3.  Only in the right dictionary.
    You then traverse all the keys from lowest to highest, building up whatever
    you want.

-}
merge :
    (k -> a -> result -> result)
    -> (k -> a -> b -> result -> result)
    -> (k -> b -> result -> result)
    -> Dict k a
    -> Dict k b
    -> result
    -> result
merge leftStep bothStep rightStep leftDict rightDict initialResult =
    let
        stepState rKey rValue ( list, result ) =
            case list of
                [] ->
                    ( list, rightStep rKey rValue result )

                ( lKey, lValue ) :: rest ->
                    if lKey < rKey then
                        stepState rKey rValue ( rest, leftStep lKey lValue result )

                    else if lKey > rKey then
                        ( list, rightStep rKey rValue result )

                    else
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
    foldr (\key _ keyList -> key :: keyList) [] dict


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
fromList : List ( k, v ) -> Dict k v
fromList list =
    case list of
        pair :: rest ->
            let
                ( sorted, remainder ) =
                    splitSortedHelp [] pair rest
            in
            List.foldl
                (\( k, v ) dict -> insert k v dict)
                (fromSortedList False sorted)
                remainder

        [] ->
            empty


{-| Split a list into its sorted prefix and the remainder. The sorted prefix
is returned in reversed order.
-}
splitSortedHelp : List ( k, v ) -> ( k, v ) -> List ( k, v ) -> ( List ( k, v ), List ( k, v ) )
splitSortedHelp sorted (( k1, _ ) as p1) list =
    case list of
        (( k2, _ ) as p2) :: rest ->
            if k1 < k2 then
                splitSortedHelp (p1 :: sorted) p2 rest

            else
                ( sorted, p1 :: list )

        [] ->
            ( p1 :: sorted, [] )


{-| Convert an association list with sorted and distinct keys into a dictionary.
-}
fromSortedList : Bool -> List ( k, v ) -> Dict k v
fromSortedList isAsc list =
    case list of
        [] ->
            Leaf

        pair :: rest ->
            fromNodeList isAsc (sortedListToNodeList isAsc [] pair rest)


{-| Represents a non-empty list of nodes separated by key-value pairs.
-}
type alias NodeList k v =
    ( Dict k v, List ( ( k, v ), Dict k v ) )


{-| Convert a non-empty association list to the bottom level of nodes separated
by key-value pairs. (reverses order)
-}
sortedListToNodeList : Bool -> List ( ( k, v ), Dict k v ) -> ( k, v ) -> List ( k, v ) -> NodeList k v
sortedListToNodeList isAsc revList ( k1, v1 ) list =
    case list of
        [] ->
            ( Node Black k1 v1 Leaf Leaf, revList )

        ( k2, v2 ) :: [] ->
            if isAsc then
                ( Node Black k2 v2 (Node Red k1 v1 Leaf Leaf) Leaf, revList )

            else
                ( Node Black k1 v1 (Node Red k2 v2 Leaf Leaf) Leaf, revList )

        p2 :: ( k3, v3 ) :: [] ->
            ( Node Black k3 v3 Leaf Leaf, ( p2, Node Black k1 v1 Leaf Leaf ) :: revList )

        ( k2, v2 ) :: p3 :: p4 :: rest ->
            if isAsc then
                sortedListToNodeList isAsc (( p3, Node Black k2 v2 (Node Red k1 v1 Leaf Leaf) Leaf ) :: revList) p4 rest

            else
                sortedListToNodeList isAsc (( p3, Node Black k1 v1 (Node Red k2 v2 Leaf Leaf) Leaf ) :: revList) p4 rest


{-| Gather up a NodeList one level at a time, in successive passes of alternating
direction, until a single root-node remains.
-}
fromNodeList : Bool -> NodeList k v -> Dict k v
fromNodeList isReversed nodeList =
    case nodeList of
        ( node, [] ) ->
            node

        ( a, ( p1, b ) :: list ) ->
            fromNodeList (not isReversed)
                (accumulateNodeList isReversed [] a p1 b list)


{-| Gather up a NodeList to the next level. (reverses order)
-}
accumulateNodeList : Bool -> List ( ( k, v ), Dict k v ) -> Dict k v -> ( k, v ) -> Dict k v -> List ( ( k, v ), Dict k v ) -> NodeList k v
accumulateNodeList isReversed revList a ( k1, v1 ) b list =
    case list of
        [] ->
            if isReversed then
                ( Node Black k1 v1 b a, revList )

            else
                ( Node Black k1 v1 a b, revList )

        ( ( k2, v2 ), c ) :: [] ->
            if isReversed then
                ( Node Black k1 v1 (Node Red k2 v2 c b) a, revList )

            else
                ( Node Black k2 v2 (Node Red k1 v1 a b) c, revList )

        ( p2, c ) :: ( ( k3, v3 ), d ) :: [] ->
            if isReversed then
                ( Node Black k3 v3 d c, ( p2, Node Black k1 v1 b a ) :: revList )

            else
                ( Node Black k3 v3 c d, ( p2, Node Black k1 v1 a b ) :: revList )

        ( ( k2, v2 ), c ) :: ( p3, d ) :: ( p4, e ) :: rest ->
            if isReversed then
                accumulateNodeList isReversed (( p3, Node Black k1 v1 (Node Red k2 v2 c b) a ) :: revList) d p4 e rest

            else
                accumulateNodeList isReversed (( p3, Node Black k2 v2 (Node Red k1 v1 a b) c ) :: revList) d p4 e rest



-- Temp: Validation


validateInvariants : Dict k v -> String
validateInvariants dict =
    if not (isBST dict) then
        "Not in symmetric order"

    else if not (is23 dict) then
        "Not a 2-3 tree"

    else if not (isBalanced dict) then
        "Not balanced"

    else
        ""


isBST : Dict k v -> Bool
isBST dict =
    isBSTHelper True (keys dict)


isBSTHelper : Bool -> List k -> Bool
isBSTHelper acc keys =
    case keys of
        [] ->
            acc

        x :: [] ->
            acc

        x :: y :: xs ->
            isBSTHelper (acc && x < y) (y :: xs)


is23 : Dict k v -> Bool
is23 dict =
    is23Helper dict dict


is23Helper : Dict k v -> Dict k v -> Bool
is23Helper root node =
    case node of
        Leaf _ ->
            True

        Node _ clr _ _ left right ->
            if isRed right then
                False

            else if node /= root && clr == Red && isRed left then
                False

            else
                is23Helper root left && is23Helper root right


isRed : Dict k v -> Bool
isRed dict =
    case dict of
        Node _ Red _ _ _ _ ->
            True

        _ ->
            False


isBalanced : Dict k v -> Bool
isBalanced dict =
    isBalancedHelper dict <| isBalancedBlacksHelper dict 0


isBalancedBlacksHelper : Dict k v -> Int -> Int
isBalancedBlacksHelper node blacks =
    case node of
        Leaf _ ->
            blacks

        Node _ color _ _ left _ ->
            if color == Red then
                isBalancedBlacksHelper left blacks

            else
                isBalancedBlacksHelper left (blacks + 1)


isBalancedHelper : Dict k v -> Int -> Bool
isBalancedHelper node blacks =
    case node of
        Leaf _ ->
            blacks == 0

        Node _ color _ _ left right ->
            let
                nextBlacks =
                    if color == Red then
                        blacks

                    else
                        blacks - 1
            in
            isBalancedHelper left nextBlacks && isBalancedHelper right nextBlacks
