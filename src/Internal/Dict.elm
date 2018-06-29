module Internal.Dict
    exposing
        ( Color(..)
        , Dict(..)
        , foldr
        , fromSortedList
        , getRange
        , getSorter
        , intersectAccumulator
        , keys
        , unionAccumulator
        , validateInvariants
        )

import Sort exposing (Sorter)



-- TYPES --


type Dict key value
    = Leaf (Sorter key)
    | Node (Sorter key) Color key value (Dict key value) (Dict key value)


{-| The color of a Node. Leafs are considered black.
-}
type Color
    = Black
    | Red


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


getSorter : Dict k v -> Sorter k
getSorter dict =
    case dict of
        Leaf sorter ->
            sorter

        Node sorter _ _ _ _ _ ->
            sorter


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
                    unionAccumulator sorter lKey lVal ( ( rKey, rVal ) :: result, rRest )

                EQ ->
                    ( ( lKey, lVal ) :: result, rRest )


intersectAccumulator : Sorter k -> k -> v -> ( List ( k, v ), List ( k, v ) ) -> ( List ( k, v ), List ( k, v ) )
intersectAccumulator sorter lKey lVal (( result, rList ) as return) =
    case rList of
        [] ->
            return

        ( rKey, rVal ) :: rRest ->
            case Sort.toOrder sorter lKey rKey of
                LT ->
                    return

                GT ->
                    intersectAccumulator sorter lKey lVal ( result, rRest )

                EQ ->
                    ( ( lKey, lVal ) :: result, rRest )


{-| Convert an association list with sorted and distinct keys into a dictionary.
-}
fromSortedList : Sorter k -> Bool -> List ( k, v ) -> Dict k v
fromSortedList sorter isAsc list =
    case list of
        [] ->
            Leaf sorter

        pair :: rest ->
            fromNodeList sorter isAsc (sortedListToNodeList sorter isAsc [] pair rest)


{-| Gather up a NodeList one level at a time, in successive passes of alternating
direction, until a single root-node remains.
-}
fromNodeList : Sorter k -> Bool -> NodeList k v -> Dict k v
fromNodeList sorter isReversed nodeList =
    case nodeList of
        ( node, [] ) ->
            node

        ( a, ( p1, b ) :: list ) ->
            fromNodeList sorter
                (not isReversed)
                (accumulateNodeList sorter isReversed [] a p1 b list)


{-| Represents a non-empty list of nodes separated by key-value pairs.
-}
type alias NodeList k v =
    ( Dict k v, List ( ( k, v ), Dict k v ) )


{-| Convert a non-empty association list to the bottom level of nodes separated
by key-value pairs. (reverses order)
-}
sortedListToNodeList : Sorter k -> Bool -> List ( ( k, v ), Dict k v ) -> ( k, v ) -> List ( k, v ) -> NodeList k v
sortedListToNodeList sorter isAsc revList ( k1, v1 ) list =
    case list of
        [] ->
            ( Node sorter Black k1 v1 (Leaf sorter) (Leaf sorter), revList )

        ( k2, v2 ) :: [] ->
            if isAsc then
                ( Node sorter Black k2 v2 (Node sorter Red k1 v1 (Leaf sorter) (Leaf sorter)) (Leaf sorter), revList )

            else
                ( Node sorter Black k1 v1 (Node sorter Red k2 v2 (Leaf sorter) (Leaf sorter)) (Leaf sorter), revList )

        p2 :: ( k3, v3 ) :: [] ->
            ( Node sorter Black k3 v3 (Leaf sorter) (Leaf sorter), ( p2, Node sorter Black k1 v1 (Leaf sorter) (Leaf sorter) ) :: revList )

        ( k2, v2 ) :: p3 :: p4 :: rest ->
            if isAsc then
                sortedListToNodeList sorter isAsc (( p3, Node sorter Black k2 v2 (Node sorter Red k1 v1 (Leaf sorter) (Leaf sorter)) (Leaf sorter) ) :: revList) p4 rest

            else
                sortedListToNodeList sorter isAsc (( p3, Node sorter Black k1 v1 (Node sorter Red k2 v2 (Leaf sorter) (Leaf sorter)) (Leaf sorter) ) :: revList) p4 rest


{-| Gather up a NodeList to the next level. (reverses order)
-}
accumulateNodeList : Sorter k -> Bool -> List ( ( k, v ), Dict k v ) -> Dict k v -> ( k, v ) -> Dict k v -> List ( ( k, v ), Dict k v ) -> NodeList k v
accumulateNodeList sorter isReversed revList a ( k1, v1 ) b list =
    case list of
        [] ->
            if isReversed then
                ( Node sorter Black k1 v1 b a, revList )

            else
                ( Node sorter Black k1 v1 a b, revList )

        ( ( k2, v2 ), c ) :: [] ->
            if isReversed then
                ( Node sorter Black k1 v1 (Node sorter Red k2 v2 c b) a, revList )

            else
                ( Node sorter Black k2 v2 (Node sorter Red k1 v1 a b) c, revList )

        ( p2, c ) :: ( ( k3, v3 ), d ) :: [] ->
            if isReversed then
                ( Node sorter Black k3 v3 d c, ( p2, Node sorter Black k1 v1 b a ) :: revList )

            else
                ( Node sorter Black k3 v3 c d, ( p2, Node sorter Black k1 v1 a b ) :: revList )

        ( ( k2, v2 ), c ) :: ( p3, d ) :: ( p4, e ) :: rest ->
            if isReversed then
                accumulateNodeList sorter isReversed (( p3, Node sorter Black k1 v1 (Node sorter Red k2 v2 c b) a ) :: revList) d p4 e rest

            else
                accumulateNodeList sorter isReversed (( p3, Node sorter Black k2 v2 (Node sorter Red k1 v1 a b) c ) :: revList) d p4 e rest


keys : Dict k v -> List k
keys dict =
    foldr (\key _ keyList -> key :: keyList) [] dict


foldr : (k -> v -> b -> b) -> b -> Dict k v -> b
foldr f acc dict =
    case dict of
        Leaf _ ->
            acc

        Node _ _ key value left right ->
            foldr f (f key value (foldr f acc right)) left



-- VALIDATION --


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
    isBSTHelper (getSorter dict) True (keys dict)


isBSTHelper : Sorter k -> Bool -> List k -> Bool
isBSTHelper sorter acc keyList =
    case keyList of
        [] ->
            acc

        x :: [] ->
            acc

        x :: y :: xs ->
            let
                xLessThanY =
                    case Sort.toOrder sorter x y of
                        LT ->
                            True

                        _ ->
                            False
            in
            isBSTHelper sorter (acc && xLessThanY) (y :: xs)


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
