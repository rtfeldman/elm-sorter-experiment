module Sort exposing
    ( Sorter
    , custom, toOrder
    , reverse, by, tiebreaker
    , alphabetical, increasing
    , list
    )

{-| Sort values. You can use this as a self-documenting alternative to `List.sort`:

    Sort.list Sort.alphabetical [ "foo", "bar", "baz" ]
        --> [ "bar", "baz", "foo" ]

You can also use [`Sort.by`](#by), [`Sort.reverse`](#reverse), and [`Sort.tiebreaker`](#tiebreaker) to transform sorters:

    -- Sort users by name, in reverse alphabetical order
    Sort.list
        (Sort.by .name (Sort.reverse Sort.alphabetical))
        users

Finally, you can use these to create sets and dictionaries with keys that
do not have to be `comparable`. See the `Sort.Dict` and `Sort.Set` modules.


## Types

@docs Sorter


## Conversions

@docs custom, toOrder


## Composing Sorters

@docs reverse, by, tiebreaker


## Primitives

@docs alphabetical, increasing


## Collections

@docs list

-}

-- TYPES --


{-| A description of how to sort values of a particular type.
-}
type Sorter a
    = Sorter (a -> a -> Order)



-- CONVERSIONS --


{-| Reverse a sorter.

    Sort.list Sort.alphabetical [ "b", "c", "a" ]
    --> [ "a", "b", "c" ]

    Sort.list (Sort.reverse Sort.alphabetical) [ "b", "c", "a" ]
    --> [ "c", "b", "a" ]

-}
reverse : Sorter a -> Sorter a
reverse (Sorter sort) =
    Sorter (flip sort)


flip : (a -> b -> c) -> b -> a -> c
flip fn a b =
    fn b a


{-| Sort by a derived value.

One way to use this is with collections of records:

    Sort.list (Sort.by .name Sort.alphabetical)
      [ { name = "Bo" }, { name = "Amy" }, { name = "Cam" } ]
    --> [ { name = "Amy" }, { name = "Bo" }, { name = "Cam" } ]

Another use is for unwrapping union type constructors:

    import Sort exposing (Sorter)

    type Username
        = Username String

    alphabetical : Sorter Username
    alphabetical =
        Sort.by (\(Username str) -> str) Sort.alphabetical

-}
by : (b -> a) -> Sorter a -> Sorter b
by transform (Sorter sort) =
    Sorter (\first second -> sort (transform first) (transform second))


{-| Apply a tiebreaker `Sorter` to use when the given `Sorter` finds the
two values are equal.

This would be useful in a table that is sorted by multiple columns.

    -- Without tiebreaker
    Sort.list (Sort.by .name Sort.alphabetical)
        [ { name = "Bo", cats = 4 }, { name = "Bo", cats = 2 }, { name = "Amy", cats = 3 } ]
        --> [ { name = "Amy", cats = 3 }, { name = "Bo", cats = 4 }, { name = "Bo", cats = 2 } ]

    -- With tiebreaker, the last two records are swapped
    Sort.list (Sort.by .name Sort.alphabetical |> Sort.tiebreaker (Sort.by .cats Sort.increasing))
        [ { name = "Bo", cats = 4 }, { name = "Bo", cats = 2 }, { name = "Amy", cats = 3 } ]
        --> [ { name = "Amy", cats = 3 }, { name = "Bo", cats = 2 }, { name = "Bo", cats = 4 } ]

-}
tiebreaker : Sorter b -> Sorter b -> Sorter b
tiebreaker (Sorter breakTie) (Sorter sort) =
    Sorter (sortWithTiebreaker breakTie sort)


sortWithTiebreaker : (a -> a -> Order) -> (a -> a -> Order) -> a -> a -> Order
sortWithTiebreaker breakTie sort first second =
    case sort first second of
        LT ->
            LT

        GT ->
            GT

        EQ ->
            breakTie first second


{-| Create a custom [`Sorter`](#Sorter) by defining how to order two values.
-}
custom : (a -> a -> Order) -> Sorter a
custom sort =
    Sorter sort


{-| Determine an Order for two values, based on the given
[`Sorter`](#Sorter)
-}
toOrder : Sorter a -> a -> a -> Order
toOrder (Sorter sort) first second =
    sort first second



-- PRIMITIVES --


{-| Sort strings alphabetically.

    Sort.list Sort.alphabetical [ "cabbage", "banana", "apple" ]
    --> [ "apple", "banana", "cabbage" ]

To sort in reverse alphabetical order, use [`Sort.reverse`](#reverse)

    Sort.list (Sort.reverse Sort.alphabetical) [ "cabbage", "banana", "apple" ]
    --> [ "cabbage",  "banana", "apple" ]

-}
alphabetical : Sorter String
alphabetical =
    Sorter compare


{-| Sort numbers from lowest to highest.

    Sort.list Sort.increasing [ 4, 7, 2 ]
    --> [ 2, 4, 7 ]

To sort decreasing, use [`Sort.reverse`](#reverse)

    Sort.list (Sort.reverse Sort.increasing) [ 4, 7, 2 ]
    --> [ 7, 4, 2 ]

-}
increasing : Sorter number
increasing =
    Sorter compareNumbers



-- COLLECTIONS --


{-| Sort a list using a `Sorter`.

This can be used as an alternative to `List.sort`, `List.sortBy`, and `List.sortWith`.

    Sort.list Sort.alphabetical [ "foo", "bar", "baz" ]
        --> [ "baz", "bar", "foo" ]

You can also use `Sort.by` and `Sort.reverse` to transform sorters:

    -- Sort users by name, in reverse alphabetical order
    Sort.list
        (Sort.by .name (Sort.reverse Sort.alphabetical))
        users

-}
list : Sorter a -> List a -> List a
list (Sorter sort) elems =
    List.sortWith sort elems



-- INTERNAL --


compareNumbers : number -> number -> Order
compareNumbers first second =
    -- For some reason, without this +0 it won't type check because number
    -- is not considered comparable. Not sure why the +0 would change that,
    -- but here we are!
    compare (first + 0) second
