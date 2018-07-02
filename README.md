# elm-sorter-experiment

It's right there in the name, but just to be totally clear about it:

⚠️  **THIS IS AN EXPERIMENT!** ⚠️

I have a hypothesis that these may be good ideas, but they may not be! The
purpose of this package is to facilitate trying them out and learning more.

This package is a group of related experiments I've been pondering for awhile.
They are:

1. A composable `Sorter` API that replaces `List.sort`, `List.sortBy`, and `List.sortWith` with a single function (`Sort.list`). In particular, `Sort.by` and `Sort.reverse` (when used together) nicely scratch an itch I've encountered on a few different occasions.
2. An implementation of `Dict` and `Set` that use `Sorter` to permit keys that are not `comparable`.
3. Some API changes to `Dict` and `Set` that seem nicer independent of (but especially in conjunction with) the introduction of `Sorter`.

Since we're already in experimental territory, I based this experiment on another
one; this is using [`Skinney/elm-dict-exploration`](http://package.elm-lang.org/packages/Skinney/elm-dict-exploration/latest)
under the hood for some performance benefits.

### Prior art

* Elm's core [`Dict`](http://package.elm-lang.org/packages/elm-lang/core/latest/Dict) and [`Set`](http://package.elm-lang.org/packages/elm-lang/core/5.1.1/Set) modules, by Evan Czaplicki
* [`eeue56/elm-all-dict`](http://package.elm-lang.org/packages/eeue56/elm-all-dict/latest) by Noah Hall
* [`robertjlooby/elm-generic-dict`](http://package.elm-lang.org/packages/robertjlooby/elm-generic-dict/latest) by Robert J. Looby
* [`Skinney/elm-dict-exploration`](http://package.elm-lang.org/packages/Skinney/elm-dict-exploration/latest) by Robin Heggelund Hansen
* [`NoRedInk/elm-compare`](http://package.elm-lang.org/packages/NoRedInk/elm-compare/latest) by Jasper Woudenberg

## `Set` and `Dict` API Changes

In this package, `filter` has been replaced by `keepIf` and `dropIf`.

The argument order on `member` has also been flipped, and its name has been
adjusted to reflect the new ordering:

```elm
Set.memberOf : Set a -> a -> Bool
Dict.memberOf : Dict k v -> k -> Bool
```

In practice, at work we've wanted to partially apply `member` almost always
with this argument ordering rather than the one currently in `elm/core`. This
ordering works wonderfully with `keepIf` and `dropIf`:

```elm
positiveEvens =
    Set.keepIf (Set.memberOf evens) positives

positiveOdds =
    Set.dropIf (Set.memberOf evens) positives
```

Now that it's so easy to filter based on other sets, it's questionable whether
`Set.intersect` and `Set.diff` should remain in the API. (More on this later.)

Finally, the following operations on `Set` and `Dict` now take
a `Sorter` as their first argument:

* `empty`
* `singleton`
* `fromList`
* `merge`

## Other `Set` API Changes

`Set.union` takes a `Sorter` as its first argument. It is now:

```elm
Set.union : Sorter a -> Set a -> Set a -> Set a
```

Since it uses the given `Sorter` to sort the sets, the order of the two sets
does not matter.

> Alternatively, `union` could be renamed to `insertAll` and could be documented in terms of "inserting all the values from one set into the other" instead of being called `union`, which is an operation where order is not supposed to matter. See `Dict.insertAll`.

`Set.diff` and `Set.intersect` have been removed in favor of using `keepIf` and `dropIf`,
which work nicely with `Set.memberOf`:

```elm
positiveEvens =
    Set.keepIf (Set.memberOf evens) positives
    -- Set.intersect positives evens

positiveOdds =
    Set.dropIf (Set.memberOf evens) positives
    -- Set.diff positives evens
```

`map` needs an extra argument to specify the `Sorter` for the resulting `Set`.
It is now:

```elm
map : Sorter b -> (a -> b) -> Set a -> Set b
```

## Other `Dict` API Changes

`Dict.union` has been renamed to `Dict.insertAll`:

```elm
{-| Take all the key-value pairs in the first dictionary and
`insert` them into the second dictionary.
-}
insertAll : Dict k v -> Dict k v -> Dict k v
```

This makes it clear what happens when both dictionaries have the same key but
different values: it does what `insert` would do in that situation. (It also
makes it clear which dictionary's `Sorter` will be used - once again, by
looking to what `insert` does.)

> It could be even clearer with labeled arguments. Here's a way to do that without sacrificing partial application: `insertAll : { from : Dict k v } -> Dict k v -> Dict k v`. So you'd call it with `Dict.insertAll { from = otherDict } someDict`. Definitely looks weird though. I'm unsure that this would be better overall.

Additionally, `diff` and `intersect` have been removed.

In both the `comparable` and the `Sorter` APIs, there are a few issues with
these functions:

1. In both functions, it matters which `Dict` is passed first and which is passed second. However, looking at a call site, it is unclear which ordering leads to which outcome. This means that not only can the compiler not help with mistakes, it is also hard for programmers to spot mistakes. It's common to need to consult documentation to understand what a `Dict.intersect` or `Dict.diff` call is actually doing.
1. Further compounding the previous point, their argument ordering is inconsistent with respect to one another, making it easy to mix up which has which order. `Dict.intersect` does the equivalent of `Dict.keepIf` on the second dictionary (checking for membership in the first dictionary), whereas `Dict.diff` does the equivalent of `Dict.dropIf` on the *first* dictionary (checking for membership in the *second* dictionary). Because of this, `Dict.diff` is also generally inconsistent with how other functions in `elm/core` order their arguments; it would be more typical if flipped.
1. They are rarely used in Elm, and no other programming language except Haskell implements them for their dictionary equivalents. Even if there were no other concerns with these functions, it's debatable whether they merit inclusion in the API - especially considering they are convenience shorthands for simple `keepIf`/`dropIf` calls rather than sources of new functionality.

These are all problems that exist with the current `comparable` API, but they
are exacerbated by the `Sorter` API.

Fortunately, using `Dict.keepIf` and `Dict.dropIf` directly has none of these
problems. Since it is generally a is better choice to use `Dict.keepIf` and
`Dict.dropIf` over `Dict.intersect` and `Dict.diff`, those functions
have been removed.
