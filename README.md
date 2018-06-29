# elm-sorter-experiment

It's right there in the name, but just to be totally clear about it:

⚠️  **THIS IS AN EXPERIMENT!** ⚠️

I have a hypothesis that these may be good ideas, but they may not be! The
purpose of this package is to facilitate trying them out and learning more.

This package is a group of related experiments I've been pondering for awhile.
They are:

1. A composable `Sorter` API that replaces `List.sort`, `List.sortBy`, and `List.sortWith` with a single function (`Sort.list`). In particular, `Sort.by` and `Sort.reverse` (when used together) nicely scratch an itch I've encountered on a few different occasions.
2. An implementation of `Dict` and `Set` that use `Sorter` to permit keys that are not `comparable`.
3. Some minor naming changes to `Dict` and `Set` (explained below).

Since we're already in experimental territory, I based this experiment on another
one; this is using [`Skinney/elm-dict-exploration`](http://package.elm-lang.org/packages/Skinney/elm-dict-exploration/latest)
under the hood for some performance benefits.

### Prior art

* Elm's core [`Dict`](http://package.elm-lang.org/packages/elm-lang/core/latest/Dict) and [`Set`](http://package.elm-lang.org/packages/elm-lang/core/5.1.1/Set) modules, by Evan Czaplicki
* [`eeue56/elm-all-dict`](http://package.elm-lang.org/packages/eeue56/elm-all-dict/latest) by Noah Hall
* [`robertjlooby/elm-generic-dict`](http://package.elm-lang.org/packages/robertjlooby/elm-generic-dict/latest) by Robert J. Looby
* [`Skinney/elm-dict-exploration`](http://package.elm-lang.org/packages/Skinney/elm-dict-exploration/latest) by Robin Heggelund Hansen

## `Set` and `Dict` API Changes

In this package, the following operations on both `Set` and `Dict` now take
a `Sorter` as their first argument:

* `empty`
* `singleton`
* `fromList`
* `merge`

Additionally, `diff` has been given named arguments in order to prevent ordering mistakes. It is now:

```elm
{-| Keep a value when it appears in the `original` set
but not in the `other` set. The `original` set's `Sorter` will be used.

-}
diff : { original : Set a, other : Set a } -> Set a
```

## `Set` API Changes

### `insert`
```diff
-insert : a -> Set a -> Set a
+add : a -> Set a -> Set a
```

There is strong consensus among languages for what this operation should be called.
Python, Ruby, Java, JavaScript, and OCaml, all call this `add`. Erlang calls it
`addElement` and Scala overloads `(+)` for this operation.

Only Haskell calls it `insert`.

### `map`

```diff
-map : (a -> b) -> Set a -> Set b
+map : Sorter b -> (a -> b) -> Set a -> Set b
```

An extra argument is needed to specify the `Sorter` for the resulting `Set`.

### `intersect` and `union`

```diff
-intersect : Set a -> Set a -> Set a
+intersect : Sorter a -> Set a -> Set a

-union : Set a -> Set a -> Set a
+union : Sorter a -> Set a -> Set a
```

These now take an explicit `Sorter` in order to make it clear which sorting
operation will be used.

## `Dict` API Changes

### `insert`

```diff
-insert : k -> v -> Dict k v -> Dict k v
+store : k -> v -> Dict k v -> Dict k v
```

There is no consensus among languages for what this operation should be called.
Ruby's `store` seems clearer than Haskell's `insert` because "inserting" is
often an operation that strictly increases the size of the collection
(e.g. `INSERT` in SQL) whereas "storing" is idempotent.

(Other alternatives considered: `put` from Java, `add` from OCaml, `replace`
from ReasonML, `set` from JavaScript.)

### `intersect`

```diff
-intersect : Dict k v -> Dict k v -> Dict k v
+intersect : { preferred : Dict k v, other : Dict k v } -> Dict k v
```

`intersect` has been given named arguments in order to clarify which values will be kept.

### `union`

```diff
-union : Dict k v -> Dict -> k v -> Dict k v
+storeAll : { from : Dict k v, into : Dict k v } -> Dict k v
```

`union` has been renamed and given named arguments to make it clearer what happens
in the event of collisions.

"It takes all the key/value pairs from one dictionary and `store`s them into
the other one" builds on the caller's understanding of how `store` resolves
collisions.

This API also makes it clear which dictionary's `Sorter` will be used.

