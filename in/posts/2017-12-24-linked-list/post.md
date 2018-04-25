--------------------------------------------------------------------------------
title:    More than linked lists
date:     2017 Dec 24
slug:     more-than-linked-lists
abstract: Linked lists are prevalent in functional programming, but they don't take the place of other kinds of lists.
--------------------------------------------------------------------------------

A [quote]:

> Functional programming uses lists as the primitive data type but it's a really annoying one to use in practice. You don't have (fast) random access nor size, performance isn't awesome as you've got a lot of pointer dereferences and bad cache coherence, you cannot easily go backwards... Growable arrays have much better properties.

Apologies to the author, who has prompted too much uproar and now just wants to be left alone. I wanted to try to write a helpful [response][tweet], though, because this is a misconception that I used to have. I believe it's quite common.

---

When I first glimpsed into the FP world, I recall thinking: *Immutable data structures? That sounds really limiting.* Then I saw the FP people talking about linked lists all the time, which only intensified that concern. But after living in that world for a while, what I find is:

**Yes** — The linked list shows up far more in functional programming than other kinds of programming.

**No** — We don't use linked lists in situations that call for properties like fast random access.

FP's extensive use of linked lists only augments the sorts of data structures you're used to, it doesn't replace them. In Haskell when we need a fancy list structure we'll use either [`Vector`][vector] which is contiguous in memory, or `Seq` (short for "sequence") which has efficient operations for concatenation, insertion, etc. `Seq` is the standard choice with suitable performance for most purposes. It is in the [containers] package. Don't let the dependencies be offputting — these packages are commonplace, de facto parts of the standard library.

If you come from the world of strict evaluation, then linked lists in the presence of lazy evaluation are not at all what you think they are. You don't use a linked list in Haskell in a case where you'd be using an `ArrayList` in Java; you use a linked list in Haskell in a case where you'd be using a `for` loop in Java. The linked list in lazy FP corresponds to things that in other paradigms aren't represented as data types at all. And, by the way, the lists often get fused out of existence by compiler optimizations and don't end up constructed at runtime at all.

  [quote]: https://gist.github.com/vjeux/cc2c4f83a6b60d69b79057b6ef651b56
  [containers]: https://hackage.haskell.org/package/containers
  [vector]: https://hackage.haskell.org/package/vector
  [tweet]: https://twitter.com/chris__martin/status/944641666404806657
