---
layout: post
title: "Randomization pipeline 2: Lazy scoring"
date_granularity: day
tags: algorithms randomness haskell
---

This post is a followup to [*Randomization pipeline*][previous] from seven years
ago and an apology for the mathematical indiscretions of my youth.

The problem
-----------

I had wondered how best to select a uniformly random subset of some fixed *n*
items from a stream. For example, in a shell we could do something like this for
*n=3*:

```
> seq 99 | shuf | head -n 3 | tr '\n' ' '
27 62 96
```

What troubled me about this approach is that `shuf` inherently requires reading
the entire input into memory. When the number of inputs is very large and the
number of outputs is very small, this is a bothersome abuse of space.

My first solution
-----------------

Here's what I settled on back then:

* Maintain a set of accepted items, initially empty.
* Accept the first *n* items into the accepted set unconditionally.
* For the rest of the items, accept the *i*<sup>th</sup> item into the accepted
  set with probability *n/i*, displacing one of the previously-accepted items at
  random.

The flaws
---------

It took some effort to prove that each element has equal likelihood of inclusion
in the accepted set, and I did not bother to prove that the items' inclusion are
mutally independent.

Also, the implementation suffered from imprecision in its random choices. How do
you simulate a Bernoulli trial with probability *p = 3/7* using binary entropy?
My crude approach was to sample a large space *[1, 2<sup>k</sup>]* and accept
when the result is *≤ p × 2<sup>k</sup>*. We can get arbitrarily close to the
desired probability this way, but it is imperfect.

We could make this exact using a [Las Vegas][vegas] algorithm. To simulate a
trial that succeeds with probability *a/b*:

* Let *x* be a sample from *[1, 2<sup>⌊log<sub>2</sub> b⌋</sup>]*
* If *x ≤ a*, return *1*; if *a < x ≤ b*, return *0*; otherwise, retry.

This all feels inelegant, so I started thinking about a new approach that
minimizes the number of coin flips.

The new solution
----------------

The new approach is a lot easier to understand at a high level:

* Assign each item a random score, and accept the *n* highest-scored items.
* To use space efficently, only keep the top *n*-scoring items in memory.
* Since we don't know up front how big a space to sample to obtain the scores,
  generate the scores lazily.

For illustration, suppose our scores are infinite-precision decimals between 0
and 1, and define a finite partially-generated score *x<sub>i</sub>* to be score
*x* rounded to a finite precision of *i* decimal places.

If one partial score is a prefix of another, the two are incomparable, then we
need to generate more digits to determine which score is higher. For example, if
*a_2* = `0.25` and *b_3* = `0.258`, then we don't yet know whether *a* or *b* is
larger.

Note that base 10 was chosen only to aid intuition in this explanation; the
algorithm is just the same if we represent our fractions in base 2, so partial
scores can simply be binary strings.

[previous]: http://chris-martin.org/2009/randomization-pipeline
[vegas]: https://en.wikipedia.org/wiki/Las_Vegas_algorithm
