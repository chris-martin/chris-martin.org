---
layout: post
title: "Extending Scala collections"
date: 2014-09-13 17:12
timezone: EST
tags: scala
---

{% highlight scala %}
import scala.collection.SeqLike
import scala.collection.generic.CanBuildFrom

implicit class Seq_[A, S,
    R : ({ type F[X] = X => SeqLike[A, S] })#F](seq: R) {

  def applyAtIndex[B >: A, T](i: Int)(f: A => B)
      (implicit bf: CanBuildFrom[S, B, T]): T =
    seq.updated(i, f(seq(i)))
}
{% endhighlight %}

Element types:

* `A` - The elements in the original collection
* `B` - The elements in the resulting collection

Collection types:

* `R` - The original collection of `A`
* `S` - A sequence of `A` into which `R` can be converted
* `T` - The resulting collection of `B`

[stackoverflow]: https://stackoverflow.com/questions/25800702/apply-function-to-one-element-only-in-list-or-array-in-scala/
