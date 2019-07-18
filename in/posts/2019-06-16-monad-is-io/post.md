## Citations

Some context that inspired this post:

  - *Metaphors we Live By*, George Lakoff and Mark Johnson, 1980
  - [*The Unreasonable Effectiveness of Metaphor*](https://argumatronic.com/posts/2018-09-02-effective-metaphor.html), Julie Moronuki, 2018

## Tony Morris

Tony Morris provides the perfect antithesis to frame this post against. From his 2009 talk, [*What Does Monad Mean?*](http://data.tmorris.net/talks/what-does-monad-mean/what-does-monad-mean/html/index.html):

  1. "Monad metaphors are stupid"
  2. "Metaphors are training wheels that obscure learning opportunities and stall progress."
  3. "A monad is any instance of [the `Monad` class that satisfies the monad laws]. That is *all it is*."  

When I read these three quotes to Julie, she said "Only the first two are wrong." After I pressed my objection to number three, she elaborated: "That *is* all it is, it's just not all we need *to understand them*."

## Burrito

It seems to be a common perspective.

## Tomas Petricek

[What we talk about when we talk about monads](http://tomasp.net/academic/papers/monads/monads-programming.pdf), Tomas Petricek, 2012

> Even a pure formalist treatment of monads – as syntactic forms with no inherent meaning that are manipulated through string operations – can be understood in terms of metaphors. The hidden metaphor ismovement. When we say, “in the next step [of a proof]”, we are expressing ourselves as if we were talking about moving along a path.

## Mark Jones

[*Functional Programming with Overloading and Higher-Order Polymorphism*](http://web.cecs.pdx.edu/~mpj/pubs/springschool.html), Mark P. Jones, 1995

  - "One useful way to think about monads is as a means of representing computations. If `m` is a monad, then an object of type `m a` represents a computation that is expected to produce a result of type `a`. [...] Simple examples include state, exceptions and input/output."
   
  - Writing `bind` as an infix operator, we can think of `c ‘bind‘ f` as a computation which runs `c`, passes the result `x` of type `a` to `f`, and runs the computation `f x` to obtain a final result of type `b`.

## "Run"

We find numerous references to "running" in Andy Gill's `transformers` library, first uploaded to Hackage [in 2009](https://hackage.haskell.org/package/transformers-0.0.0.0/docs/src/Control-Monad-Trans-State-Lazy.html#StateT) by Ross Paterson:

```haskell
newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }
```

## "Effect"

[*The essence of the Iterator pattern*](http://www.cs.ox.ac.uk/people/jeremy.gibbons/publications/#iterator) by Jeremy Gibbons and Bruno C. d. S. Oliveira, 2007

## Metaphor: AN EXECUTION THREAD IS A MOTOR VEHICLE

  - A program *starts*, *runs*, and *stops*.
  - A program may *crash*.
  - After a *fork*, two programs run *in parallel*.
  - A program may *run slowly under a heavy load*. 

## Metaphor: MONAD IS AN EXECUTION THREAD

## Metaphors in disguise

Lakoff argues that metaphor pervades all language, and the distinction between language we perceive as "literal" versus "metaphoric" is often made only by whether the metaphor is normalized in the culture.

In Chapter 11, *The Partial Nature of Metaphorical Meaning*, the example is THEORIES ARE BUILDINGS. If we say "he constructed a theory", this is not perceived as metaphoric language (though it is coherent with the THEORIES ARE BUILDINGS metaphor). It we say "his theory has long, winding corridors", we recognize that language as metaphor. Not because this second sentence is any more metaphoric than the first, but merely because it chooses an unconventional aspect -- the corridors -- and so this expression stands out to us as imaginative.

Every time these phrases slip in,

  - "Run"
  - "Effect"
  - "Result" (describing the aspect of a function's codomain which is considered separate from the "effect")

we are relying on the MONAD IS AN EXECUTION THREAD metaphor. The examples from Tomas Petricek and Mark Jones above have directly called out this comparison. Most of us speak these words literally, without an explicit acknowledgement that a conceptual metaphor is in play.

## Language learning

We all know that TIME IS SPACE; the future is in front of us and the past is behind us. Except this isn't universal; if you speak Navajo, the past is in front of you and the future is behind you. When learning a second language, accepting it and not just perceiving it as *weird* can benefit from reflecting on the conceptual metaphors built into your own culture, so you can see how they differ in the target language.

Imperative paradigm is largely based on the conceptual metaphor AN EXECUTION THREAD IS A COOKBOOK, and reflecting on that can give us a basis for understanding how the functional paradigm is grounded in different metaphors. 

One difference between programming and natural language is how often and rapidly we *invent* conceptual metaphors.
