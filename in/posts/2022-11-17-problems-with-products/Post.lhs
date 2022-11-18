> module Post where

> import Control.Applicative (liftA2)
> import Data.Function ((&))
> import Data.Map (Map)
> import qualified Data.Map as Map

Where we see a type parameter to the right of a function arrow (or with no arrow
at all, since `a` is isomorphic to `() -> a`), we have a covariant functor (or
simply, "functor").

> newtype Decode k v a = Decode{ decode :: Map k v -> a }

Covariant functors quite often admit an *applicative* aspect that allows us to
build up larger stuff within the type parameter.

> (★) :: Decode k v a -> Decode k v b -> Decode k v (a, b)

> at :: Ord k => k -> Decode k Char Char
> at k = Decode (Map.findWithDefault '\0' k)

> abc :: Map Int Char
> abc = [(1, 'a'), (2, 'b'), (3, 'c')]

> assertion1 = (abc & decode (at 1 ★ at 2))  ==  ('a', 'b')

To define `(★)` is left as an exercise for the reader; it is a good exercise for
anyone who does not already immediately know how. The particularly
Haskell-savvy, however, will likely choose to use a derived `Applicative`
instance.

> deriving newtype instance Functor (Decode k v)
> deriving newtype instance Applicative (Decode k v)

> (★) = liftA2 (,)

The two-tuple only takes us so far, and sooner or later (but probably sooner, if
we're not just playing around) we'll want to start working with larger products.
More than two things. `(★)` can be nested. (I knew this ahead of time, which is
why I named it as an infix operator.)

> assertion2 = (abc & decode (at 1 ★ at 2))           ==  ('a', 'b')
> assertion3 = (abc & decode ((at 1 ★ at 2) ★ at 3))  ==  (('a', 'b'), 'c')
> assertion4 = (abc & decode (at 1 ★ (at 2 ★ at 3)))  ==  ('a', ('b', 'c'))

This, of course, sucks, because nobody wants nested tuples like that. You want a
three-tuple, right? Thus enters the real clever trick of the `Applicative`
class.

> assertion5 = (abc & decode (pure (,,) <*> at 1 <*> at 2 <*> at 3))
>              == ('a', 'b', 'c')

Better than tuples, even, we can use types that are more meaningful to us.

> data XYZ a = XYZ{ x :: a, y :: a, z :: a } deriving Eq

> decodeXYZ :: Decode Int Char (XYZ Char)
> decodeXYZ = pure XYZ <*> at 1 <*> at 2 <*> at 3

And we can use `do` and record sugar to forget about field order.

> decodeXYZ' = do x <- at 1; y <- at 2; z <- at 3; pure XYZ{ x, y, z }

> decodeXYZ'' = do x <- at 1; z <- at 3; y <- at 2; pure XYZ{ x, y, z }

And it is, I think, fair to call it a trick. Because, for one thing, history
implies that this approach wasn't obvious; I've seen nothing predating
*Applicative Programming with Effects* which seems to have started circulating
in 2005, the class appears in GHC's `base` library starting in 2006, and
`ApplicativeDo` doesn't hit the compiler until 2016.

And for another thing, it's sort of weird. Consider the types of the subterms:

> subterm1 = pure (,,)                   :: Decode Int Char (Char -> Char -> Char -> (Char, Char, Char))
> subterm2 = pure (,,) <*> at 0          :: Decode Int Char (        Char -> Char -> (Char, Char, Char))
> subterm3 = pure (,,) <*> at 0 <*> at 1 :: Decode Int Char (                Char -> (Char, Char, Char))

The idea that we have a `Decode` that represents a parser which produces a
result of type `Char -> (Char, Char, Char)` is, frankly, not even what I'm
thinking about when I write an applicative expression. It is only something I
think about when I'm explaining why it works. To me this is enough to place any
technique squarely in the category of clever trick.

The final reason I call applicative programming a trick is that when we step
outside of covariant functors, we find that `Applicative` is no longer
applicable. (Indeed the superclass constraint said this already.)

---

Just like we encode a product of values such as `XYZ` from an input map, so too
may we be interested in the reverse: turning a record into a `Map` by turning
each field into a `Map` and merging all the results together.

(This is not so contrived a topic. The problem posed here is one encountered
whenever we encode and write process environment variables, HTTP headers, JSON
objects, and so on.)

> newtype Encode k v a = Encode{ encode :: a -> Map k v }

> to :: k -> Encode k v v
> to k = Encode \v -> Map.singleton k v

The `to` function can construct an `Encode` definition suitable for each of the
three fields `x`, `y`, `z` of the `XYZ` record.

> assertion6 = ('a' & encode (to 1))  ==  [(1, 'a')]
> assertion7 = ('b' & encode (to 2))  ==  [(2, 'b')]
> assertion8 = ('c' & encode (to 3))  ==  [(3, 'c')]

We can define a 2-tuple combination, much like we did for `Decode`.

> (★★) :: Ord k => Encode k v a -> Encode k v b -> Encode k v (a, b)
> encodeA ★★ encodeB = Encode \(a, b) ->
>     (a & encode encodeA) <>
>     (b & encode encodeB)

> assertion9 = (('a', 'b') & encode (to 1 ★★ to 2))  ==  [(1, 'a'), (2, 'b')]

And we can define an `Encode` for `XYZ`.

> encodeXYZ :: Encode Int a (XYZ a)
> encodeXYZ = Encode \xyz ->
>     (x xyz & encode (to 1)) <>
>     (y xyz & encode (to 2)) <>
>     (z xyz & encode (to 3))

> assertion10 = (XYZ{ x = 'a', y = 'b', z = 'c' } & encode encodeXYZ)
>              == [(1, 'a'), (2, 'b'), (3, 'c')]

What we don't have is a combinator that works directly on `Encode` like `<*>`
does for `Decode`. This may seem like an unimportant quibble, since `encodeXYZ`
was not particularly difficult to define, but it ends up compounding into a
bigger practical problems.

There is also one other concern of more obvious practical relevance:

> encodeXYZ' = Encode \xyz ->
>     (x xyz & encode (to 1)) <>
>     (z xyz & encode (to 3))

If we accidentally neglect to encode one of the fields in the applicative-do
`Decode` expression, the compiler emits a warning. If we omit an `Encode` step,
as in `encodeXYZ'` above, the mistake goes unremarked upon.

---

When a type variable appears in both a covariant and a contravariant way, we
have an *invariant* functor. The contravariant aspect prohibits defining `fmap`,
and the covariant aspect prohibits defining `contramap`.

This happens, for example, when we bundle `Decode` and `Encode` together into one
type, which seems like a reasonable thing to want to do:

> data Codec k v a = Codec{ co :: Encode k v a, dec :: Decode k v a }

> key :: Ord k => k -> Codec k Char Char
> key k = Codec{ co = to k, dec = at k }

We can concoct combinations for invariant functors of products.

> (★★★) :: Ord k => Codec k v a -> Codec k v b -> Codec k v (a, b)
> formatA ★★★ formatB = Codec
>     { dec = dec formatA ★ dec formatB
>     , co = co formatA ★★ co formatB
>     }

Exercise: Define `Format Int Char XYZ` in terms of `key 1`, `key 2`, and `key 3`.

> codecXYZ = Codec{ co = undefined, dec = undefined }

It isn't terribly difficult, but it isn't terribly easy either. What you have to
do is write the `Decode` and the `Encode` separately. They don't compose in
parallel. It's up to you to make sure they're defined harmoniously. It's all
terribly inconvenient.
