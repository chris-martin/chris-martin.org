---
title: Problems with products, part 2
date: 2022 Nov 18
slug: problems-products-2
abstract: Higher-kinded products
---

*You can find this article as a literate Haskell file <a href="${Post.lhs}">here</a>.*

Definitions carried over from part one:

```haskell
newtype Decode k v a = Decode{ decode :: Map k v -> a }
newtype Encode k v a = Encode{ encode :: a -> Map k v }
data Codec k v a = Codec{ co :: Encode k v a, dec :: Decode k v a }
```

I first saw the following technique used with `optparse-applicative`, though I
cannot remember where. Here is a typical-looking type that might represent the
values of all a program's command-line arguments:

```haskell
data Opt1 = Opt1{ verbose :: Bool,
                  file    :: FilePath,
                  jobs    :: Natural }
```

```haskell
opt1 = Opt1{ verbose = True, file = "/tmp/xyz.hs", jobs = 4 }
```

Suppose we wrap each of the three fields in the `Identity` constructor. This, of
course, achieves nothing, apart from giving us something to refer to when we
think about the alteration that follows.

```haskell
data Opt2 = Opt2{ verbose :: Identity Bool,
                  file    :: Identity FilePath,
                  jobs    :: Identity Natural }
```

```haskell
opt2 = Opt2{ verbose = Identity True,
             file    = Identity "/tmp/xyz.hs",
             jobs    = Identity 4 }
```

Now instead of `Identity`, we'll make that a type parameter `f`.

```haskell
data Opt3 f = Opt3{ verbose :: f Bool,
                    file    :: f FilePath,
                    jobs    :: f Natural }
```

```haskell
opt3 = Opt3{ verbose = Identity True,
             file    = Identity "/tmp/xyz.hs",
             jobs    = Identity 4 }
```

Notice that still apparently we've done very little. The definition of `opt3` is
exactly like that of `opt2`, only now its type is `Opt3 Identity` rather than
`Opt2`. But what this shift has done is now let us use type constructors other
than just `Identity`. For example, `Opt.Parser`:

```haskell
opt3Parser :: Opt3 Opt.Parser
opt3Parser =
  Opt3{ verbose = Opt.switch (Opt.short 'v'),
        file    = Opt.strOption (Opt.short 'f'),
        jobs    = Opt.option Opt.auto (Opt.short 'j') }
```

Whereas `Opt3 Identity` consists of three values -- `Bool`, `FilePath`, and
`Natural` -- `Opt3 Opt.Parser` consists of three parsers: an `Opt.Parser Bool`,
an `Opt.Parser FilePath`, and an `Opt.Parser Natural`.

The question is then how to use such a thing, because I do not want three
parsers; I want them combined into one parser that shall have `Opt3 Identity` as
its result type.

```haskell
multiplyParser :: Opt3 Opt.Parser -> Opt.Parser (Opt3 Identity)
multiplyParser = undefined
```

However, it now feels like we have made some progress, because we can at least
*express* the composition of parsers (in a way that generalizes to contravariant
and invariant functors), even if we do not yet have to means to evaluate the
expression.

The parsing example may seem superfluous because `Opt.Parser` is already
`Applicative`, and so there is little need to improve upon the faculties for
composition that it already has. But notice that we can insert other types of
`f` as well, including invariant functors such as `Codec k v` that do not admit
`Applicative` composition.

```haskell
opt3Codec :: Opt3 (Codec Char String)
opt3Codec = Opt3{ verbose = verboseCodec, file = fileCodec, jobs = jobsCodec }
```

```haskell
verboseCodec = charStringCodec 'v' (Iso show (== "True"))
```

```haskell
fileCodec = charStringCodec 'f' (Iso id id)
```

```haskell
jobsCodec = charStringCodec 'k' (Iso show (fromMaybe 0 . readMaybe))
```

```haskell
data Iso a b = Iso (a -> b) (b -> a)
```

```haskell
charStringCodec :: Char -> Iso a String -> Codec Char String a
charStringCodec k (Iso encodeString decodeString) =
    Codec{ co = Encode \v -> Map.singleton k (encodeString v),
           dec = Decode \m -> decodeString (Map.findWithDefault "" k m) }
```

Again, now we have a way to express the product of three codecs, but we are
still missing a function to multiply these three factors into a single codec.

```haskell
multiplyCodec :: Opt3 (Codec k v) -> Codec k v (Opt3 Identity)
multiplyCodec = undefined
```

---

The next time I saw higher-kinded products was Oliver Charles presenting the
`rel8` library, which includes a neat trick that I will include now. This
business of having to insert the `Identity` data constructor into each field
expression when we changed from `Opt1` to `Opt2 Identity` is a bit irksome. The
trick lets us eliminate it.

```haskell
type Factor :: (Type -> Type) -> Type -> Type
type family Factor f a where
    Factor Identity a = a
    Factor f        a = f a
```

```haskell
data Opt4 f = Opt4{ verbose :: Factor f Bool,
                    file    :: Factor f FilePath,
                    jobs    :: Factor f Natural }
```

The family instance `Factor Identity a = a` provides a special handling of the
case where `f = Identity`, stating that this is merely an alias for `a` itself.
The second instance, `Factor f a = f a`, says that for all other `f`,
`Factor f a` should be read as simply `f a`, which is what we had before.

Values of `Opt4 Identity` can now be written without an `Identity` term, exactly
as we wrote `opt1`.

```haskell
opt4 :: Opt4 Identity
opt4 = Opt4{ verbose = True, file = "/tmp/xyz.hs", jobs = 4 }
```

And the codec definitions for `Opt3` and `Opt4` are the same.

```haskell
opt4Codec :: Opt4 (Codec Char String)
opt4Codec = Opt4{ verbose = verboseCodec, file = fileCodec, jobs = jobsCodec }
```

---

Now we must return to how to define the two "multiply" functions.

- `Opt4 Opt.Parser -> Opt.Parser (Opt4 Identity)`
- `Opt4 (Codec k v) -> Codec k v (Opt4 Identity)`

More generally, something that looks like:

```haskell
multiply :: forall ( factors :: (Type -> Type) -> Type )
                   ( functor ::  Type -> Type          ).
            factors functor -> functor (factors Identity)
multiply = undefined
```

In our latter example, the `factors` are `Opt4` (representing the three option
factors `verbose`, `file`, and `jobs`) and the `functor` is `Codec k v`.

But that's a job for another day.
