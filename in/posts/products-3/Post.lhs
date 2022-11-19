> module Post where

> import Data.Functor.Identity (Identity (..))
> import Data.Kind (Type)
> import Data.Map (Map)
> import Data.Maybe (fromMaybe)
> import Numeric.Natural (Natural)
> import qualified Data.Map as Map
> import Text.Read (readMaybe)
> import GHC.Generics (Generic, Rep, M1 (..), S, D, C, R, Selector, selName, (:*:)(..), K1 (..))
> import qualified GHC.Generics as Generic
> import Data.Tree (Tree (Node), Forest)
> import qualified Data.Tree as Tree

Definitions carried over from parts one and two:

> newtype Decode k v a = Decode{ decode :: Map k v -> a }
> newtype Encode k v a = Encode{ encode :: a -> Map k v }
> data Codec k v a = Codec{ co :: Encode k v a, dec :: Decode k v a }

> verboseCodec = charStringCodec 'v' (Iso show (== "True"))
> fileCodec = charStringCodec 'f' (Iso id id)
> jobsCodec = charStringCodec 'k' (Iso show (fromMaybe 0 . readMaybe))

> data Iso a b = Iso (a -> b) (b -> a)

> charStringCodec :: Char -> Iso a String -> Codec Char String a
> charStringCodec k (Iso encodeString decodeString) =
>     Codec{ co = Encode \v -> Map.singleton k (encodeString v),
>            dec = Decode \m -> decodeString (Map.findWithDefault "" k m) }

> type Factor :: (Type -> Type) -> Type -> Type
> type family Factor f a where
>     Factor Identity a = a
>     Factor f        a = f a

> data Opt f = Opt{ verbose :: Factor f Bool,
>                   file    :: Factor f FilePath,
>                   jobs    :: Factor f Natural }

> opt1 :: Opt Identity
> opt1 = Opt{ verbose = True, file = "/tmp/xyz.hs", jobs = 4 }

> opt2 :: Opt Identity
> opt2 = Opt{ verbose = True, file = "/run/user/1000/xyz.hs", jobs = 8 }

> optCodec :: Opt (Codec Char String)
> optCodec = Opt{ verbose = verboseCodec, file = fileCodec, jobs = jobsCodec }

I left off with the concrete goal of defining this function ...

> multiplyCodec :: Opt (Codec k v) -> Codec k v (Opt Identity)
> multiplyCodec = undefined

...which I hope to write as a specialization of a more general mechanism that
should take roughly the form of:

> multiply :: forall ( factors :: (Type -> Type) -> Type )
>                    ( functor ::  Type -> Type          ).
>             factors functor -> functor (factors Identity)
> multiply = undefined

I will go ahead and add one more goal, to imitate the full power of what `rel8`
can do: I want an approach that will work for nested records -- for example, to
have a codec for `Opt`-squared:

> data OptDiff f = OptDiff{ old :: Opt f, new :: Opt f }

> diff :: OptDiff Identity
> diff = OptDiff{ old = opt1, new = opt2 }

This complicates matters. We must now consider our domain to be trees, where the
nodes are the `factors` records (`OptDiff` or `Opt`) and the leaves are members
of the `functor` (`Codec k v`, `Opt.Parser`, or `Identity`).

---

First let's demonstrate that Haskell's metaprogramming facilities even give us
the ability to distinguish nodes from leaves. We need to be able to recognize in
the type `OptDiff (Codec k v)` that `new` and `old` are node fields, whereas
`verbose`, `file`, and `jobs` are leaf fields, and the analysis should not
descend further into them to consider the fields `co` and `dec`. My first goal
is to simply print the tree of field names, as evidence that this aim is
distinction possible. The result will have the type `Tree (Maybe String)`, where
the field names are in a `Maybe` context because not all fields have names (only
ones written using record syntax), nor does the tree root.

It's time for some generics.

> deriving instance Generic (Opt f)
> deriving instance Generic (OptDiff f)

The typical pattern for generics is to define two classes: `FactorsForest` will
be the class for the types we're interested in such as `Opt f` and `OptDiff f`,
and `RepFactorsForest` will have instances for `Rep (Opt f)` and
`Rep (OptDiff f)`.

> class FactorsForest f factors | factors -> f
>   where
>     factorsForest :: factors -> Forest (Maybe String)
>
>     default factorsForest ::
>         (Generic factors, RepFactorsForest f (Rep factors)) =>
>         factors -> Forest (Maybe String)
>     factorsForest = repFactorsForest . Generic.from

> class RepFactorsForest f rep | rep -> f where
>     repFactorsForest :: rep x -> Forest (Maybe String)

Whether a field's type has a `FactorsForest` instance is what will distinguish
nodes from leaves. Whether a type's `Rep` has a `RepFactorsForest` instance is
what will determine what shape of datatype can have a generically-derived
`FactorsForest` instance. Since `factorsForest` has a default method
implementation wherever a `RepFactorsForest` instance is present, the only thing
required to mark a type as a node is an empty instance declaration:

> instance FactorsForest (Codec k v) (Opt (Codec k v))
> instance FactorsForest (Codec k v) (OptDiff (Codec k v))

> instance FactorsForest Identity (Codec k v a) where
>     factorsForest _ = []

> printFactorsForest :: FactorsForest f x => x -> IO ()
> printFactorsForest = putStr . Tree.drawForest . fmap (fmap (fromMaybe "?")) . factorsForest

---

% Additionally, a tree class will be required at the representation layer only, to
% describe fields. The root of the tree here is the field name. There is no
% corresponding class at the type layer because a field itself (or "selector", in
% generics parlance) does not correspond to a type.

% > class RepFactorsTree rep where
% >     repFactorsTree :: rep x -> Tree (Maybe String)

% For a selector (`S`), if the type of the field has a forest, then we construct a
% tree by taking the name of the selector as the root, and the forest of the type
% as its subforest.

% > instance (Selector selector, RepFactorsForest content) =>
% >    RepFactorsTree (M1 S selector content)
% >  where
% >     repFactorsTree selector@(Generic.M1 content) =
% >         Node
% >           (Just (Generic.selName selector))  -- name of the selector
% >           (repFactorsForest content)         -- subforest

For a selector (`S`), if the type of the field has a forest, then we construct a
forest with a single tree whose root is the name of the selector, and whose
subforest comes from the type of the field.

> instance (Selector selector, RepFactorsForest f content) =>
>    RepFactorsForest f (M1 S selector content)
>  where
>     repFactorsForest selector@(Generic.M1 content) =
>         [ Node
>             (Just (Generic.selName selector))  -- name of the selector
>             (repFactorsForest content)         -- subforest
>         ]

...

> instance (RepFactorsForest f a, RepFactorsForest f b) =>
>     RepFactorsForest f (a :*: b)
>   where
>     repFactorsForest (a :*: b) = repFactorsForest a <> repFactorsForest b

`K1 R` kicks us back around from the representation level to the type level. `x`
below is not a representation but an actual value.

> instance FactorsForest f content => RepFactorsForest f (K1 R content) where
>     repFactorsForest (K1 x) = factorsForest x

% > instance {-# overlaps #-} RepFactorsForest (K1 R content) where
% >     repFactorsForest _ = []

Datatype and constructor (`D` and `C`) metadata are irrelevant, so we just skip
down through to their content.

> instance RepFactorsForest f content =>
>     RepFactorsForest f (M1 D datatype content)
>   where
>     repFactorsForest (M1 content) = repFactorsForest content

> instance RepFactorsForest f content =>
>     RepFactorsForest f (M1 C constructor content)
>   where
>     repFactorsForest (M1 content) = repFactorsForest content
