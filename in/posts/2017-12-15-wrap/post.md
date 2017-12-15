--------------------------------------------------------------------------------
title:    Line wrap
date:     2017 Dec 15
slug:     line-wrap
abstract: ...
--------------------------------------------------------------------------------

Last night I let myself be driven to anger by some old [Uncle Bob][bob] writing, and I had to write a retort.

  [bob]: https://thecleancoder.blogspot.ca/2010/10/craftsman-62-dark-path.html

1. The process involves intentionally writing code that is wrong. Maybe this is supposed to be a way to overcome writer's block, I don't know, but I cannot be convinced that the path to correctness is anything other than writing correct code at every step of the way. I know that it's extremely popular to make broken things and then fix them, but I believe we can do so much better.

2. The solution fails to break the problem into smaller, more understandable pieces. This is the source of Alphonse's difficulty in writing accurate tests; they are trying to think about too many things at once.

---

Write down the type of the thing you're trying to define.

```haskell
wrap :: Natural -> Text -> Text
```

`wrap column input` will produce `input` with line breaks inserted so that no
line is longer than `column`, preferring to break lines at word boundaries.

Consider whether there is anything we can do to break the problem into smaller pieces that can be solved separately. I think there's one candidate that seems good here: Let's try breaking the input into its component lines, wrapping each line, and then putting the result back together. The way that one line is wrapped shouldn't affect the way another line is wrapped, so the problem cleanly reduces to wrapping a single line at a time, which seems easier to think about.

```haskell
wrap column = splitOn "\n" >>> fmap (wrapOneLine column) >>> intercalate "\n"

wrapOneLine :: Natural -> Text -> Text
```

`wrapOneLine` is the same as `wrap`, but the `input` argument may not contain any line breaks.

Is there another way to further subdivide the input? We should probably try splitting it on word boundaries, since that's the only aspect of the string's structure that we care about.

```haskell
tokenize :: Text -> [Text]
```

`tokenize` splits a string on word boundaries.

A place where we're comfortable inserting a line break is a place that is either before or after a space (or both; breaking between two spaces seems acceptable).

```haskell
tokenize = groupBy (\x y -> not (isSpace x || isSpace y))
```

It isn't entirely obvious to me that what we just wrote is correct, so let's test it:

```haskell
>>> tokenize "abc def  gh"
["abc"," ","def"," "," ","gh"]
```

Looks good. Notice how each space character is its own separate token, even where there were two spaces in a row.

Now I think we're going to need a function that packs as many tokens as possible into one line without breaking any.

```haskell
type Packing = (Text, [Text])

pack :: Natural -> NonEmpty Text -> Maybe Packing
```

`pack column tokens` returns the concatenation of as many tokens as possible without exceeding `column` characters, and a list of the remaining unconsumed tokens. If it is not possible to pack *any* tokens, we will return `Nothing`.

To implement this, let's start by writing a function that enumerates *all* of the ways to pack tokens, regardless of maximum column length.

```haskell
packings :: NonEmpty Text -> [Packing]
packings tokens = f (empty, toList tokens)
  where
    f :: Packing -> [Packing]
    f (_, []) = []
    f (packed, nextToken : remainingTokens) = packing : f packing
      where packing = (append packed nextToken, remainingTokens)
```

Again this is probably something that we need to test to make sure what we just wrote makes sense.

```haskell
>>> packings ["a", "b"]
[("a",["b"]),("ab",[])]

Now let's implement `pack` by selecting, among the possible packings, the longest one that fits within the column length.

```haskell
pack column = packings >>> foldr f Nothing
  where
    f :: Packing -> Maybe Packing -> Maybe Packing
    f x@(packed, _) Nothing | fromIntegral (length packed) <= column = Just x
    f _ y = y
```

This one calls for a whole battery of tests, to cover all of the cases.

Where *all* of the tokens fit onto the line:

```haskell
>>> pack 3 ["a", "bb"]
Just ("abb",[])
```

Where *not all* of the tokens fit onto the line:

```haskell
>>> pack 4 ["a", "bb", "cc"]
Just ("abb",["cc"])
```

Where *no* tokens fit onto the line:

```haskell
>>> pack 2 ["abc"]
Nothing
```

-- In the last case, we're allowed to resort to breaking a token. So let's write
-- a modified version of the `pack` function that breaks a token if necessary,
-- thus eliminating `Maybe` from the codomain.

pack' :: Natural -> NonEmpty Text -> Packing
pack' column tokens@(firstToken :| remainingTokens) =
  pack column tokens & fromMaybe break
  where
    break = splitAt (fromIntegral column) firstToken & fmap (: remainingTokens)

-- | >>> pack' 2 ["abc", "d"]
-- ("ab",["c","d"])

-- Now that we know how to pack a single line full of as many tokens as it can
-- hold, we can do that recursively to fill a bunch of lines.

wrapTokens :: Natural -> [Text] -> [Text]
wrapTokens column tokens =
  case nonEmpty tokens of
    Nothing -> []
    Just tokens' ->
      let (line, moreTokens) = pack' column tokens'
      in  line : wrapTokens column moreTokens

-- Let's just reuse the same test cases we used for `pack`.

-- | >>> wrapTokens 3 ["a", "bb"]
-- ["abb"]

-- | >>> wrapTokens 4 ["a", "bb", "cc"]
-- ["abb","cc"]

-- | >>> wrapTokens 2 ["abc"]
-- ["ab","c"]

-- Now we can finally get back to implementing `wrapOneLine`.

wrapOneLine column = tokenize >>> wrapTokens column >>> intercalate "\n"

-- So now the `wrap` function ought to work.

-- | >>> wrap 3 "one two three"
-- "one\n \ntwo\n \nthr\nee"

-- | >>> wrap 4 "one two three"
-- "one \ntwo \nthre\ne"

-- | >>> wrap 5 "one two three"
-- "one \ntwo \nthree"

-- | >>> wrap 5 "one   two three"
-- "one  \n two \nthree"


```haskell
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Arrow      ((>>>))
import Data.Bool          (not, (||))
import Data.Char          (isSpace)
import Data.Function      ((&))
import Data.Functor       (fmap, ($>))
import Data.List          (foldr)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty,
                           toList)
import Data.Maybe         (Maybe (..), fromMaybe)
import Data.Ord           ((<=))
import Data.Text.Lazy     (Text, append, empty,
                           groupBy, intercalate,
                           length, splitAt, splitOn)
import Numeric.Natural    (Natural)
import Prelude            (fromIntegral, undefined)
```
