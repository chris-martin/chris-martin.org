--------------------------------------------------------------------------------
title:    Line wrap
date:     2017 Dec 15
slug:     line-wrap
abstract: How I prefer to design compositionally, and why I dislike TDD.
--------------------------------------------------------------------------------

Last night I let myself be driven to anger by some old [Uncle Bob][bob] writing, and I had to write a retort.

  [bob]: https://thecleancoder.blogspot.ca/2010/10/craftsman-62-dark-path.html

1. This "TDD" process involves intentionally writing code that is wrong. Maybe this is supposed to be a way to overcome writer's block, I don't know, but I cannot be convinced that the path to correctness is anything other than writing correct code at every step of the way. I know that it's extremely popular to make broken things and then fix them, but I believe we can do so much better.

2. The solution fails to break the problem into smaller, more understandable pieces. This is the source of Alphonse's difficulty in writing accurate tests; they are trying to think about too many things at once.

## Composition-driven design

Write down the type of the thing you're trying to define.

```haskell
wrap :: Natural  -- column
     -> Text     -- input
     -> Text
```

`wrap column input` will produce `input` with line breaks inserted so that no
line is longer than `column`, preferring to break lines at word boundaries.

Consider whether there is anything we can do to break the problem into smaller pieces that can be solved separately. There's a useful observation we can make here: The way that one line is wrapped doesn't affect the way another line is wrapped. So we can break the input into its component lines, wrap each line, and then put the result back together. This seems like a quite clean reduction from our original problem to an easier one.

```haskell
wrap column =
  splitOn "\n" >>>
  fmap (wrapOneLine column) >>>
  intercalate "\n"

wrapOneLine :: Natural  -- column
            -> Text     -- input
            -> Text
```

`wrapOneLine` is the same as `wrap`, but the `input` argument may not contain any line breaks.

Is there another way to further subdivide the input? We should probably try splitting it on word boundaries, since the word boundary positions are the only aspect of the string's structure that we care about.

```haskell
tokenize :: Text -> [Text]
```

A place where we're comfortable inserting a line break is a place that is either before or after a space (or both; breaking between two spaces seems acceptable).

```haskell
tokenize = groupBy
  (\x y -> not (isSpace x || isSpace y))
```

It isn't entirely obvious to me that what we just wrote is correct, so let's test it:

```haskell
>>> tokenize "abc def  gh"
["abc"," ","def"," "," ","gh"]
```

Looks good. Notice how each space character is its own separate token, even where there were two spaces in a row, because it's fine to insert a line break at any point among a group of contiguous spaces.

Now I think we're going to need a function that packs as many tokens as possible into one line. We'll refer to the result of this operation as a "packing," which consists of the line we constructed and the remaining tokens that we didn't use.

```haskell
type Packing =
  ( Text    -- tokens packed into a line
  , [Text]  -- remaining tokens that didn't fit
  )

pack :: Natural        -- column
     -> NonEmpty Text  -- tokens
     -> Maybe Packing
```

`pack column tokens` is the concatenation of as many tokens as possible without exceeding `column` characters, and a list of the remaining unconsumed tokens. If it is not possible to pack *any* tokens, this will evaluate to `Nothing`.

To implement this, let's start by writing a function that enumerates *all* of the ways to pack tokens, regardless of maximum column length.

```haskell
packings :: NonEmpty Text -> [Packing]
packings tokens = f (empty, toList tokens)
  where
    f :: Packing -> [Packing]
    f (_, []) = []
    f (packed, nextToken : remainingTokens) =
      packing : f packing
      where
        packing = ( append packed nextToken
                  , remainingTokens )
```

Again this is probably something that we need to test to make sure what we just wrote makes sense.

```haskell
>>> packings ["a", "b"]
[("a",["b"]),("ab",[])]
```

Now let's implement `pack` by selecting, among the possible packings, the longest one that fits within the column length.

```haskell
pack column = packings >>> foldr f Nothing
  where
    f :: Packing -> Maybe Packing -> Maybe Packing
    f x@(packed, _) Nothing
      | fromIntegral (length packed) <= column = Just x
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

In the last case, we're allowed to resort to breaking a token. So let's write a variant of the `pack` function that breaks a token if necessary, thus *always* giving us a packing (hence we have eliminated the `Maybe` from the result type).

```haskell
pack' :: Natural -> NonEmpty Text -> Packing
pack' column tokens@(firstToken :| remainingTokens) =
  pack column tokens & fromMaybe break
  where
    break = splitAt (fromIntegral column) firstToken
      & fmap (: remainingTokens)
```

I'm pretty sure we don't need to test all of the conditions we tested for `pack`, so let's just check the token-breaking aspect that `pack'` introduced.

```haskell
>>> pack' 2 ["abc", "d"]
("ab",["c","d"])
```

Now that we know how to pack a single line full of as many tokens as it can hold, we can do that recursively to fill a bunch of lines.

```haskell
wrapTokens :: Natural -> [Text] -> [Text]
wrapTokens column tokens =
  case nonEmpty tokens of
    Nothing -> []
    Just tokens' ->
      let (line, moreTokens) = pack' column tokens'
      in  line : wrapTokens column moreTokens
```

Let's just reuse the same test cases we used for `pack`.

```haskell
>>> wrapTokens 3 ["a", "bb"]
["abb"]

>>> wrapTokens 4 ["a", "bb", "cc"]
["abb","cc"]

>>> wrapTokens 2 ["abc"]
["ab","c"]
```

Now we have enough components finished to finally get back to implementing `wrapOneLine`. We'll split the line into tokens, group the tokens into lines, and then glue the lines together with a newline character.

```haskell
wrapOneLine column =
  tokenize >>>
  wrapTokens column >>>
  intercalate "\n"
```

So now... what were we doing here? Oh yeah, the `wrap` function. It ought to work now.

```haskell
>>> wrap 3 "one two three"
"one\n \ntwo\n \nthr\nee"

>>> wrap 4 "one two three"
"one \ntwo \nthre\ne"

>>> wrap 5 "one two three"
"one \ntwo \nthree"

>>> wrap 5 "one   two three"
"one  \n two \nthree"
```

And it does.

## Reflection

I tried to let this post fairly accurately represent my thought process; the ordering reflects the order in which I wrote the code. A few observations:

1. I write functions before writing tests. The process of implementation helps you know what sorts of test cases are worth checking, versus which things are pretty obviously right.

2. Sometimes I take a top-down approach: First write the function that combines the smaller pieces, and then implement the smaller pieces. Sometimes I take a bottom-up approach: First write small pieces that seem like they will be useful, and then figure out how to use them. I don't think either is necessarily more appropriate; I think you should start with whatever occurs to you first.

3. I proceeded in a very linear fashion. I didn't go back and rewrite anything, and I didn't do any whole-program debugging. Once each function was written and tested, it was done.

4. I defined seven top-level terms; Alphonse wrote only one, and at the end refactored it into two. It isn't apparent that more is better, but generally I find that to be the case. It signals a more granular breakdown of the problem, which corresponds to comprehensibility, testability, and generality.

## Appendix

For completeness, here is the full lists of extensions and imports I used:

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
