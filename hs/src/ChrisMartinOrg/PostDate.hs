{- |

Defines 'PostDate', the type we use to represent the date of a blog post, and
thing for formatting and parsing these dates.

-}
module ChrisMartinOrg.PostDate
  ( PostDate (..)
  , formatPostDate
  , postDateParser
  , postDateToUTCTime
  ) where

import Control.Applicative (optional)
import Control.Monad (mfilter)
import Data.Attoparsec.Text (Parser, (<?>))
import Data.Char (isLetter)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)

import qualified Data.Attoparsec.Text as A
import qualified Data.Text as Text
import qualified Data.Time.Calendar as Cal
import qualified Data.Time.Format as TimeF
import Data.Time (UTCTime (..))

{- |

Every blog post has at least year-and-month precision. Some posts also have a
specific day of the month.

-}
data PostDate =
  PostDate
    { postDateYear  :: Int
    , postDateMonth :: Int
    , postDateDay   :: Maybe Int
    }
  deriving (Eq, Ord, Show)

postDateToUTCTime :: PostDate -> UTCTime
postDateToUTCTime x = UTCTime (Cal.fromGregorian year month day) 0
  where
    year = (fromIntegral . postDateYear) x
    month = postDateMonth x
    day = (fromMaybe 1 . postDateDay) x

{- |

>>> formatPostDate (PostDate 2008 9 (Just 19))
"2008 September 19"

>>> formatPostDate (PostDate 2008 9 Nothing)
"2008 September"

-}
formatPostDate :: PostDate -> Text
formatPostDate c =
  Text.unwords $ concat [[ yearW, monthW ], maybe [] pure dayWMaybe]
  where
    yearW =
      Text.pack $ show $ postDateYear c
    monthW =
      Text.pack $ fst $
      TimeF.months TimeF.defaultTimeLocale !! ((postDateMonth c) - 1)
    dayWMaybe =
      Text.pack . show <$> postDateDay c

{- |

>>> A.parseOnly postDateParser "2008 Sept 19"
Right (PostDate {postDateYear = 2008, postDateMonth = 9, postDateDay = Just 19})

>>> A.parseOnly postDateParser "2008 Sept"
Right (PostDate {postDateYear = 2008, postDateMonth = 9, postDateDay = Nothing})

-}
postDateParser :: Parser PostDate
postDateParser =
    mfilter postDateValid p <?> "post date"
  where
    p = PostDate <$> (A.decimal <?> "year")
                 <*> (A.skipSpace *> monthParser <?> "month")
                 <*> (A.skipSpace *> optional A.decimal <?> "day")

{- |

>>> A.parseOnly monthParser "Sept"
Right 9

-}
monthParser :: Parser Int
monthParser =
  do
    text <- A.takeWhile1 isLetter
    maybe mempty pure (getMonth text)

{- |

>>> getMonth "Sept"
Just 9

-}
getMonth :: Text -> Maybe Int
getMonth prefix =
  case filter f (zip [1..] months) of
    [(i, _)] -> Just i
    _ -> Nothing
  where
    f (_, x) = Text.toLower prefix `Text.isPrefixOf` x

{- |

>>> head months
"january"

-}
months :: [Text]
months =
  Text.toLower . Text.pack . fst <$> TimeF.months TimeF.defaultTimeLocale

postDateValid :: PostDate -> Bool
postDateValid x =
  isJust (Cal.fromGregorianValid y m d)
  where
    y = fromIntegral (postDateYear x)
    m = postDateMonth x
    d = maybe 1 id (postDateDay x)
