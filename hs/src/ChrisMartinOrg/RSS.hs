{-# LANGUAGE OverloadedStrings #-}

module ChrisMartinOrg.RSS
  ( rssString
  ) where

import ChrisMartinOrg.Core (Post (..))
import ChrisMartinOrg.Post (postUrl)
import ChrisMartinOrg.PostDate (postDateToUTCTime)

import Network.URI (URI (..), URIAuth (..))
import Text.RSS
import qualified Data.Text as Text

rssString :: [Post] -> String
rssString posts = (showXML . rssToXML) rss
  where
  rss = RSS title link description channels items
  title = "Chris Martin"
  link = uri "/"
  description = "Chris blogs mostly about Haskell and NixOS."
  channels = []
  items = postItem <$> posts

postItem :: Post -> Item
postItem p =
  [ (Title . Text.unpack . postTitle) p
  , (Link . uri . postUrl) p
  , (Description . Text.unpack . postAbstract) p
  , (PubDate . postDateToUTCTime . postDate) p
  ]

uri :: String -> URI
uri path =
  URI
    "https:"
    (Just $ URIAuth "" "chris-martin.org" "")
    ("/" ++ path)
    ""
    ""
