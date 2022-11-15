{-# LANGUAGE OverloadedStrings #-}

module ChrisMartinOrg.Home
  ( pageHtml
  ) where

import Prelude hiding (head)

import ChrisMartinOrg.Content (contentToHtml)
import ChrisMartinOrg.Core
import ChrisMartinOrg.Css (styleLink)
import ChrisMartinOrg.Post (postUrl)
import ChrisMartinOrg.PostDate (formatPostDate)

import Data.Foldable (for_)
import Data.String (fromString)
import Text.Blaze.Html5 (Html, toHtml, (!))

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

pageHtml :: Content -> Maybe CompiledCss -> [Post] -> Html
pageHtml content css posts =
    H.docTypeHtml (head <> body)
  where

    head = H.head $ do
      H.meta ! A.charset "utf-8"
      H.meta ! A.content "width=device-width,initial-scale=1"
             ! A.name "viewport"
      H.title "Chris Martin"
      H.link ! A.rel "icon" ! A.href ""
      H.link ! A.href "https://fonts.googleapis.com/css?\
                      \family=Inconsolata|Merriweather"
             ! A.rel "stylesheet"
      H.link ! A.rel "alternate"
             ! A.type_ "application/rss+xml"
             ! A.href "/rss.xml"
      for_ css styleLink

    body = H.body $ do
      globalPageHeader HomePage
      H.main $
        H.div $ do
          H.div ! A.class_ "content" $
            contentToHtml content
          H.div ! A.class_ "container" $ do
            H.h2 "Writings"
            for_ posts postHtml

postHtml :: Post -> Html
postHtml post = H.div ! A.class_ "post" $ do
  H.div ! A.class_ "post-head" $ do
    H.a ! A.class_ "post-title" ! A.href (fromString $ postUrl post) $
      toHtml $ postTitle post
    H.div ! A.class_ "post-date" $
      toHtml $ formatPostDate $ postDate post
  H.div ! A.class_ "post-abstract" $ do
    for_ (postThumb post) $ \t ->
      H.img ! A.class_ "post-thumb" ! A.src (fromString t)
    markdown $ postAbstract post
  H.br
