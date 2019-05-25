{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module ChrisMartinOrg.Post.Page
  ( Input(..)
  , html
  ) where

import Prelude hiding (head)

import ChrisMartinOrg.Core
import ChrisMartinOrg.Css (styleLink)
import ChrisMartinOrg.PostDate (PostDate, formatPostDate)

import Control.Monad (forM_)
import Data.Semigroup ((<>))
import Data.Text (Text)
import System.FilePath.Posix ((</>))
import Text.Blaze.Html5 (Html, toHtml, (!))

import qualified Text.Blaze as Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.Text as Text

data Input = Input
  { inputTitle    :: Html
  , inputPostDate :: PostDate
  , inputCss      :: Maybe CompiledCss
  , inputBody     :: Html
  , inputMeta     :: [(Text, Text)]
  }

html :: Input -> Html
html Input{..} =
    H.docTypeHtml (head <> body)
  where

    head = H.head $ do
      H.meta ! A.charset "utf-8"

      H.meta ! A.name "viewport"
             ! A.content "width=device-width,initial-scale=1"

      H.title inputTitle

      -- Favicon
      H.link ! A.rel "icon" ! A.href ""

      -- Stylesheets
      H.link ! A.href "https://fonts.googleapis.com/css?\
                      \family=Inconsolata|Merriweather"
             ! A.rel "stylesheet"
      mapM_ (styleLink . CompiledCss . (".." </>) . compiledCssPath) inputCss

      -- Miscellaneous other meta tags
      forM_ inputMeta $ \(name, content) ->
          H.meta ! A.name (Blaze.toValue name)
                 ! A.content (Blaze.toValue content)

    body = H.body $ do
      globalPageHeader PostPage
      H.main $
        H.div $ do
          H.div ! A.class_ "post-head container" $ do
            H.h1 ! A.class_ "post-title" $ do
              inputTitle
            H.div ! A.class_ "post-date" $
              toHtml $ formatPostDate inputPostDate
          H.div ! A.class_ "post-body" $ do
            inputBody
      H.div ! A.class_ "post-footer container" $
        H.p $ do
          H.toHtml (Text.pack "I write about Haskell and related topics; you can find my works online on ")
          H.a ! A.href "http://typeclasses.com" $
            H.toHtml (Text.pack "Type Classes")
          H.toHtml (Text.pack " and in print from ")
          H.a ! A.href "http://joyofhaskell.com" $
            H.span ! A.style "text-decoration: italic;" $
              H.toHtml (Text.pack "The Joy of Haskell")
          H.toHtml (Text.pack ".")
