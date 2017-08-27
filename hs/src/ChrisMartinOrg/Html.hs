{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module ChrisMartinOrg.Html
  ( Html (..)
  , renderDocument
  ) where

import Data.Map (Map)
import Data.MonoTraversable (ofoldMap, omap, onull)
import Data.Semigroup ((<>))
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Vector (Vector)

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified HTMLEntities.Text

newtype Html v a =
  Html (v (Node v a))

data Node v a
  = TextNode a
  | ElementNode (Element v a)

data Element v a =
  Element (Tag v a) (ElementBody v a)

data ElementBody v a
  = VoidElement
      -- ^ Examples: @br@, @hr@, @img@, @input@, @link@, @meta@
  | RawTextElement a
      -- ^ Examples: @script@, @style@
  | NormalElement (Html v a)
      -- ^ Examples: @html@, @head@, @body@, @p@, @a@, @ol@, @ul@, @li@, @title@

data Tag v a =
  Tag TagName (Attrs v a) RenderStyle

newtype TagName =
  TagName Text

newtype Attrs v a =
  Attrs (v (Attribute a))

data RenderStyle =
  Inline | Block

data Attribute a =
  Attribute Text (Maybe a)

data Indent a =
  Indent a Int

renderDocument :: Foldable v => Indent Text -> Html v Text -> Text
renderDocument indent html =
  "<!DOCTYPE html>\n" <> renderHtml indent html

{- |

>>> renderHtml (Indent 0) $ Html []
""

>>> renderHtml (Indent 0) $ Html [TextNode "hello"]
"hello"

>>> head = Html []

>>> print $ renderHtml (Indent 0) $ Html []
<html>
  <head>
    <title>Hello</title>
    <link rel="stylesheet" type="text/css" href="hello.css">
    <meta charset="utf-8">
  </head>
  <body>
    This is some <em>text</em>.
    <p>This is some text in a <em>parahraph</em>.</p>
    <ul>
      <li>One</li>
      <li>
        <p>Two, in a paragraph.</p>
      </li>
      <li>Three</li>
    </ul>
  </body>
</html>

-}
renderHtml :: Foldable v => Indent Text -> Html v Text -> Text
renderHtml indent (Html nodes) =
  foldMap (renderNode indent) nodes

{-
renderNode :: Foldable v => Indent -> Node v Text -> Text
renderNode indent (TextNode text) =
  renderElement indent el
renderNode _ (TextNode text) =
  text
-}

{-
renderElement :: Foldable v => Indent -> Element v Text -> Text
renderElement _ (Element tag attrs Block VoidElement) =
  "<" <> tag <> renderAttrs attrs <> ">"
renderElement indent (Element tag attrs _ (RawTextElement content)) =
  renderBlockTagOpen indent tag attrs <>
  indentText (indent + Indent 1) content <>
  renderBlockTagClose indent tag
renderElement indent (Element tag _attrs Block (NormalElement Block content)) =
  renderBlockTagOpen indent tag attrs <>
  renderBlockTagClose indent tag
-}

{- |

>>> renderBlockTagOpen (Indent "\t" 2) (TagName "td") (attrList [Attribute "class" (Just "highlight"), Attribute "colspan" (Just "2")])
"\t\t<td class=\"highlight\" colspan=\"2\">\n"

-}
renderBlockTagOpen :: Foldable v => Indent Text -> TagName -> v (Attribute Text) -> Text
renderBlockTagOpen indent (TagName tagName) attrs =
  renderIndent indent <> "<" <> tagName <> renderAttrs attrs <> ">\n"

{- |

>>> renderBlockTagClose (Indent "\t" 2) (TagName "td")
"\t\t</td>\n"

-}
renderBlockTagClose :: Indent Text -> TagName -> Text
renderBlockTagClose indent (TagName tagName) =
  renderIndent indent <> "</" <> tagName <> ">\n"

{- |

>>> indentText (Indent "\t" 2) "One\n\tTwo\n\nThree\n"
"\t\tOne\n\t\t\tTwo\n\n\t\tThree\n"

-}
indentText :: Indent Text -> (Text -> Text)
indentText indent =
    Text.unlines . omap f . Text.lines
  where
    f t = if onull t then t else (renderIndent indent <> t)

renderIndent :: Indent Text -> Text
renderIndent (Indent tab indent) =
  Text.replicate indent tab

{- |

>>> renderAttrs (Vector.fromList [Attribute "type" (Just "checkbox"), Attribute "checked" Nothing])
" type=\"checkbox\" checked"

>>> renderAttrs (Vector.fromList [])
""

-}
renderAttrs :: Foldable v => v (Attribute Text) -> Text
renderAttrs attrs =
  foldMap renderAttr attrs

{- |

>>> renderAttr (Attribute "href" (Just "index.html"))
" href=\"index.html\""

>>> renderAttr (Attribute "checked" Nothing)
" checked"

-}
renderAttr :: Attribute Text -> Text
renderAttr (Attribute name value) =
  " " <> name <> maybe "" renderAttrValue value

{- |

>>> renderAttrValue "4"
"=\"4\""

>>> renderAttrValue "The \"final\" countdown"
"=\"The &quot;final&quot; countdown\""

-}
renderAttrValue :: Text -> Text
renderAttrValue v =
  "=\"" <> HTMLEntities.Text.text v <> "\""
