module ChrisMartinOrg.MarkupLanguage
  (
  ) where
{-
import ChrisMartinOrg.Html (Html)

import Data.Vector (Vector)

type Document = Vector Block

data ListItem = ListItemInline Inline | ListItemBlocks (Vector Block)

data OrderedListItem = OrderedListItem { olNumber :: Text, olItem :: ListItem }

data Code = Code { codeLang :: Maybe Text, codeBody :: Text }

data Block
  = BlockParagraph Inline
  | BlockBulletList (Vector ListItem)
  | BlockOrderedList (Vector OrderedListItem)
  | BlockCode Code
  | BlockQuote (Vector Block)

{-
    | BlockHtml Text
    | BlockRule
    | BlockHeading Int inline
    | BlockReference Text Text
    | BlockPlainText inline
  deriving (Show, Eq)
-}

data Inline
-}
