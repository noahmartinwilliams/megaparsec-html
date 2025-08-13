module Text.Megaparsec.HTML.Types where

import Data.Text 
import Data.Void
import Data.Map
import Text.Megaparsec

type Parser = Parsec Void Text

data Tag = Node Text (Map Text Text) [Tag]  deriving(Show, Eq)
