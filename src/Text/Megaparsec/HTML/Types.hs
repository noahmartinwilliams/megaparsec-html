module Text.Megaparsec.HTML.Types where

import Data.Text 
import Data.Void
import Data.Map
import Text.Megaparsec
import Text.Megaparsec.JS

type Parser = Parsec Void Text

data Tag = JSNode Text (Map Text Text) Doc | Node Text (Map Text Text) [Tag]  deriving(Show, Eq)
