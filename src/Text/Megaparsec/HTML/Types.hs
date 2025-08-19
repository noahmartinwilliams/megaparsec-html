module Text.Megaparsec.HTML.Types where

import Data.Text 
import Data.Void
import Data.Map
import Text.Megaparsec
import Text.Megaparsec.JS as JS

type Parser = Parsec Void Text

data Tag = JSNode Text (Map Text Text) JS.Doc | Node Text (Map Text Text) [Tag]  deriving(Show, Eq)

data DTD = DTD Text Text deriving(Show, Eq)

data Doc = Doc DTD Tag deriving(Show, Eq)
