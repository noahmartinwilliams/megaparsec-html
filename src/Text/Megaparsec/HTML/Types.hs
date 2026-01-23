module Text.Megaparsec.HTML.Types where

import Data.Void
import Data.Map
import Text.Megaparsec
import Text.Megaparsec.CSS as CSS
import Text.Megaparsec.JS as JS

type HTMLParser = Parsec Void String

data Tag = CSSNode String (Map String String) [CSS.RuleSet] | JSNode String (Map String String) JS.Doc | Node String (Map String String) [Tag]  deriving(Show, Eq)

data DTD = DTD String String deriving(Show, Eq)

data Doc = Doc DTD Tag deriving(Show, Eq)
