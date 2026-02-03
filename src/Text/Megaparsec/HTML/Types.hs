module Text.Megaparsec.HTML.Types where

import Data.Map
import Data.Tree
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.CSS as CSS
import Text.Megaparsec.JS as JS

type HTMLParser = Parsec Void String

data Tag = NullTag | CSSNode String (Map String String) CSSDoc | JSNode String (Map String String) JS.Doc | Node String (Map String String) | TextNode String deriving(Show, Eq)

data DTD = DTD String String deriving(Show, Eq)

data Doc = Doc DTD (Tree Tag) deriving(Show, Eq)
