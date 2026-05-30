module Text.Megaparsec.HTML.Types where

import Data.Map
import Data.Tree
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.CSS as CSS
import Text.Megaparsec.JS as JS
import Text.Megaparsec.JSON as JSON

type HTMLParser = Parsec Void String

data Tag = NullTag | 
    CSSNode String (Map String String) CSSDoc | 
    JSNode String (Map String String) (Maybe JS.Doc) | 
    JSONNode String (Map String String) JSONObj | 
    Node String (Map String String) | 
    TextNode String deriving(Show, Eq)

data DTD = DTD (Maybe String) (Maybe String) deriving(Show, Eq)

data Doc = Doc (Maybe DTD) (Tree Tag) deriving(Show, Eq)
