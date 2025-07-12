module Text.Megaparsec.HTML.Types where

import Data.Text 
import Data.Void
import Text.Megaparsec

type Parser = Parsec Void Text
