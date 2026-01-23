{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Text.Megaparsec.HTML.Space where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.HTML.Types
import qualified Text.Megaparsec.Char.Lexer as L

sc :: HTMLParser ()
sc = L.space space1 empty empty

lexeme :: HTMLParser a -> HTMLParser a
lexeme = L.lexeme sc
