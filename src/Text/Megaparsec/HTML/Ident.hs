module Text.Megaparsec.HTML.Ident where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.HTML.Space as S
import Text.Megaparsec.HTML.Types

htmlIdent :: HTMLParser String
htmlIdent = do
    f <- letterChar
    r <- S.lexeme (some (alphaNumChar <|> single '-' <|> single '_'))
    return (f : r)
