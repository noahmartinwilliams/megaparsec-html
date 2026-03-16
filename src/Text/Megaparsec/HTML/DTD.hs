module Text.Megaparsec.HTML.DTD where

import Control.Monad
import Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.HTML.Types
import Text.Megaparsec.HTML.Space as S
import Text.Megaparsec.HTML.Tags

htmlDTD :: HTMLParser DTD
htmlDTD = do
    void $ S.lexeme (string "<!")
    void $ S.lexeme ((string "DOCTYPE") <|> (string "doctype"))
    void $ hspace
    void $ S.lexeme (string "html")
    void $ hspace
    void $ S.lexeme (optional ((string "PUBLIC")))
    void $ hspace
    first <- optional (htmlString)
    void $ hspace
    second <- optional (htmlString)
    void $ hspace
    void $ (single '>')
    void $ space
    return (DTD first second)
