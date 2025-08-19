module Text.Megaparsec.HTML.DTD where

import Control.Monad
import Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Text.Megaparsec.HTML.Types
import Text.Megaparsec.HTML.Space as S
import Text.Megaparsec.HTML.Tags

htmlDTD :: Parser DTD
htmlDTD = do
    void $ S.lexeme (string (T.pack "<!"))
    void $ S.lexeme (string (T.pack "DOCTYPE"))
    void $ S.lexeme (string (T.pack "html"))
    void $ S.lexeme (string (T.pack "PUBLIC"))
    first <- S.lexeme (htmlString)
    second <- S.lexeme (htmlString)
    void $ S.lexeme (single '>')
    return (DTD first second)
