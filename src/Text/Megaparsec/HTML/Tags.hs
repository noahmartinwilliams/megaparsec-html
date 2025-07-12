{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Text.Megaparsec.HTML.Tags where

import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Data.Void
import Data.Text as T
import Text.Megaparsec.HTML.Space as S
import Text.Megaparsec.HTML.Types 

htmlBeginTag :: Parser String
htmlBeginTag = do
    void $ single '<'
    notFollowedBy (single '/')
    name <- some alphaNumChar
    void $ S.lexeme (single '>')
    return name

htmlEndTag :: String -> Parser ()
htmlEndTag name = do
    void $ S.lexeme ( single '<')
    void $ single '/'
    void $ S.lexeme (string (T.pack name))
    void $ S.lexeme (single '>')
