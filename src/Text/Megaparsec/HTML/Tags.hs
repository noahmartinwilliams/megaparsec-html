{-# LANGUAGE FlexibleContexts #-}
module Text.Megaparsec.HTML.Tags where

import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Data.Void

htmlBeginTag :: (MonadParsec Void s m, Token s ~ Char) => m String
htmlBeginTag = do
    void $ single '<'
    notFollowedBy (single '/')
    name <- some alphaNumChar
    void $ single '>'
    return name
