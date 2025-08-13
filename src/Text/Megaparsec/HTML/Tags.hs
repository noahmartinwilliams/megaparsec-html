{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Text.Megaparsec.HTML.Tags where

import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Data.Void
import Data.Text as T
import Data.Map
import Text.Megaparsec.HTML.Space as S
import Text.Megaparsec.HTML.Types 

htmlBeginTag :: Parser (String, [(Text, Text)])
htmlBeginTag = do
    void $ single '<'
    notFollowedBy (single '/')
    name <- some alphaNumChar
    attrs <- S.lexeme htmlAttrs
    void $ S.lexeme (single '>')
    return (name, attrs)

htmlEndTag :: String -> Parser ()
htmlEndTag name = do
    void $ S.lexeme ( single '<')
    void $ single '/'
    void $ S.lexeme (string (T.pack name))
    void $ S.lexeme (single '>')

htmlSingleTag :: Parser Tag
htmlSingleTag = do
    void $ S.lexeme (single '<')
    name <- S.lexeme (some alphaNumChar)
    attrs <- htmlAttrs 
    let attrs2 = Data.Map.fromList attrs
    void $ S.lexeme (string (T.pack "/>"))
    return (Node (T.pack name) attrs2 [])

htmlAttr :: Parser (Text, Text)
htmlAttr = do
    name <- S.lexeme (some alphaNumChar)
    void $ S.lexeme (single '=')
    val <- S.lexeme (htmlString)
    return ((T.pack name), val)

htmlString :: Parser Text
htmlString = do
    str <- char '"' *> manyTill charInString (char '"') 
    return (T.pack str) where
        charInString = try (char '\\' *> escapedChar) <|> satisfy (/= '"')
        escapedChar = 
            (char 'n' >> return '\n') <|>
            (char 't' >> return '\t') <|>
            (char '"' >> return '"' ) <|>
            (char '\\' >> return '\\')

htmlAttrs :: Parser [(Text, Text)]
htmlAttrs = many htmlAttr

htmlNode :: Parser Tag
htmlNode = do
    (name, attrs) <- try htmlBeginTag
    nodes <- many (try (htmlNode <|> htmlSingleTag))
    void $ htmlEndTag name
    return (Node (T.pack name) (Data.Map.fromList attrs) nodes)
