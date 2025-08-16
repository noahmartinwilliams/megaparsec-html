{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Text.Megaparsec.HTML.Tags where

import Control.Monad
import Control.Monad.State
import Data.Either
import Data.Map
import Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Text.Megaparsec.HTML.Space as S
import Text.Megaparsec.HTML.Types as HTML
import Text.Megaparsec.JS as JS

htmlBeginTag :: HTML.Parser (String, [(Text, Text)])
htmlBeginTag = do
    void $ single '<'
    notFollowedBy (single '/')
    name <- S.lexeme (some alphaNumChar)
    attrs <- S.lexeme htmlAttrs
    void $ S.lexeme (single '>')
    return (name, attrs)

htmlEndTag :: String -> HTML.Parser ()
htmlEndTag name = do
    void $ S.lexeme ( single '<')
    void $ single '/'
    void $ S.lexeme (string (T.pack name))
    void $ S.lexeme (single '>')

htmlSingleTag :: HTML.Parser Tag
htmlSingleTag = do
    void $ S.lexeme (single '<')
    name <- S.lexeme (some alphaNumChar)
    attrs <- htmlAttrs 
    let attrs2 = Data.Map.fromList attrs
    void $ S.lexeme (string (T.pack "/>"))
    return (Node (T.pack name) attrs2 [])

htmlAttr :: HTML.Parser (Text, Text)
htmlAttr = do
    name <- S.lexeme (some alphaNumChar)
    void $ S.lexeme (single '=')
    val <- S.lexeme (htmlString)
    return ((T.pack name), val)

htmlString :: HTML.Parser Text
htmlString = do
    str <- char '"' *> manyTill charInString (char '"') 
    return (T.pack str) where
        charInString = try (char '\\' *> escapedChar) <|> satisfy (/= '"')
        escapedChar = 
            (char 'n' >> return '\n') <|>
            (char 't' >> return '\t') <|>
            (char '"' >> return '"' ) <|>
            (char '\\' >> return '\\')

htmlAttrs :: HTML.Parser [(Text, Text)]
htmlAttrs = many htmlAttr

htmlEmbeddedJS :: HTML.Parser (JS.Doc, Text.Megaparsec.State Text Void)
htmlEmbeddedJS = do
    i <- getInput
    let (jsr, _) = runState (runParserT jsDoc "<inline javascript>" i) JS.jsInitialState
    let (Right (jsd, st@(Text.Megaparsec.State { stateInput = sti}))) = jsr
    let (Text.Megaparsec.State { stateOffset = so }) = st
    setInput sti
    return (jsd, st)

htmlNode :: HTML.Parser Tag
htmlNode = do
    (name, attrs) <- try htmlBeginTag
    if name == "script"
    then do
        (jsd, _) <- htmlEmbeddedJS
        void $ htmlEndTag name
        return (JSNode (T.pack name) (Data.Map.fromList attrs) jsd)
    else do
        nodes <- many (try (htmlNode <|> htmlSingleTag))
        void $ htmlEndTag name
        return (Node (T.pack name) (Data.Map.fromList attrs) nodes)
