{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Text.Megaparsec.HTML.Tags where

import Control.Monad
import Control.Monad.State
import Data.Either
import Data.Functor.Identity
import Data.Map
import Data.Set
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Text.Megaparsec.CSS as CSS
import Text.Megaparsec.HTML.Space as S
import Text.Megaparsec.HTML.Types as HTML
import Text.Megaparsec.JS as JS

htmlBeginTag :: HTMLParser (String, [(String, String)])
htmlBeginTag = do
    void $ single '<'
    void $ notFollowedBy (single '/')
    name <- S.lexeme (some alphaNumChar)
    attrs <- S.lexeme htmlAttrs
    void $ notFollowedBy (S.lexeme (single '/'))
    void $ S.lexeme (single '>')
    return (name, attrs)

htmlEndTag :: String -> HTMLParser ()
htmlEndTag name = do
    void $ S.lexeme ( single '<')
    void $ single '/'
    void $ S.lexeme (string name)
    void $ S.lexeme (single '>')

htmlSingleTag :: HTMLParser Tag
htmlSingleTag = do
    void $ S.lexeme (single '<')
    name <- S.lexeme (some alphaNumChar)
    attrs <- htmlAttrs 
    let attrs2 = Data.Map.fromList attrs
    void $ S.lexeme (string "/>")
    return (Node name attrs2 [])

htmlAttr :: HTMLParser (String, String)
htmlAttr = do
    name <- S.lexeme (some (alphaNumChar <|> single '-'))
    void $ S.lexeme (single '=')
    val <- S.lexeme (htmlString)
    return (name, val)

htmlString :: HTMLParser String
htmlString = do
    str <- char '"' *> manyTill charInString (char '"') 
    return str where
        charInString = try (char '\\' *> escapedChar) <|> satisfy (/= '"')
        escapedChar = 
            (char 'n' >> return '\n') <|>
            (char 't' >> return '\t') <|>
            (char '"' >> return '"' ) <|>
            (char '\\' >> return '\\')

htmlAttrs :: HTMLParser [(String, String)]
htmlAttrs = many htmlAttr

htmlEmbeddedJS :: HTMLParser (JS.Doc, Text.Megaparsec.State String Void)
htmlEmbeddedJS = do
    i <- getInput
    let (jsr, _) = runState (runParserT jsDoc "<inline javascript>" i) JS.jsInitialState
        (Right (jsd, st@(Text.Megaparsec.State { stateInput = sti}))) = jsr
        (Text.Megaparsec.State { stateOffset = so }) = st
    setInput sti
    return (jsd, st)

htmlEmbeddedCSS :: HTMLParser CSSDoc
htmlEmbeddedCSS = do
    i <- getInput
    o <- getOffset
    st' <- getParserState
    let st = State { stateInput = i, stateOffset = o, stateParseErrors = [], statePosState = (statePosState st') }
        css = (runParser' cssDoc st) 
        (Text.Megaparsec.State { stateOffset = sto, stateInput = sti}, cssDE) = css
    if isLeft cssDE
    then
        let (Left a) = cssDE in fancyFailure (Data.Set.fromList [ErrorFail (errorBundlePretty a)])
    else do
        let (Right cssD) = cssDE
        setInput sti
        setOffset sto
        return cssD

htmlNode :: HTMLParser Tag
htmlNode = do
    (name, attrs) <- htmlBeginTag
    let name' = name
        attrs' = Data.Map.fromList attrs
    if name == "script"
    then do
        (jsd, _) <- htmlEmbeddedJS
        void $ htmlEndTag name
        return (JSNode name' attrs' jsd)
    else if name == "style"
    then do
        cssd <- htmlEmbeddedCSS
        void $ htmlEndTag name
        return (CSSNode name' attrs' cssd)
    else do
        nodes <- many ((htmlNode) <|> (htmlSingleTag))
        void $ htmlEndTag name
        return (Node name' attrs' nodes)
