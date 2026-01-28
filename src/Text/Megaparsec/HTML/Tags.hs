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
import Text.Megaparsec.Char as Ch
import Text.Megaparsec.Char.Lexer
import Text.Megaparsec.CSS as CSS
import Text.Megaparsec.HTML.Space as S
import Text.Megaparsec.HTML.Text
import Text.Megaparsec.HTML.Types as HTML
import Text.Megaparsec.JS as JS

htmlBeginTag :: HTMLParser Symbol
htmlBeginTag = do
    void $ single '<'
    void $ notFollowedBy (single '/')
    name <- (some alphaNumChar)
    void $ Ch.space
    attrs <- htmlAttrs
    void $ Ch.space
    void $ notFollowedBy (single '/')
    void $ (single '>')
    if name == "script"
    then do
        (jsd, _) <- htmlEmbeddedJS
        void $ htmlEndTag 
        return (SymTag (JSNode name (Data.Map.fromList attrs) jsd))
    else if name == "style"
    then do
        cssd <- htmlEmbeddedCSS
        void $ htmlEndTag 
        return (SymTag (CSSNode name (Data.Map.fromList attrs) cssd))
    else
        return (SymBeginTag name attrs)

htmlEndTag :: HTMLParser Symbol
htmlEndTag = do
    void $ lookAhead (string "</")
    void $ string "</"
    void $ Ch.space
    ident <- some (alphaNumChar)
    void $ Ch.space
    void $ (single '>')
    return (SymEndTag ident)

htmlSingleTag :: HTMLParser Symbol
htmlSingleTag = do
    void $ S.lexeme (single '<')
    void $ notFollowedBy (single '/')
    void $ Ch.space
    name <- (some alphaNumChar)
    void $ Ch.space
    attrs <- htmlAttrs 
    let attrs2 = Data.Map.fromList attrs
    void $ Ch.space
    void $ (string "/>")
    return (SymTag (Node name attrs2 []))

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
    if isLeft jsr
    then
        let (Left err) = jsr in fancyFailure (Data.Set.fromList [ErrorFail (errorBundlePretty err)])
    else do
        let (Right (jsd, st@(Text.Megaparsec.State { stateInput = sti}))) = jsr
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

htmlText :: HTMLParser Symbol
htmlText = do
    t <- htmlTextNode
    return (SymTag t)

htmlNode :: HTMLParser Tag
htmlNode = do
    nodes <- some (try htmlText <|> try htmlBeginTag <|> try htmlEndTag <|> try htmlSingleTag)
    let (node1 : _) = nodes
    if (isBeginTag node1)
    then
        return (treeify nodes [])
    else
        fancyFailure (Data.Set.fromList [ErrorFail "Document does not begin with a start tag."])

isBeginTag :: Symbol -> Bool
isBeginTag (SymBeginTag _ _ ) = True
isBeginTag _ = False

isEndTag :: String -> Symbol -> Bool
isEndTag n (SymEndTag n') | n == n' = True
isEndTag _ _ = False

treeify :: [Symbol] -> [Symbol] -> Tag

treeify [] [SymTag t] = t

treeify [] [(SymMulti ts)] = Node "html" Data.Map.empty ts

treeify input ((SymTag t) : rest) = treeify input ((SymMulti [t]) : rest)

treeify input ((SymTag t) : (SymMulti ts) : rest) = treeify input ((SymMulti (ts ++ [t])) : rest)

treeify input ((SymEndTag name) : (SymMulti ts) : (SymBeginTag name' attrs) : rest) | name == name' = treeify input ((SymTag (Node name (Data.Map.fromList attrs) ts)) : rest)

treeify input ((SymEndTag name) : (SymBeginTag name' attrs) : rest) | name == name' = treeify input ((SymTag (Node name (Data.Map.fromList attrs) [])) : rest)

treeify (a : rest) stack = treeify rest (a : stack)
