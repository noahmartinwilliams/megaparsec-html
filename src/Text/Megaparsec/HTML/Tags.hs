{-# LANGUAGE FlexibleContexts #-}
module Text.Megaparsec.HTML.Tags where

import Control.Monad
import Control.Monad.State
import Data.Either
import Data.Map
import Data.Set
import Data.Tree as Tree
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char as Ch
import Text.Megaparsec.CSS as CSS
import Text.Megaparsec.HTML.Space as S
import Text.Megaparsec.HTML.Text
import Text.Megaparsec.HTML.Types as HTML
import Text.Megaparsec.JS as JS

data Symbol = SymMulti (Tree Tag) | SymBeginTag String [(String, String)] | SymTag (Tree Tag) | SymEndTag String deriving(Show, Eq)

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
        jsd <- htmlEmbeddedJS
        void $ htmlEndTag
        let (jsd', _) = jsd
        return (SymTag (Tree.Node (JSNode name (Data.Map.fromList attrs) jsd') []))
    else if name == "style"
    then do
        cssd <- htmlEmbeddedCSS
        void $ htmlEndTag 
        return (SymTag (Tree.Node (CSSNode name (Data.Map.fromList attrs) cssd) []))
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
    return (SymTag (Tree.Node (HTML.Node name attrs2) []))

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
    o <- getOffset
    st <- getParserState
    let st' = State { stateInput = i, stateOffset = o, statePosState = (statePosState st), stateParseErrors = []}
        ((st'', jsr), _) = runState (runParserT' jsDoc st' ) JS.jsInitialState
    if isLeft jsr
    then
        let (Left err) = jsr in fancyFailure (Data.Set.fromList [(ErrorFail (errorBundlePretty err))])
    else do
        let (Right (jsd, _)) = jsr
            (Text.Megaparsec.State { stateOffset = so, stateInput = si }) = st''
        setInput si
        setOffset so
        return (jsd, st'')

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
    return (SymTag (Tree.Node t []))

htmlNode :: HTMLParser (Tree Tag)
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

treeify :: [Symbol] -> [Symbol] -> Tree Tag

treeify [] [SymTag t] = t

treeify [] [SymTag t, SymMulti (Tree.Node (TextNode "\n") _)] = t

treeify [] [(SymMulti (Tree.Node _ ts))] = Tree.Node (HTML.Node "html" Data.Map.empty) ts

treeify input ((SymTag t) : (SymMulti (Tree.Node _ ts)) : rest) = treeify input ((SymMulti (Tree.Node HTML.NullTag (ts ++ [t]))) : rest)

treeify input ((SymTag t) : rest) = treeify input ((SymMulti (Tree.Node HTML.NullTag [t] )) : rest)

treeify input ((SymBeginTag "link" attrs) : rest) = treeify input ((SymTag (Tree.Node (HTML.Node "link" (Data.Map.fromList attrs)) [])) : rest)

treeify input ((SymBeginTag "meta" attrs) : rest) = treeify input ((SymTag (Tree.Node (HTML.Node "meta" (Data.Map.fromList attrs)) [])) : rest)

treeify input ((SymEndTag name) : (SymMulti (Tree.Node _ ts)) : (SymBeginTag name' attrs) : rest) | name == name' = treeify input ((SymTag (Tree.Node (HTML.Node name (Data.Map.fromList attrs)) ts)) : rest)

treeify input ((SymEndTag name) : (SymBeginTag name' attrs) : rest) | name == name' = treeify input ((SymTag (Tree.Node (HTML.Node name (Data.Map.fromList attrs)) [])) : rest)

treeify (a : rest) stack = treeify rest (a : stack)
