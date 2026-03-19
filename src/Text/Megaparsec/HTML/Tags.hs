{-# LANGUAGE FlexibleContexts #-}
module Text.Megaparsec.HTML.Tags where

import Control.Monad
import Control.Monad.State
import Data.Either
import Data.Map
import Data.Maybe
import Data.Set
import Data.Tree as Tree
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char as Ch
import Text.Megaparsec.CSS as CSS
import Text.Megaparsec.HTML.JS
import Text.Megaparsec.HTML.Space as S
import Text.Megaparsec.HTML.Text
import Text.Megaparsec.HTML.Types as HTML

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
    void $ S.lexeme (single '>')
    let attrs' = Data.Map.fromList attrs
    if name == "script"
    then do
        if isFollowedByJS attrs'
        then do
            jsd <- S.lexeme (try htmlEndJSTag <|> try htmlEmbeddedJS)
            if isJust jsd
            then do
                void $ S.lexeme htmlEndTag
                let (Just (jsd', _)) = jsd in return (SymTag (Tree.Node (JSNode name attrs' (Just jsd')) []))
            else
                return (SymTag (Tree.Node (JSNode name attrs' Nothing) []))
        else do
            void $ S.lexeme htmlEndTag
            return (SymTag (Tree.Node (JSNode name attrs' Nothing) []))

    else if name == "style"
    then do
        cssd <- htmlEmbeddedCSS
        void $ S.lexeme htmlEndTag 
        return (SymTag (Tree.Node (CSSNode name attrs' cssd) []))
    else
        return (SymBeginTag name attrs)

htmlEndTag :: HTMLParser Symbol
htmlEndTag = do
    void $ lookAhead (string "</")
    void $ string "</"
    void $ Ch.space
    ident <- some (alphaNumChar)
    void $ Ch.space
    void $ S.lexeme (single '>')
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
    void $ S.lexeme (string "/>")
    return (SymTag (Tree.Node (HTML.Node name attrs2) []))

htmlAttr' :: HTMLParser (String, String)
htmlAttr' = (try htmlDefer) <|> (try htmlAttr)

htmlDefer :: HTMLParser (String, String)
htmlDefer = do
    void $ S.lexeme (string "defer")
    void $ notFollowedBy (S.lexeme (single '='))
    return ("defer", "")

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
htmlAttrs = many htmlAttr'


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
        (treeify nodes [])
    else
        fancyFailure (Data.Set.fromList [ErrorFail "Document does not begin with a start tag."])

isBeginTag :: Symbol -> Bool
isBeginTag (SymBeginTag _ _ ) = True
isBeginTag _ = False

treeify :: [Symbol] -> [Symbol] -> HTMLParser (Tree Tag)

treeify [] [SymTag t] = return t

treeify [] [SymTag t, SymMulti (Tree.Node (TextNode "\n") _)] = return t

treeify [] [(SymMulti (Tree.Node _ ts))] = return (Tree.Node (HTML.Node "html" Data.Map.empty) ts)

treeify input ((SymTag t) : (SymMulti (Tree.Node _ ts)) : rest) = treeify input ((SymMulti (Tree.Node HTML.NullTag (ts ++ [t]))) : rest)

treeify input ((SymTag t) : rest) = treeify input ((SymMulti (Tree.Node HTML.NullTag [t] )) : rest)

treeify input ((SymBeginTag "link" attrs) : rest) = treeify input ((SymTag (Tree.Node (HTML.Node "link" (Data.Map.fromList attrs)) [])) : rest)

treeify input ((SymBeginTag "meta" attrs) : rest) = treeify input ((SymTag (Tree.Node (HTML.Node "meta" (Data.Map.fromList attrs)) [])) : rest)

treeify input ((SymEndTag name) : (SymMulti (Tree.Node _ ts)) : (SymBeginTag name' attrs) : rest) | name == name' = treeify input ((SymTag (Tree.Node (HTML.Node name (Data.Map.fromList attrs)) ts)) : rest)

treeify input ((SymEndTag name) : (SymBeginTag name' attrs) : rest) | name == name' = treeify input ((SymTag (Tree.Node (HTML.Node name (Data.Map.fromList attrs)) [])) : rest)

treeify (a : rest) stack = treeify rest (a : stack)

treeify [] (a : _) = let err = "Reached end of input before tags closed. Last tag: " ++ (show a) in fancyFailure (Data.Set.fromList [(ErrorFail err)])
