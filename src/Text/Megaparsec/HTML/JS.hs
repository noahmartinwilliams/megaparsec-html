module Text.Megaparsec.HTML.JS where

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
import Text.Megaparsec.Error
import Text.Megaparsec.HTML.Space as S
import Text.Megaparsec.HTML.Types as HTML
import Text.Megaparsec.JS as JS

htmlEndJSTag :: HTMLParser ()
htmlEndJSTag = do
    void $ S.lexeme (string "</script>")

htmlEmbeddedJS :: HTMLParser (JS.Doc, Text.Megaparsec.State String Void)
htmlEmbeddedJS = do
    void $ notFollowedBy (string "</script>")
    i <- getInput
    o <- getOffset
    st <- getParserState
    let st' = State { stateInput = i, stateOffset = o, statePosState = (statePosState st), stateParseErrors = []}
        ((st'', jsr), _) = runState (runParserT' (jsDoc True) st' ) JS.jsInitialState
    case jsr of
        (Left err) -> fancyFailure (Data.Set.singleton (ErrorFail (errorBundlePretty err)))
        (Right (jsd, _)) -> do
            let (Text.Megaparsec.State { stateOffset = so, stateInput = si }) = st''
            setInput si
            setOffset so
            return (jsd, st'')

isFollowedByJS :: Map String String -> Bool
isFollowedByJS m = noSrc m where
    noSrc :: Map String String -> Bool
    noSrc inp = do
        let res = Data.Map.lookup "src" inp
        let res' = Data.Map.lookup "type" inp
        case res of
            Nothing -> do
                case res' of
                    Nothing -> True
                    (Just "text/javascript") -> True
                    notRight -> False
            (Just _) -> False

addScript :: Map String String -> HTML.ParserState -> HTML.ParserState
addScript m st = st { htmlExternScripts = (m : (htmlExternScripts st))}
