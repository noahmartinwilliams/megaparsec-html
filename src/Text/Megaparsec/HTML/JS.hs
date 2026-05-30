module Text.Megaparsec.HTML.JS where

import Control.Monad
import Control.Monad.State
import Data.Either
import Data.Map
import Data.Maybe
import Data.Set
import Data.Tree as Tree
import Data.Void
import Debug.Trace
import Text.Megaparsec
import Text.Megaparsec.Char as Ch
import Text.Megaparsec.Char as Ch
import Text.Megaparsec.HTML.Space as S
import Text.Megaparsec.HTML.Types
import Text.Megaparsec.JS as JS

htmlEndJSTag :: HTMLParser (Maybe (JS.Doc, Text.Megaparsec.State String Void))
htmlEndJSTag = do
    void $ string "</script>"
    return Nothing

htmlEmbeddedJS :: HTMLParser (Maybe (JS.Doc, Text.Megaparsec.State String Void))
htmlEmbeddedJS = do
    void $ notFollowedBy (string "</script>")
    i <- getInput
    o <- getOffset
    st <- getParserState
    let st' = State { stateInput = i, stateOffset = o, statePosState = (statePosState st), stateParseErrors = []}
        ((st'', jsr), _) = runState (runParserT' jsDoc st' ) JS.jsInitialState
    if isLeft jsr
    then
        let (Left err) = jsr in (fancyFailure (Data.Set.fromList [(ErrorFail (errorBundlePretty err))]))
    else do
        let (Right (jsd, _)) = jsr
            (Text.Megaparsec.State { stateOffset = so, stateInput = si }) = st''
        setInput si
        setOffset so
        return (Just (jsd, st''))

isFollowedByJS :: Map String String -> Bool
isFollowedByJS m = noSrc m where
    noSrc :: Map String String -> Bool
    noSrc inp = do
        let res = Data.Map.lookup "src" inp
        let res' = Data.Map.lookup "type" inp
        case res of
            Nothing -> do
                case res' of
                    Nothing -> False
                    (Just "text/javascript") -> True
                    notRight -> (trace (show notRight) False)
            (Just _) -> False

isFollowedByJSON :: Map String String -> Bool
isFollowedByJSON inp = do
    let res = Data.Map.lookup "src" inp
        res' = Data.Map.lookup "type" inp
    case res of
        Nothing -> do
            case res' of
                Nothing -> False
                (Just "application/ld+json") -> True
                (Just _) -> False

        (Just _) -> False
