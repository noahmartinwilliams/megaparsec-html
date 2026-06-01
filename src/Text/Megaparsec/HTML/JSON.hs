module Text.Megaparsec.HTML.JSON where

import Data.Map
import Data.Set
import Text.Megaparsec
import Text.Megaparsec.HTML.Types
import Text.Megaparsec.JSON

htmlJSON :: HTMLParser JSONObj
htmlJSON = do
    i <- getInput
    o <- getOffset
    st <- getParserState
    let (st', result) = runParser' jsonDoc st
    case result of 
        (Left e) -> fancyFailure (Data.Set.singleton (ErrorFail (errorBundlePretty e)))
        (Right r) -> do
            setInput (stateInput st')
            setOffset (stateOffset st')
            return r

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
