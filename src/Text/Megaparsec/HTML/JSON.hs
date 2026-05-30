module Text.Megaparsec.HTML.JSON where

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
        (Left e) -> fancyFailure (Data.Set.fromList [(ErrorFail (errorBundlePretty e))])
        (Right r) -> do
            setInput (stateInput st')
            setOffset (stateOffset st')
            return r
