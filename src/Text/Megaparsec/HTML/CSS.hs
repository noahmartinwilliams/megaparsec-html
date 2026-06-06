module Text.Megaparsec.HTML.CSS where

import Control.Monad.State
import Data.Either
import Data.Map
import Data.Set
import Text.Megaparsec
import Text.Megaparsec.CSS
import Text.Megaparsec.HTML.Types as HTML

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

addStyle :: Map String String -> HTML.ParserState -> HTML.ParserState
addStyle m s = s { htmlExternCSS = (m : (htmlExternCSS s))}
