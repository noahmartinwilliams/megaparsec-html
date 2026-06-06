module Text.Megaparsec.HTML.Img where

import Data.Map
import Text.Megaparsec.HTML.Types

addImg :: Map String String -> ParserState -> ParserState
addImg str p = p { htmlExternImgs = (str : (htmlExternImgs p))}
