module Text.Megaparsec.HTML.State where

import Data.Map
import Text.Megaparsec.HTML.Types

htmlDefaultState :: ParserState 
htmlDefaultState = ParserState { htmlExternScripts = [], htmlExternImgs = [] }

