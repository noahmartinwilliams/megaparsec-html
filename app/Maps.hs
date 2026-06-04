module Maps where

import Data.Map
import Text.Megaparsec.HTML.Types

getAttrOrEmpty :: String -> Map String String -> String
getAttrOrEmpty str m = do
    case Data.Map.lookup str m of
        Nothing -> ""
        (Just s) -> s
