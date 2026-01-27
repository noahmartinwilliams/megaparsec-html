module Text.Megaparsec.HTML.Text where

import Text.Megaparsec
import Text.Megaparsec.HTML.Types

htmlTextNode :: HTMLParser Tag
htmlTextNode = do
    text <- some (anySingleBut '<')
    return (TextNode text)
