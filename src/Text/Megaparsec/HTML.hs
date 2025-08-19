module Text.Megaparsec.HTML(htmlDoc, 
Doc(..))
where

import Text.Megaparsec.HTML.DTD
import Text.Megaparsec.HTML.Tags
import Text.Megaparsec.HTML.Types as HTML

htmlDoc :: Parser HTML.Doc
htmlDoc = do
    dtd <- htmlDTD
    tags <- htmlNode
    return (Doc dtd tags)
