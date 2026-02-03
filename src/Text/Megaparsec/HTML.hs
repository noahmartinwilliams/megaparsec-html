module Text.Megaparsec.HTML(htmlDoc, 
HTMLParser(..), 
Doc(..), 
DTD(..), 
Tag(CSSNode, JSNode, Node, TextNode))
where

import Text.Megaparsec.HTML.DTD
import Text.Megaparsec.HTML.Tags
import Text.Megaparsec.HTML.Types as HTML

htmlDoc :: HTMLParser HTML.Doc
htmlDoc = do
    dtd <- htmlDTD
    tags <- htmlNode
    return (Doc dtd tags)
