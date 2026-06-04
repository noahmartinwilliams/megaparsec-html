module Text.Megaparsec.HTML(htmlDoc, 
htmlDefaultState,
HTMLParser(..), 
Doc(..), 
DTD(..), 
ParserState(..),
Tag(CSSNode, JSNode, Node, TextNode))
where

import Text.Megaparsec
import Text.Megaparsec.HTML.DTD
import Text.Megaparsec.HTML.Space as S
import Text.Megaparsec.HTML.State
import Text.Megaparsec.HTML.Tags
import Text.Megaparsec.HTML.Types as HTML

htmlDoc :: HTMLParser HTML.Doc
htmlDoc = do
    dtd <- S.lexeme (optional htmlDTD)
    tags <- htmlNode
    return (Doc dtd tags)
