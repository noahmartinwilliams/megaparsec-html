module Main where

import Text.Megaparsec
import Text.Megaparsec.HTML.Tags
import qualified Data.Text as T
import Data.Either

test01 :: IO ()
test01 = do
    let tag = "<tag>"
        result = parse htmlBeginTag "" (T.pack tag)
    if isRight result 
    then do
        let (Right result') = result
        if result' == "tag"
        then
            putStrLn "Test 01 succeeded."
        else
            putStrLn "Test 01 failed."
    else 
        let (Left err) = result in putStrLn (errorBundlePretty err)

main :: IO ()
main = do
    test01
