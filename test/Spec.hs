module Main where

import Text.Megaparsec
import Text.Megaparsec.HTML.Tags
import qualified Data.Text as T
import Data.Either
import Text.Megaparsec.HTML.Types
import Text.Megaparsec.JS

test01 :: IO ()
test01 = do
    let tag = "<tag>"
        result = parse htmlBeginTag "" (T.pack tag)
    if isRight result 
    then do
        let (Right result') = result
        if result' == ("tag", [])
        then
            putStrLn "Test 01 succeeded."
        else
            putStrLn "Test 01 failed."
    else 
        let (Left err) = result in putStrLn (errorBundlePretty err)

test02 :: IO ()
test02 = do
    let tag = "</tag>"
        result = parse (htmlEndTag "tag") "" (T.pack tag)
    if isRight result 
    then do
        putStrLn "Test 02 succeeded."
    else 
        let (Left err) = result in putStrLn (errorBundlePretty err)

test03 :: IO ()
test03 = do
    let tag = "<tag attr=\"1\"/>"
        result = parse (htmlSingleTag ) "" (T.pack tag)
    if isRight result 
    then do
        let (Right (Node name _ _)) = result
        if name == (T.pack "tag")
        then do
            putStrLn "Test 03 succeeded."
        else do
            putStrLn "Test 03 failed."
    else 
        let (Left err) = result in putStrLn (errorBundlePretty err)

test04 :: IO ()
test04 = do
    let tag = "<body><tag attr=\"1\"/><h1><br/></h1></body>"
        result = parse (htmlNode) "" (T.pack tag)
    if isRight result 
    then do
        let (Right (Node name _ l)) = result
        if (name == (T.pack "body")) && ((Prelude.length l) == 2)
        then do
            putStrLn "Test 04 succeeded."
        else do
            putStrLn ("Test 04 failed. Got: " ++ (show result))
    else 
        let (Left err) = result in putStrLn (errorBundlePretty err)


test05 :: IO ()
test05 = do
    let tag = "<script type=\"text/javascript\">function f(a) { return a + 1; }</script>"
        result = parse (htmlNode) "" (T.pack tag)
    if isRight result 
    then do
        let (Right (JSNode _ _ doc)) = result
            (Text.Megaparsec.JS.Doc l) = doc
        if (Prelude.length l) == 1
        then do
            putStrLn "Test 05 succeeded."
        else do
            putStrLn ("Test 05 failed. Got: " ++ (show result))
    else 
        let (Left err) = result in putStrLn (errorBundlePretty err)
main :: IO ()
main = do
    test01
    test02
    test03
    test04
    test05
