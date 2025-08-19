module Main where

import Control.Parallel.Strategies
import Data.Either
import Data.Text as T
import GHC.Conc
import System.Environment
import System.Exit
import System.IO
import Text.Megaparsec
import Text.Megaparsec.HTML

parser :: (Text, String) -> (Bool, String)
parser (contents, fname) = do
    let res = parse htmlDoc fname contents
    if isLeft res
    then
        let (Left bundle) = res in (False, (errorBundlePretty bundle))
    else
        (True, "")

openEach :: [String] -> IO [Text]
openEach [] = return []
openEach (head : tail) = do
    fd <- readFile head
    rest <- openEach tail
    return ((T.pack fd) : rest)

and :: (Bool, String) -> (Bool, String) -> (Bool, String)
and (a, str) (b, str') = (a && b, str ++ str')

main :: IO ()
main = do
    args <- getArgs
    args' <- openEach args
    let results = (Prelude.map parser (Prelude.zip args' args) ) `using` (parBuffer numCapabilities rdeepseq)
        result = Prelude.foldr (Main.and) (True, "") results
    if fst result
    then
        exitWith ExitSuccess
    else do
        let (_, str) = result
        putStrLn str
        exitWith (ExitFailure 1)
