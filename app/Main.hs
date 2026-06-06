module Main where

import Args
import Control.Monad.Reader
import Control.Monad.State
import Control.Parallel.Strategies
import Data.Either
import GHC.Conc
import Maps
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Text.Megaparsec
import Text.Megaparsec.HTML as HTML

catn :: [String] -> String
catn ls = foldr (\x -> \y -> x ++ "\n" ++ y) "" ls

getResults :: HTML.Doc -> ParserState -> Reader Opts String
getResults doc st = do
    pei <- reader printExternImgs
    pes <- reader printExternScripts
    pec <- reader printExternCSS
    let ret0 = if pei then catn (map (\x -> getAttrOrEmpty "src" x) (htmlExternImgs st)) else ""
        ret1 = if pes then catn (map (\x -> getAttrOrEmpty "src" x) (htmlExternScripts st)) else ""
        ret2 = if pec then catn (map (\x -> getAttrOrEmpty "src" x) (htmlExternCSS st)) else ""
    return (ret0 ++ ret1 ++ ret2)
        
parser :: Reader Opts (Bool, String)
parser = do
    fname <- reader inputFileName
    contents <- reader inputFile
    let res = runParserT htmlDoc fname contents
        (res', state') = runState res htmlDefaultState
    case res' of
        (Left bundle) -> return (False, (errorBundlePretty bundle))
        (Right res'') -> do
            result <- getResults res'' state'
            return (True, result)

main :: IO ()
main = do
    args <- getArgs
    pname <- getProgName
    let dopts = defaultOpts
    case getOpt RequireOrder flags args of
        (opts, _, []) -> do
            let newOpts = (foldl (flip id) dopts opts)
            f <- readFile (inputFileName newOpts)
            let newOpts' = newOpts { inputFile = f }
                rets = runReader parser newOpts'
            case rets of
                (True, p) -> putStrLn p
                (False, e) -> do
                    hPutStrLn stderr e
                    exitFailure
        (_, _, errs) -> do
            hPutStrLn stderr ((concat errs) ++ "\n" ++ (usageInfo "htmlChecker" flags))
            exitFailure
