module Args where

import System.Console.GetOpt

data Opts = Opts { printExternImgs :: Bool, printExternScripts :: Bool, inputFile :: String, inputFileName :: String } deriving(Eq)

defaultOpts :: Opts
defaultOpts = Opts { printExternImgs = False, printExternScripts = False, inputFile = "", inputFileName = "" }

flags :: [OptDescr (Opts -> Opts)]
flags = [ Option ['i']  ["list-images"] (NoArg (\opts -> opts { printExternImgs = True})) "List images referenced by HTML code.",
    Option ['s'] ["list-scripts"] (NoArg (\opts -> opts { printExternScripts = True})) "List external javascript files referenced by HTML code.",
    Option ['f'] ["file"] (ReqArg (\f -> \opts -> opts { inputFileName = f }) "") "Specify HTML file to read from." ]
