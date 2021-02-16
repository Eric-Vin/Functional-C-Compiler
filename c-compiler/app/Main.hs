module Main (main, savePreprocessed) where

import System.Environment

import Compiler

main :: IO ()
main = undefined
--         do
--         args <- getArgs
--         compiled_code <- compile (head args)
--         putStrLn compiled_code

---------------------------------------------------------------------------------------------------
--Command Line Argument Processing Functions and Datatypes
---------------------------------------------------------------------------------------------------
data CompilerParams = CompilerParams { input_file_path     :: String
                                     , output_preprocessed :: Bool
                                     }

processArgs :: [String] -> CompilerParams
processArgs args = processFlags unset_params flags
    where
        (input_file_path:flags) = args

        unset_params = CompilerParams input_file_path False

        processFlags :: CompilerParams -> [String] -> CompilerParams
        processFlags params []         = params
        processFlags params (arg:args) = case arg of 
                                            "-p"      -> (CompilerParams input_file_path True)
                                            otherwise -> error ("Unrecognized flag passed: \"" ++ arg ++ "\"")
            where
                (CompilerParams orig_input_file_path orig_output_preprocessed) = params