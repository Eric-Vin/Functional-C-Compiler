module Compiler (compile, savePreprocessed) where

import System.FilePath (takeBaseName, replaceExtension, splitPath, joinPath)

import Debug.Trace

import Preprocess.Preprocessor

compile :: FilePath -> IO String
compile file_path = preprocess file_path

savePreprocessed :: FilePath -> IO ()
savePreprocessed input_file_path = do
                                    preprocessed_code <- preprocess input_file_path
                                    writeFile (traceShowId outputFilePath) preprocessed_code
    where
        outputFilePath    = if take 17 input_file_path == "test/input_files/"
                 then
                    replaceExtension (swapInputOutput input_file_path) ".pp"
                 else
                    replaceExtension input_file_path ".pp"

---------------------------------------------------------------------------------------------------
swapInputOutput :: FilePath -> FilePath
swapInputOutput in_file_path = out_file_path
	where
		split_in_file_path = splitPath in_file_path
		out_file_path      = joinPath (swapInputOutput split_in_file_path)

		swapInputOutput :: [FilePath] -> [FilePath]
		swapInputOutput []                  = error "Invalid path! Something went wrong"
		swapInputOutput ("input_files/":xs) = "output_files/":xs
		swapInputOutput (x:xs)              = x:(swapInputOutput xs)