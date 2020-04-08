module Compiler (compile, savePreprocessed) where

import System.FilePath (takeBaseName, replaceExtension, splitPath, joinPath, hasTrailingPathSeparator)

import Debug.Trace

import Preprocess.Preprocessor

compile :: FilePath -> IO String
compile file_path = do
                        preprocessed_code <- preprocess file_path
                        return (show preprocessed_code)

---------------------------------------------------------------------------------------------------
--Takes a FilePath pointing to an input ".c" file. Preprocesses the file and saves it to the
--original FilePath with extension ".pp" or if it is a Test Harness file calls swapInputOutput
--before saving
---------------------------------------------------------------------------------------------------
savePreprocessed :: FilePath -> FilePath -> IO ()
savePreprocessed in_file_path out_file_path =   do
                                                    preprocessed_code <- preprocess in_file_path
                                                    writeFile out_file_path (show preprocessed_code)

---------------------------------------------------------------------------------------------------
--Takes a FilePath and swaps "input_files/" with "output_files/" in said FilePath while removing
--any sub directories
---------------------------------------------------------------------------------------------------
swapInputOutput :: FilePath -> FilePath
swapInputOutput in_file_path = out_file_path
    where
        split_in_file_path = splitPath in_file_path
        out_file_path      = joinPath (swapInputOutput split_in_file_path)

        swapInputOutput :: [FilePath] -> [FilePath]
        swapInputOutput []                  = error "Invalid path passed to swapInputOutput"
        swapInputOutput ("input_files/":xs) = "output_files/":[head xs]
        swapInputOutput (x:xs)              = x:(swapInputOutput xs)