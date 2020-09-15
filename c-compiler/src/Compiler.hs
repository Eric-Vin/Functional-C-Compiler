module Compiler (compile, savePreprocessed) where

import System.FilePath (takeBaseName, replaceExtension, splitPath, joinPath, hasTrailingPathSeparator)

import Debug.Trace

import Preprocessor.Preprocessor

compile :: FilePath -> IO String
compile file_path = do
                        preprocessed_code <- preprocess file_path
                        return (show preprocessed_code)

---------------------------------------------------------------------------------------------------
--Takes a FilePath pointing to an input ".c" file. Preprocesses the file and saves it to the
--output FilePath with extension ".pp"
---------------------------------------------------------------------------------------------------
savePreprocessed :: FilePath -> FilePath -> IO ()
savePreprocessed in_file_path out_file_path =   do
                                                    preprocessed_code <- preprocess in_file_path
                                                    writeFile out_file_path (show preprocessed_code)
