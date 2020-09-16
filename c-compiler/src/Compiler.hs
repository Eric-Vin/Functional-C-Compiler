module Compiler (compile, savePreprocessed) where

import Debug.Trace

import Preprocessor.Preprocessor

compile :: FilePath -> IO String
compile file_path = undefined

---------------------------------------------------------------------------------------------------
--Takes a FilePath pointing to an input ".c" file. Preprocesses the file and saves it to the
--output FilePath with extension ".pp"
---------------------------------------------------------------------------------------------------
savePreprocessed :: FilePath -> FilePath -> IO ()
savePreprocessed in_file_path out_file_path =   do
                                                    preprocessed_code <- preprocess in_file_path
                                                    writeFile out_file_path (show preprocessed_code)
