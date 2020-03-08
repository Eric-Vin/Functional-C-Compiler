module Preprocess.Preprocessor (preprocess) where

import System.IO

preprocess :: FilePath -> IO String
preprocess file_path = do
                        file <- openFile file_path (ReadMode)       --Opens the file passed in read only mode
                        preprocessed_code <- preprocessFile file    --Preprocessed the opened file
                        return preprocessed_code

preprocessFile :: Handle -> IO String
preprocessFile file = raw_file_contents
    where
        raw_file_contents = hGetContents file  --Pulls the contents of the file into raw_file_contents

