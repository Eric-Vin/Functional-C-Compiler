module Preprocessor.Preprocessor (preprocess) where

import Preprocessor.Scanner
import Preprocessor.Common

---------------------------------------------------------------------------------------------------
-- | Preprocessing Functions
---------------------------------------------------------------------------------------------------

-- | Preprocess function that takes a FilePath and returns an IO PreprocessedFile
preprocess :: FilePath -> IO PreprocessedFile
preprocess file_path =  do  file_text <- readFile file_path
                            let preprocessor_tokens = tokenize file_text
                            return preprocessor_tokens
