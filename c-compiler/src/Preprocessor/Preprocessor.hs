module Preprocessor.Preprocessor (preprocess) where

import System.Directory
import System.FilePath
import System.Info (os)

import Utilities.Error
import Utilities.SourceTracker

import Preprocessor.Scanner
import Preprocessor.Common

---------------------------------------------------------------------------------------------------
-- | Preprocessing Functions
---------------------------------------------------------------------------------------------------

preprocess :: FilePath -> IO PreprocessedFile
preprocess file_path =  do  
                            st <- makeSourceTracker <$> (makeAbsolute file_path)
                            preprocessed_toks <- preprocessFile file_path st
                            return $ PreprocessedFile preprocessed_toks

preprocessFile :: FilePath -> SourceTracker -> IO [PreprocessorToken]
preprocessFile file_path st =  do
                                file_text <- readFile file_path
                                let preprocessor_toks = ensureEOFNewline $ tokenize file_text
                                processed_toks <- applyDirectives preprocessor_toks st
                                return processed_toks
                            where
                                ensureEOFNewline :: [PreprocessorToken] -> [PreprocessorToken]
                                ensureEOFNewline toks = case last toks of
                                                        Other "\n"  -> toks
                                                        otherwise   -> toks ++ [Other "\n"]

applyDirectives :: [PreprocessorToken] -> SourceTracker -> IO [PreprocessorToken]
applyDirectives [] _                            = return []
applyDirectives ((Directive dir):rest_tok) st   = do
                                                    applied_dir <- applyDirective dir st
                                                    applied_tok <- applyDirectives rest_tok st
                                                    return $ applied_dir ++ applied_tok
applyDirectives (curr_tok:rest_tok) st          = (curr_tok:) <$> applyDirectives rest_tok st

applyDirective :: PreprocessorDirective -> SourceTracker -> IO [PreprocessorToken]
applyDirective (Include File file) st       = do 
                                                let normalized_file = (dropFileName $ sourcePath st) </> (file)
                                                file_exists <- doesFileExist normalized_file
                                                let target_file =   if file_exists 
                                                                    then 
                                                                        normalized_file 
                                                                    else 
                                                                        throwCompilerError $ PreprocessorError 
                                                                        $ "Attempted to include the file (Original: \"" ++ file ++ 
                                                                          "\" , Absolute:  \"" ++ normalized_file ++ 
                                                                          "\") which does not exist."
                                                preprocessFile target_file (makeSourceTracker target_file)

applyDirective (Include Library lib) st     = do
                                                let absolute_file = include_path </> (lib)
                                                file_exists <- doesFileExist absolute_file
                                                let target_file =   if file_exists 
                                                                    then 
                                                                        absolute_file 
                                                                    else 
                                                                        throwCompilerError $ PreprocessorError 
                                                                        $ "Attempted to include the library (Original: \"" ++ lib ++ 
                                                                          "\" , Absolute:  \"" ++ absolute_file ++ 
                                                                          "\") which does not exist."
                                                preprocessFile target_file (makeSourceTracker target_file)

                                            where
                                                include_path = case os of
                                                    "linux" -> "/usr/include/"
