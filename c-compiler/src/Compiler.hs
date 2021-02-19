module Compiler where

import System.FilePath
import System.Directory

import Control.Monad

import Preprocessor.Preprocessor
import Preprocessor.Common(PreprocessedFile)

compile :: CompilerParams -> IO ()
compile params  = do
                    preprocessed_code <- preprocess in_file_path
                    when (savePreprocessed params) $ savePreprocessedFile out_file_path preprocessed_code
                where
                    in_file_path    = inputFilePath params
                    out_file_path   = outputFilePath params

---------------------------------------------------------------------------------------------------
-- | Takes a PreprocessedFile and a FilePath and saves the PreprocessedFile to the FilePath with
-- | the extension .pp
---------------------------------------------------------------------------------------------------
savePreprocessedFile :: FilePath -> PreprocessedFile -> IO ()
savePreprocessedFile out_file_path pp_file  = writeFile ((dropExtension out_file_path) ++ ".pp") (show $ pp_file)
---------------------------------------------------------------------------------------------------
--Command Line Argument Datatypes and Functions
---------------------------------------------------------------------------------------------------
data CompilerParams = CompilerParams { inputFilePath      :: FilePath
                                     , outputFilePath     :: FilePath
                                     , savePreprocessed   :: Bool
                                     }
                                     deriving (Show)

processArgs :: [String] -> IO CompilerParams
processArgs args    = do
                        let def_in_file_path    = head args
                        let def_out_file_path   = (dropExtension def_in_file_path) ++ ".asm"
                        let def_params          = CompilerParams def_in_file_path def_out_file_path False
                        flags <- cleanIOPaths $ processFlags def_params (tail args)
                        return flags

                        where
                            processFlags :: CompilerParams -> [String] -> CompilerParams
                            processFlags params []          = params
                            processFlags params (arg:args)  = case arg of
                                                                "-o"        -> (CompilerParams orig_input_file_path new_output_path orig_save_preprocessed)
                                                                            where
                                                                                (CompilerParams orig_input_file_path orig_output_path orig_save_preprocessed) = processFlags params (tail args)
                                                                                new_output_path = head args
                                                                "-p"        -> CompilerParams orig_input_file_path orig_output_path True
                                                                            where
                                                                                (CompilerParams orig_input_file_path orig_output_path orig_save_preprocessed) = processFlags params args
                                                                otherwise   -> error ("Argument Error: Unrecognized flag passed: \"" ++ arg ++ "\"")

cleanIOPaths :: CompilerParams -> IO CompilerParams
cleanIOPaths old_params = do
                            valid_in_file_path  <- makeAbsolute orig_input_path >>= doesFileExist
                            new_in_file_path    <-  if valid_in_file_path
                                                    then 
                                                        makeAbsolute orig_input_path
                                                    else
                                                        error $ "Argument Error: The specified input file \"" ++ orig_input_path ++ "\" does not exist"
                            new_out_file_path   <-  makeAbsolute orig_output_path
                            new_output_path <- makeAbsolute orig_output_path
                            return $ CompilerParams new_in_file_path new_out_file_path orig_save_preprocessed
                        where
                            (CompilerParams orig_input_path orig_output_path orig_save_preprocessed) = old_params
