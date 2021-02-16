module Preprocessor.Preprocessor (preprocess) where

import System.Directory
import System.FilePath
import System.Info (os)

import Control.Monad.State.Lazy

import Utilities.Error
import Utilities.SourceTracker

import Preprocessor.Scanner
import Preprocessor.Common

---------------------------------------------------------------------------------------------------
-- | Preprocessing Functions
---------------------------------------------------------------------------------------------------

preprocess :: FilePath -> IO PreprocessedFile
preprocess file_path =  do  
                            st                      <- makeSourceTracker <$> (makeAbsolute file_path)
                            let preprocessor_env = PreprocessorEnv [] st
                            (_, preprocessed_toks)  <- preprocessFile preprocessor_env file_path
                            return $ PreprocessedFile preprocessed_toks

preprocessFile :: PreprocessorEnv -> FilePath -> IO (PreprocessorEnv, [PreprocessorToken])
preprocessFile env file_path    = do
                                    file_text <- readFile file_path
                                    let preprocessor_toks = ensureEOFNewline $ tokenize file_text
                                    let preprocessor_env = replaceEnvSourceTracker env (makeSourceTracker file_path)
                                    applyDirectives env preprocessor_toks
                                where
                                    ensureEOFNewline :: [PreprocessorToken] -> [PreprocessorToken]
                                    ensureEOFNewline toks = case last toks of
                                                            Other "\n"  -> toks
                                                            otherwise   -> toks ++ [Other "\n"]

-- | Runs through a list of PreprocessorTokens and applies any directives in the list
applyDirectives :: PreprocessorEnv -> [PreprocessorToken] -> IO (PreprocessorEnv, [PreprocessorToken])
applyDirectives env []                                  = return (env, [])
applyDirectives env ((Directive dir):rem_tok)           = do
                                                            (dir_env, dir_tok)                      <- applyDirective env dir
                                                            (preprocessed_env, preprocessed_tok)    <- applyDirectives dir_env rem_tok
                                                            return $ (preprocessed_env, dir_tok ++ preprocessed_tok)
applyDirectives env toks                                = do
                                                            let (curr_tok, rem_tok) = applyMacros env toks
                                                            (preprocessed_env, preprocessed_tok) <- applyDirectives env rem_tok
                                                            return $ (preprocessed_env, curr_tok ++ preprocessed_tok)

-- | Takes a single directive and applies it
applyDirective :: PreprocessorEnv -> PreprocessorDirective -> IO (PreprocessorEnv, [PreprocessorToken])
applyDirective env (Include File file)      = do 
                                                let normalized_file = (dropFileName $ file_path) </> (file)
                                                file_exists <- doesFileExist normalized_file
                                                let target_file =   if file_exists 
                                                                    then 
                                                                        normalized_file 
                                                                    else 
                                                                        throwCompilerError $ PreprocessorError 
                                                                        $ "Attempted to include the file (Original: \"" ++ file ++ 
                                                                          "\" , Absolute:  \"" ++ normalized_file ++ 
                                                                          "\") which does not exist."
                                                preprocessFile env target_file
                                            where
                                                file_path = sourcePath $ st env

applyDirective env (Include Library lib)    = do
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
                                                preprocessFile env target_file

                                            where
                                                include_path = case os of
                                                    "linux" -> "/usr/include/"

applyDirective env (Define id tokens)       = return (replaceEnvMacroEnv env new_menv, [])
                                            where
                                                old_menv = macro_env env
                                                new_menv = addDefineVal old_menv id tokens

applyDirective env (Undefine id)            = return (replaceEnvMacroEnv env new_menv, [])
                                            where
                                                old_menv = macro_env env
                                                new_menv = removeDefineVal old_menv id

-- | Takes a list or PreprocessorTokens applies a macro if the first token is a macro. 
-- | Returns a tuple with the first element being the tokens that have been applied or 
-- | do not contain a macro and the second element being the remaining tokens.
-- | NOTE: Takes a list of tokens instead of one to allow support for functional macros.
applyMacros :: PreprocessorEnv -> [PreprocessorToken] -> ([PreprocessorToken], [PreprocessorToken])
applyMacros env []              = ([], [])
applyMacros env toks    = case head toks of
                            (Identifier id) ->  case getMacroVal (macro_env env) id of
                                                    Just mtoks  ->  let (nested_toks, nested_rem_toks) = applyMacros trimmed_env mtoks
                                                                    in (nested_toks ++ nested_rem_toks, tail toks)
                                                    Nothing     -> ([head toks], tail toks)
                                                where
                                                    trimmed_menv = removeDefineVal (macro_env env) id
                                                    trimmed_env = replaceEnvMacroEnv env trimmed_menv
                            otherwise -> ([head toks], tail toks)