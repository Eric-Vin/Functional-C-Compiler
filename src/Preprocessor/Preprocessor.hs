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
                            st                      <- makeSourceTracker <$> (makeAbsolute file_path)
                            let preprocessor_env = PreprocessorEnv [] st
                            (_, preprocessed_toks)  <- preprocessFile preprocessor_env file_path
                            return $ PreprocessedFile preprocessed_toks

preprocessFile :: PreprocessorEnv -> FilePath -> IO (PreprocessorEnv, [PreprocessorToken])
preprocessFile env file_path    = do
                                    file_text <- readFile file_path
                                    let preprocessor_toks = ensureEOFNewline $ tokenize file_text
                                    let preprocessor_env = replaceEnvSourceTracker env (makeSourceTracker file_path)
                                    applyDirectives preprocessor_env preprocessor_toks
                                where
                                    ensureEOFNewline :: [PreprocessorToken] -> [PreprocessorToken]
                                    ensureEOFNewline toks = case last toks of
                                                                Other "\n"  -> toks
                                                                _           -> toks ++ [Other "\n"]

-- | Runs through a list of PreprocessorTokens and applies any directives in the list
applyDirectives :: PreprocessorEnv -> [PreprocessorToken] -> IO (PreprocessorEnv, [PreprocessorToken])
applyDirectives env []                                  = return (env, [])
applyDirectives env ((Directive dir):rem_tok)           = do
                                                            (dir_env, dir_tok)                      <- applyDirective env dir rem_tok
                                                            (preprocessed_env, preprocessed_tok)    <- applyDirectives dir_env dir_tok
                                                            return $ (preprocessed_env, preprocessed_tok)
applyDirectives env toks                                = do
                                                            let (curr_tok, rem_tok) = applyMacros env toks
                                                            (preprocessed_env, preprocessed_tok) <- applyDirectives env rem_tok
                                                            return $ (preprocessed_env, curr_tok ++ preprocessed_tok)

-- | Takes a single directive and applies it
applyDirective :: PreprocessorEnv -> PreprocessorDirective -> [PreprocessorToken] -> IO (PreprocessorEnv, [PreprocessorToken])
applyDirective env (Include File file) toks      = do 
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
                                                    (preprocessed_env, preprocessor_toks) <- preprocessFile env target_file
                                                    return (preprocessed_env, preprocessor_toks ++ toks)
                                                where
                                                    file_path = sourcePath $ sourceTracker env

applyDirective env (Include Library lib) toks    = do
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
                                                    (preprocessed_env, preprocessor_toks) <- preprocessFile env target_file
                                                    return (preprocessed_env, preprocessor_toks ++ toks)
                                                where
                                                    include_path = case os of
                                                        "linux" -> "/usr/include/"

applyDirective env (Define macro tokens) toks   = return (replaceEnvMacroEnv env new_menv, toks)
                                                where
                                                    old_menv = macroEnv env
                                                    new_menv = addDefineVal old_menv macro tokens

applyDirective env (Undefine macro) toks        = return (replaceEnvMacroEnv env new_menv, toks)
                                                where
                                                    old_menv = macroEnv env
                                                    new_menv = removeDefineVal old_menv macro

applyDirective env (Ifdef macro) toks           = return (env, new_toks)
                                                where
                                                    menv = macroEnv env
                                                    conditional_val = isDefined menv macro
                                                    new_toks =  if conditional_val
                                                                then
                                                                    retainConditionalToks toks
                                                                else
                                                                    removeConditionalToks toks

applyDirective env (Ifndef macro) toks          = return (env, new_toks)
                                                where
                                                    menv = macroEnv env
                                                    conditional_val = not $ isDefined menv macro
                                                    new_toks =  if conditional_val
                                                                then
                                                                    retainConditionalToks toks
                                                                else
                                                                    removeConditionalToks toks

applyDirective _ (Endif) _                      = throwCompilerError $ PreprocessorError "Reached EOF outside of conditional"

-- | Takes a list or PreprocessorTokens applies a macro if the first token is a macro. 
-- | Returns a tuple with the first element being the tokens that have been applied or 
-- | do not contain a macro and the second element being the remaining tokens.
-- | NOTE: Takes a list of tokens instead of one to allow support for functional macros.
applyMacros :: PreprocessorEnv -> [PreprocessorToken] -> ([PreprocessorToken], [PreprocessorToken])
applyMacros _ []        = ([], [])
applyMacros env toks    = case head toks of
                            (Identifier macro)  ->  case getMacroVal (macroEnv env) macro of
                                                    Just mtoks  ->  let (nested_toks, nested_rem_toks) = applyMacros trimmed_env mtoks
                                                                    in (nested_toks ++ nested_rem_toks, tail toks)
                                                    Nothing     -> ([head toks], tail toks)
                                                where
                                                    trimmed_menv = removeDefineVal (macroEnv env) macro
                                                    trimmed_env = replaceEnvMacroEnv env trimmed_menv
                            _                   -> ([head toks], tail toks)

-- | Returns the tokens passed, including conditional text, with the next endif directive removed.
retainConditionalToks :: [PreprocessorToken] -> [PreprocessorToken]
retainConditionalToks toks  = retainConditionalToksH toks 0
                            where
                                retainConditionalToksH :: [PreprocessorToken] -> Int -> [PreprocessorToken]
                                retainConditionalToksH [] _                                     = throwCompilerError $ PreprocessorError "Reached EOF without encountering #endif"
                                retainConditionalToksH (tok@(Directive Endif):rem_toks) nf      =   if nf == 0
                                                                                                        then
                                                                                                            rem_toks
                                                                                                        else
                                                                                                            tok:(retainConditionalToksH rem_toks (nf - 1))
                                retainConditionalToksH (tok@(Directive (Ifdef _)):rem_toks) nf  = tok:(retainConditionalToksH rem_toks (nf + 1))
                                retainConditionalToksH (tok@(Directive (Ifndef _)):rem_toks) nf = tok:(retainConditionalToksH rem_toks (nf + 1))
                                retainConditionalToksH (curr_tok:rem_toks) nf                   = curr_tok:(retainConditionalToksH rem_toks nf)

-- | Returns the tokens passed, including conditional text, with the next endif directive removed.
removeConditionalToks :: [PreprocessorToken] -> [PreprocessorToken]
removeConditionalToks toks  = removeConditionalToksH toks 0
                            where
                                removeConditionalToksH :: [PreprocessorToken] -> Int -> [PreprocessorToken]
                                removeConditionalToksH [] _                                 = throwCompilerError $ PreprocessorError "Reached EOF without encountering #endif"
                                removeConditionalToksH ((Directive Endif):rem_toks) nf      =   if nf == 0
                                                                                                then
                                                                                                    rem_toks
                                                                                                else
                                                                                                    (removeConditionalToksH rem_toks (nf - 1))
                                removeConditionalToksH ((Directive (Ifdef _)):rem_toks) nf  = removeConditionalToksH rem_toks (nf + 1)
                                removeConditionalToksH ((Directive (Ifndef _)):rem_toks) nf = removeConditionalToksH rem_toks (nf + 1)
                                removeConditionalToksH (_:rem_toks) nf                      = removeConditionalToksH rem_toks nf

