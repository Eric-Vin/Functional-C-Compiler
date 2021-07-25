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
                                                            let (curr_tok, rem_tok) = applyMacrosOnce (macroEnv env) toks
                                                            (preprocessed_env, preprocessed_tok) <- applyDirectives env rem_tok
                                                            return $ (preprocessed_env, curr_tok:preprocessed_tok)

-- | Takes a single directive and applies it
applyDirective :: PreprocessorEnv -> PreprocessorDirective -> [PreprocessorToken] -> IO (PreprocessorEnv, [PreprocessorToken])
applyDirective env (Include File file) toks                 = do 
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

applyDirective env (Include Library lib) toks               = do
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

applyDirective env (ObjectDefine macro val) toks            = return (replaceEnvMacroEnv env new_menv, toks)
                                                            where
                                                                old_menv = macroEnv env
                                                                new_menv = addObjectDefineVal old_menv macro val
applyDirective env (FunctionDefine macro params val) toks   = return (replaceEnvMacroEnv env new_menv, toks)
                                                            where
                                                                old_menv = macroEnv env
                                                                new_menv = addFunctionDefineVal old_menv macro params val

applyDirective env (Undefine macro) toks                    = return (replaceEnvMacroEnv env new_menv, toks)
                                                            where
                                                                old_menv = macroEnv env
                                                                new_menv = removeDefineVal old_menv macro

applyDirective env (Ifdef macro) toks                       = return (env, new_toks)
                                                            where
                                                                menv = macroEnv env
                                                                conditional_val = isDefined menv macro
                                                                new_toks =  if conditional_val
                                                                            then
                                                                                retainConditionalToks toks
                                                                            else
                                                                                removeConditionalToks toks

applyDirective env (Ifndef macro) toks                      = return (env, new_toks)
                                                            where
                                                                menv = macroEnv env
                                                                conditional_val = not $ isDefined menv macro
                                                                new_toks =  if conditional_val
                                                                            then
                                                                                retainConditionalToks toks
                                                                            else
                                                                                removeConditionalToks toks

applyDirective _ (Endif) _                                  = throwCompilerError $ PreprocessorError "Reached EOF outside of conditional"

-- | Takes a list of PreprocessorTokens and expands the first token until it can no longer be expanded.
-- | Returns a tuple with the first element being the first token in its expanded form and the second
-- | element being the remaining tokens.
-- | NOTE: Takes a list of tokens instead of one to allow support for functional macros.
applyMacrosOnce :: MacroEnv -> [PreprocessorToken] -> (PreprocessorToken, [PreprocessorToken])
applyMacrosOnce _ []        = error "Cannot apply macros to an empty list of PreprocessorTokens."
applyMacrosOnce env toks    = case isExpandable env (head toks) of
                                True    -> applyMacrosOnce env new_toks
                                        where
                                            Identifier mid _ = head toks
                                            Just macro_map = getMacroVal env mid
                                            new_toks = applyMacro macro_map toks
                                False   -> (head toks, tail toks)
                            where
                                -- | Returns true if a macro using the String value in the Identifier is
                                -- | defined and the Identifier does not have itself as a parent.
                                -- | Note that since only an Identifier is expandable, any other PreprocessorToken
                                -- | being passed results in false.
                                isExpandable :: MacroEnv -> PreprocessorToken -> Bool
                                isExpandable env (Identifier name parents)  = (isDefined env name) && (not $ elem name parents)
                                isExpandable _ _                            = False

                                applyMacro :: MacroMap -> [PreprocessorToken] -> [PreprocessorToken]
                                applyMacro (ObjectMap mid mtoks) toks   = new_toks
                                                                        where
                                                                            Identifier name parents = head toks
                                                                            new_toks = (setParentList (mid:parents) mtoks) ++ tail toks

                                setParentList :: [String] -> [PreprocessorToken] -> [PreprocessorToken]
                                setParentList _ []                                              = []
                                setParentList new_parents ((Identifier name _):rem_toks)        = new_identifier:(setParentList new_parents rem_toks)
                                                                                                    where
                                                                                                        new_identifier = Identifier name (new_parents)
                                setParentList new_parents (toks:rem_toks)                       = toks:(setParentList new_parents rem_toks)

-- | Takes a list of PreprocessorTokens and applies macro expansion to the whole list of tokens
-- | Returns a list of tokens that have been fully expanded
-- applyMacrosExhaustive :: MacroEnv -> [PreprocessorToken] -> [PreprocessorToken]
-- applyMacrosExhaustive _ []      = []
-- applyMacrosExhaustive env toks  = 

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

-- | Returns the tokens passed, without conditional text, with the next endif directive removed.
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

