module Preprocess.Preprocessor (preprocess, SourceTracker) where

import System.IO
import System.FilePath(dropFileName)

import Data.List.Split

import Text.Regex.TDFA

import Error

import Debug.Trace

---------------------------------------------------------------------------------------------------
--Preprocessing Functions
---------------------------------------------------------------------------------------------------

-- |Preprocess function that takes a FilePath and returns an IO PreprocessedFile
preprocess :: FilePath -> IO PreprocessedFile
preprocess file_path =  do 
                            preprocessed_file_chunks <- preprocessFile emptyPreprocessorEnv file_path
                            return $ PreprocessedFile preprocessed_file_chunks

-- |Returns a PreprocessedFileChunk list containing the preprocessed contents of the file
preprocessFile :: PreprocessorEnv -> FilePath -> IO [PreprocessedFileChunk]
preprocessFile env file_path =  do
                                    file <- openFile file_path (ReadMode)                                                  --Opens the file passed in read only mode
                                    raw_file_contents <- hGetContents file                                                 --Pulls the contents of the file into raw_file_contents

                                    let split_raw_file_contents = (split . keepDelimsR . onSublist) "\n" raw_file_contents --Splits file into lines
                                        file_source_tracker     = SourceTracker file_path 1                                --Creates SourceTracker for target file_path
                                        
                                    preprocessed_text <- preprocessText env file_source_tracker split_raw_file_contents    --Preprocesses the text of the file

                                    return $ (SourceChunk file_source_tracker):preprocessed_text

-- |Returns 
preprocessText :: PreprocessorEnv -> SourceTracker -> [String] -> IO [PreprocessedFileChunk]
preprocessText _ _ []           =   return []
preprocessText env st (line:ls) =   do
                                        let line_type    = getLineType st line
                                            next_st      = incrementLineNum st

                                        processed_chunks <- case line_type of
                                                                Line text_line       -> return [(TextChunk (applyEnv env text_line))]
                                                                Include include_path -> applyIncludeDirective env next_st include_path

                                        later_chunks     <- (preprocessText env next_st ls)

                                        return $ processed_chunks ++ later_chunks

-- |Takes a line and returns type of Preprocessor directive. If not preprocessor directive, returns Line
getLineType :: SourceTracker -> String -> LineType
getLineType st line | not (lineIsDirective line) = Line (line)
                    | otherwise                  = (getDirective st line)

-- |Returns True if the passed line is a preprocessor directive and False otherwise
lineIsDirective :: String -> Bool
lineIsDirective line | (take 1 line) == "#" = True
                     | otherwise            = False

-- |Takes a line and returns a Directive.
getDirective :: SourceTracker -> String -> LineType
getDirective st line =  case (head split_line) of
                            "include" -> parseIncludeDirective st split_line
                            otherwise -> throwCompilerError Preprocessor st $ "Invalid directive \"" ++ (head split_line) ++ "\""
                        where
                            stripped_line = tail line
                            split_line    = splitOn " " stripped_line

parseIncludeDirective :: SourceTracker -> [String] -> LineType
parseIncludeDirective st include_params =   if (length include_params) == 2
                                            then
                                                (Include include_path)
                                            else
                                                throwCompilerError Preprocessor st "#include directive has incorrect number of parameters"
                                        where
                                            raw_include_path        = head $ tail include_params

                                            stripped_include_path   | raw_include_path =~ include_file_regex = tail $ init $ init raw_include_path
                                                                    | raw_include_path =~ include_lib_regex  = throwCompilerError Preprocessor st "#include does not support standard library includes at this time" 
                                                                    | otherwise                              = throwCompilerError Preprocessor st "#include expects \"FILENAME\" or <FILENAME>"

                                            include_path            = (dropFileName (sourcePath st)) ++ (stripped_include_path)


applyIncludeDirective :: PreprocessorEnv -> SourceTracker -> FilePath -> IO [PreprocessedFileChunk]
applyIncludeDirective env resume_st include_path =  do
                                                        include_chunks <- (preprocessFile env include_path)
                                                        return $ include_chunks ++ [(SourceChunk resume_st)]

---------------------------------------------------------------------------------------------------
--Preprocessing Datatypes
---------------------------------------------------------------------------------------------------
data PreprocessedFile      = PreprocessedFile [PreprocessedFileChunk]

data PreprocessedFileChunk = SourceChunk SourceTracker | TextChunk String

data SourceTracker         = SourceTracker  { 
                                              sourcePath :: FilePath
                                            , lineNumber :: Int
                                            }

data LineType              = Line String | Include {includePath :: FilePath}

instance Show PreprocessedFile where
    show (PreprocessedFile [])     = ""
    show (PreprocessedFile (x:xs)) = show x ++ (show (PreprocessedFile xs))

instance Show PreprocessedFileChunk where
    show (SourceChunk source_tracker)  = ("#" ++ (show source_tracker) ++ "\n")
    show (TextChunk preprocessed_text) = preprocessed_text

instance Show SourceTracker where
    show source_tracker        = "Source: \"" ++ (sourcePath source_tracker) ++ "\" Line: " ++ (show (lineNumber source_tracker))

---------------------------------------------------------------------------------------------------
--SourceTracker functions
---------------------------------------------------------------------------------------------------
incrementLineNum :: SourceTracker -> SourceTracker
incrementLineNum (SourceTracker source_path line_num) = (SourceTracker source_path (line_num + 1))

---------------------------------------------------------------------------------------------------
--PreprocessorEnv Datatype and Functions
---------------------------------------------------------------------------------------------------
type PreprocessorEnv      = [PreprocessorEnvEntry]

data PreprocessorEnvEntry = PreprocessorEnvEntry { 
                                                   key    :: String
                                                 , val    :: String
                                                 , source :: SourceTracker
                                                 }

emptyPreprocessorEnv :: PreprocessorEnv
emptyPreprocessorEnv = []

instance Show PreprocessorEnvEntry where
    show env_entry                = (key env_entry) ++ " -> " ++ (val env_entry) ++ " @ " ++ (show (source env_entry))

addEnv :: PreprocessorEnv -> SourceTracker -> (String,String) -> PreprocessorEnv
addEnv [] st (k, e)        = [(PreprocessorEnvEntry k e st)]
addEnv (x:xs) st new_entry = if (key x) == (fst new_entry)
                             then
                                error "PreprocessorEnv: Tried to add entry that already existed"
                             else
                                x:(addEnv xs st new_entry) 

removeEnv :: PreprocessorEnv -> String -> PreprocessorEnv
removeEnv [] k      = error "PreprocessorEnv: Tried to remove entry not in environment"
removeEnv (x:xs) k  = if k == (key x)
                         then
                            xs
                         else
                            x:(removeEnv xs k) 


lookupEnv :: PreprocessorEnv -> String -> String
lookupEnv [] k     = error "PreprocessorEnv: Tried to find entry not in environment"
lookupEnv (x:xs) k = if (key x) == k
                     then
                        val x
                     else
                        lookupEnv xs k

isDefined :: PreprocessorEnv -> String -> Bool
isDefined [] k     = False
isDefined (x:xs) k = if (key x) == k
                     then
                        True
                     else
                        isDefined xs k

isNotDefined :: PreprocessorEnv -> String -> Bool
isNotDefined env k = not (isDefined env k)

applyEnv :: PreprocessorEnv -> String -> String
applyEnv [] line            = line
applyEnv (entry:env) line   = applyEnv env replaced_line
                            where
                                entry_key     = key entry
                                entry_val     = val entry
                                replaced_line = replace line entry_key entry_val 

---------------------------------------------------------------------------------------------------
--RegEx Declarations
---------------------------------------------------------------------------------------------------
include_file_regex = "\"[a-zA-Z0-9_.-]+\""

include_lib_regex = "<[a-zA-Z0-9_.-]+>"

---------------------------------------------------------------------------------------------------
--Replace
---------------------------------------------------------------------------------------------------
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _ _ = []
replace s find repl =
    if take (length find) s == find
        then repl ++ (replace (drop (length find) s) find repl)
        else [head s] ++ (replace (tail s) find repl)