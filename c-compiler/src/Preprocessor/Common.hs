module Preprocessor.Common where 

import Data.List

import Utilities.Error
import Utilities.SourceTracker

---------------------------------------------------------------------------------------------------
-- | Preprocessing Datatypes
---------------------------------------------------------------------------------------------------

-- | Datatype representing a list of Preprocessor Tokens
newtype PreprocessedFile   = PreprocessedFile {getFile :: [PreprocessorToken]}

instance Show PreprocessedFile where
    show file  = fileConcat $ stripFirstNewlines $ map show unpackaged_file
                where
                    unpackaged_file = getFile file

                    stripFirstNewlines :: [String] -> [String]
                    stripFirstNewlines ("\n":xs)     = stripFirstNewlines xs
                    stripFirstNewlines string        = string

                    fileConcat :: [String] -> String
                    fileConcat []               = []
                    fileConcat (x:[])           = x
                    fileConcat ("\n":"\n":xs)   = fileConcat ("\n":xs)
                    fileConcat ("\n":xs)        = "\n" ++ (fileConcat xs)
                    fileConcat (x:xs)           = x ++ " " ++ (fileConcat xs)

-- | Datatype representing one Preprocessor token
data PreprocessorToken  = Directive PreprocessorDirective
                        | Punctuator String
                        | Identifier String
                        | PreprocessingNumber String
                        | StringLiteral String
                        | Other String
                        deriving (Eq)
                        
instance Show PreprocessorToken where
    show (Directive directive)          = show directive
    show (Punctuator value)             = value
    show (Identifier value)             = value
    show (PreprocessingNumber value)    = value
    show (StringLiteral value)          = value
    show (Other value)                  = value

-- | Datatype representing a Preprocessor Directive
data PreprocessorDirective  = Include IncludeType String
                            | Define String [PreprocessorToken]
                            | Undefine String
                            | Ifdef String
                            | Ifndef String
                            | Endif
                            deriving (Eq, Show)

data IncludeType    = File | Library
                    deriving (Eq, Show)

-- | Datatype representing a Preprocessor Environment

data PreprocessorEnv    = PreprocessorEnv {
                            macro_env   :: MacroEnv ,
                            st          :: SourceTracker
                        } 

replaceEnvSourceTracker :: PreprocessorEnv -> SourceTracker -> PreprocessorEnv
replaceEnvSourceTracker env st  = PreprocessorEnv (macro_env env) st

replaceEnvMacroEnv :: PreprocessorEnv -> MacroEnv -> PreprocessorEnv
replaceEnvMacroEnv env menv = PreprocessorEnv menv (st env)

type MacroEnv = [(String, [PreprocessorToken])]

-- | Returns true if a macro using the String as an id is defined
isDefined :: MacroEnv -> String -> Bool
isDefined [] id                 = False
isDefined ((macro,val):renv) id =   if macro == id
                                    then
                                        True
                                    else
                                        isDefined renv id

-- | Returns Just val where val is the macro vaue if the macro is defined
-- | and Nothing if the macro is undefined
getMacroVal :: MacroEnv -> String -> Maybe [PreprocessorToken]
getMacroVal [] id                   = Nothing
getMacroVal ((macro,val):renv) id   =   if macro == id
                                        then
                                            Just val
                                        else
                                            getMacroVal renv id

-- | Adds a macro value to the macro environment. If the macro is already
-- | defined, then the macro is redefined with the new value.
addDefineVal :: MacroEnv -> String -> [PreprocessorToken] -> MacroEnv
addDefineVal [] id new_val                  = [(id, new_val)]
addDefineVal ((macro, val):renv) id new_val =   if macro == id
                                                then
                                                    (macro, new_val):renv
                                                else
                                                    (macro, val):(addDefineVal renv id new_val)

-- | Removes a macro value from this environment. Does nothing if the
-- | macro is not defined in the environment.
removeDefineVal :: MacroEnv -> String -> MacroEnv
removeDefineVal [] id                   = []
removeDefineVal ((macro, val):renv) id  =   if macro == id
                                            then
                                                renv
                                            else
                                                (macro, val):(removeDefineVal renv id)
