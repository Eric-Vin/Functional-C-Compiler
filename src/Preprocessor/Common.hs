module Preprocessor.Common where 

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
-- | The Identifier token also contains a list of macro
-- | expansions it has been a part of to prevent infinite
-- | recursive expansion of macros.
data PreprocessorToken  = Directive PreprocessorDirective
                        | Punctuator String
                        | Identifier String [String]
                        | PreprocessingNumber String
                        | StringLiteral String
                        | Other String
                        deriving (Eq)
                        
instance Show PreprocessorToken where
    show (Directive directive)          = show directive
    show (Punctuator value)             = value
    show (Identifier value _)           = value
    show (PreprocessingNumber value)    = value
    show (StringLiteral value)          = value
    show (Other value)                  = value

-- | Datatype representing a Preprocessor Directive
data PreprocessorDirective  = Include IncludeType String
                            | ObjectDefine String [PreprocessorToken]
                            | FunctionDefine String [String] [PreprocessorToken]
                            | Undefine String
                            | Ifdef String
                            | Ifndef String
                            | Endif
                            deriving (Eq, Show)

data IncludeType    = File | Library
                    deriving (Eq, Show)

-- | Datatype representing a Preprocessor Environment

data PreprocessorEnv    = PreprocessorEnv {
                            macroEnv        :: MacroEnv ,
                            sourceTracker   :: SourceTracker
                        } 

replaceEnvSourceTracker :: PreprocessorEnv -> SourceTracker -> PreprocessorEnv
replaceEnvSourceTracker env st  = PreprocessorEnv (macroEnv env) st

replaceEnvMacroEnv :: PreprocessorEnv -> MacroEnv -> PreprocessorEnv
replaceEnvMacroEnv env menv = PreprocessorEnv menv (sourceTracker env)

type MacroEnv   = [MacroMap]

data MacroMap   = ObjectMap String [PreprocessorToken]
                | FunctionMap String [String] [PreprocessorToken]

-- | Returns Just val where val is the macro string if the macro is defined
-- | and Nothing if the macro is undefined
getMacroName :: MacroMap -> String
getMacroName (ObjectMap macro _)        = macro
getMacroName (FunctionMap macro _ _)    = macro

-- | Returns Just val where val is the macro value if the macro is defined
-- | and Nothing if the macro is undefined.
getMacroVal :: MacroEnv -> String -> Maybe MacroMap
getMacroVal [] _                = Nothing
getMacroVal (m:renv) t_macro    =   if (getMacroName m) == t_macro
                                    then
                                        Just m
                                    else
                                        getMacroVal renv t_macro

-- | Returns true if a macro using the String as an id is defined
isDefined :: MacroEnv -> String -> Bool
isDefined env macro = case getMacroVal env macro of
                        Nothing -> False
                        Just _  -> True

-- | Adds an object macro to the macro environment. If the macro is already
-- | defined, then the macro is redefined with the new value.
addObjectDefineVal :: MacroEnv -> String -> [PreprocessorToken] -> MacroEnv
addObjectDefineVal [] macro val         = [ObjectMap macro val]
addObjectDefineVal (m:renv) macro val   =   if (getMacroName m) == macro
                                            then
                                                (ObjectMap macro val):renv
                                            else
                                                m:(addObjectDefineVal renv macro val)


addFunctionDefineVal :: MacroEnv -> String -> [String] -> [PreprocessorToken] -> MacroEnv
addFunctionDefineVal [] macro params val       = [FunctionMap macro params val]
addFunctionDefineVal (m:renv) macro params val =   if (getMacroName m) == macro
                                                    then
                                                        (FunctionMap macro params val):renv
                                                    else
                                                        m:(addFunctionDefineVal renv macro params val)

-- | Removes a macro value from this environment. Does nothing if the
-- | macro is not defined in the environment.
removeDefineVal :: MacroEnv -> String -> MacroEnv
removeDefineVal [] _            = []
removeDefineVal (m:renv) macro  =   if (getMacroName m) == macro
                                    then
                                        renv
                                    else
                                        m:(removeDefineVal renv macro)
