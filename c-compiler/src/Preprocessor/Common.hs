module Preprocessor.Common where 

import Data.List

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
                        
instance Show PreprocessorToken where
    show (Directive directive)          = show directive
    show (Punctuator value)             = value
    show (Identifier value)             = value
    show (PreprocessingNumber value)    = value
    show (StringLiteral value)          = "\"" ++ value ++ "\""
    show (Other value)                  = value

-- | Datatype representing a Preprocessor Directive
data PreprocessorDirective  = Include String
                            | Define String String
                            | Undefine String
                            deriving (Show)