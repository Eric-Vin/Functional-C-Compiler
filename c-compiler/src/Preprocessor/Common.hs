module Preprocessor.Common where 

---------------------------------------------------------------------------------------------------
-- | Preprocessing Datatypes
---------------------------------------------------------------------------------------------------

-- | Datatype representing a list of Preprocessor Tokens
type PreprocessedFile   = [PreprocessorToken]

-- | Datatype representing one Preprocessor token
data PreprocessorToken  = Directive PreprocessorDirective
                        | Punctuator String
                        | Identifier String
                        | PreprocessingNumber String
                        | StringLiteral String
                        | Other String
                        deriving (Show)

-- | Datatype representing a Preprocessor Directive
data PreprocessorDirective  = Include String
                            | Define String String
                            | Undefine String
                            deriving (Show)