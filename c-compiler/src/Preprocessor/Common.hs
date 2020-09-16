module Preprocessor.Common where 

---------------------------------------------------------------------------------------------------
-- | Preprocessing Datatypes
---------------------------------------------------------------------------------------------------

-- | Datatype representing a list of Preprocessor Tokens
type PreprocessedFile   = [PreprocessorToken]

-- | Datatype representing one Preprocessor token
data PreprocessorToken  = Identifier String
                        | PreprocessingNumber String
                        | StringLiteral String
                        | Punctuator String
                        | Other String
                        deriving (Show)
