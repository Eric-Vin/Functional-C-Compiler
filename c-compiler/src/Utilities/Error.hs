module Utilities.Error where

data CompilerError  = PreprocessorError String 
                    | LexerError String
                    | ParserError String
                    deriving (Show)