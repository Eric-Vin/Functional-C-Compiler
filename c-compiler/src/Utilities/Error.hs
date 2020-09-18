module Utilities.Error where

data CompilerError  = PreprocessorError String 
                    | LexerError String
                    | ParserError String
                    deriving (Show)
throwCompilerError :: CompilerError -> a0
throwCompilerError compiler_error = error (show compiler_error)
