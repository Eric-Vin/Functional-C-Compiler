module Utilities.Error where

data CompilerError  = PreprocessorError String 
                    | LexerError String
                    | ParserError String

instance Show CompilerError where
    show (PreprocessorError msg)    = "(Preprocessor Error) " ++ msg
    show (LexerError msg)           = "(Lexer Error) " ++ msg
    show (ParserError msg)          = "(Parser Error) " ++ msg


throwCompilerError :: CompilerError -> a0
throwCompilerError compiler_error = error (show compiler_error)
