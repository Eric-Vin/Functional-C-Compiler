module Error where

data CompilerError  = PreprocessorError SourceTracker String 
                    | LexerError SourceTracker String
                    | ParserError SourceTracker String