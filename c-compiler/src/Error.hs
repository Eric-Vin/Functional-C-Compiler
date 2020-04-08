module Error where

data ErrorType = Preprocessor | Lexer | Parser

-- throwCompilerError :: ErrorType -> String
throwCompilerError Preprocessor st message = errorWithoutStackTrace $ "Preprocessor Error: " ++ message ++ " @ " ++ (show st)