{-# LANGUAGE FlexibleInstances #-}

module Preprocessor.Scanner where 

import Control.Applicative
import Data.Monoid
import Data.Foldable (asum)
import Data.Char (isLetter, isDigit, isSpace)

import Utilities.Error
import Utilities.Language (punctuators_list)

import Preprocessor.Common

import Debug.Trace

---------------------------------------------------------------------------------------------------
-- | Scanner Main Functons
---------------------------------------------------------------------------------------------------

-- | Tokenizes passed string into preprocessor tokens
tokenize :: String -> PreprocessedFile
tokenize input = case (runScanner $ many (tryScanInvisibleSpace *> scanToken <* tryScanInvisibleSpace)) input of
                    Left compiler_error -> throwCompilerError compiler_error
                    Right ("", tokens)  -> PreprocessedFile tokens
                    Right (input', _)   -> throwCompilerError $ PreprocessorError $ "Not all input parsed, \"" ++ input' ++ "\" remains.)"

scanToken :: Scanner PreprocessorToken
scanToken = scanDirective <|> scanPunctuator <|> 
            scanIdentifier <|> scanPreprocessingNumber <|> 
            scanStringLiteral <|> scanOther

scanDirective :: Scanner PreprocessorToken
scanDirective   = Directive <$> ((scanString "#") *> (scanInclude))

scanPunctuator :: Scanner PreprocessorToken
scanPunctuator  = Punctuator <$> (asum $ map scanString punctuators_list)

scanIdentifier :: Scanner PreprocessorToken
scanIdentifier  = Identifier <$> ((++) <$> scan_head_char <*> (concat <$> many scan_tail_char))
                where
                    scan_head_char = (scanSingle isLetter) <|> (scanString "_")
                    scan_tail_char =  scan_head_char <|> (scanSingle isDigit)


scanPreprocessingNumber :: Scanner PreprocessorToken
scanPreprocessingNumber  = PreprocessingNumber <$> scan_optional_period
                        where
                            exponents_list          = ["e+", "e-", "E+", "E-", "p+", "p-", "P+", "P-"]
                            scan_exponents          = (asum $ map scanString exponents_list)

                            scan_optional_period    = ((++) <$> (tryScanString ".")  <*> scan_mandatory_digit)
                            scan_mandatory_digit    = ((++) <$> (scanSingle isDigit) <*> (concat <$> many scan_tail))
                            scan_tail               = scan_exponents <|> 
                                                      (scanSpanChar isDigit) <|> (scanSpanChar isLetter) <|>
                                                      (scanString "_") <|> (scanString ".")
                                                      
scanStringLiteral :: Scanner PreprocessorToken
scanStringLiteral   = StringLiteral <$> (scan_single_quote <|> scan_double_quote)
                    where
                        scan_single_quote   = scanChar '\'' *>  (concat <$> many scan_single_quote_inner) <* scanChar '\''
                        scan_double_quote   = scanChar '"'  *> (concat <$> many scan_double_quote_inner) <* scanChar '"'

                        scan_single_quote_inner = (scanString "\\'")   <|> scan_escape_sequences <|> (scanSpanChar $ getAll . ((All . (/= '\'')) <> (All . (/='\\'))))
                        scan_double_quote_inner = (scanString "\\\"")  <|> scan_escape_sequences <|> (scanSpanChar $ getAll . ((All . (/= '"'))  <> (All . (/='\\'))))

                        -- | Covers all escape sequences except the appropriate Char/String delimeter
                        -- | NOTE: Add support for remaining escape sequences
                        scan_escape_sequences = (scanString "\\\\")

scanOther :: Scanner PreprocessorToken
scanOther  = Other <$> scanSingle (const True)

---------------------------------------------------------------------------------------------------
-- | Scan Directive Functons
---------------------------------------------------------------------------------------------------

-- | Attempts to scan an Include directive.
-- | If it cannot scan a valid Include directive, returns a left value.
scanInclude :: Scanner PreprocessorDirective
scanInclude = Include <$> (scan_directive_type *> tryScanInvisibleSpace *> scan_include_string_literal <* tryScanInvisibleSpace <* scanString "\n")
            where
                scan_directive_type = scanString "include"

                scan_include_string_literal = (scan_double_quote <|> scan_angle_brackets)

                scan_double_quote   = scanChar '"'  *> scan_string_inner_quote <* scanChar '"'
                scan_angle_brackets = scanChar '<'  *> scan_string_inner_brackets <* scanChar '>'

                scan_string_inner_quote     = tryScanSpanChar (/= '"') 
                scan_string_inner_brackets  = tryScanSpanChar (/= '>') 


---------------------------------------------------------------------------------------------------
-- | General Scanning Functons
---------------------------------------------------------------------------------------------------

-- | Creates a Scanner that will attempt to scan a line continuation.
-- | Otherwise returns a Left value.
scanBackslashNewline :: Scanner String
scanBackslashNewline    = Scanner f
                        where
                            f i@('\\':'\n':xs)  = Right (xs, "\\\n")
                            f i@(x:xs)          = Left $ PreprocessorError $ "Expected \"\\\n\" but got \"" ++ (take 2 i) ++ "\""
                            f []                = Left $ PreprocessorError $ "Expected \"\\\n\" but reached end of input"

-- | Creates a Scanner that will attempt to all whitespace and comments possible.
-- | NOTE: This function never returns a Left value, only an empty list.
tryScanInvisibleSpace :: Scanner String
tryScanInvisibleSpace = scanInvisibleSpace <|> pure []

-- | Creates a Scanner that will attempt to all whitespace and comments possible.
-- | NOTE: This function never returns a Left value, only an empty list.
scanInvisibleSpace :: Scanner String
scanInvisibleSpace = (concat <$> many (scanComment <|> scanWhitespace))

-- | Creates a Scanner that will attempt to scan a comment from start to end, excluding newline.
-- | Otherwise returns a Left value.
scanComment :: Scanner String
scanComment  = scanLineComment <|> scanBlockComment
                where
                    scanLineComment :: Scanner String
                    scanLineComment = scanString "//" *> tryScanSpanChar (/= '\n') 

                    scanBlockComment :: Scanner String
                    scanBlockComment = scanString "/*" *> scanSpanString (\input -> (take 2 input) /= "*/") <* scanString "*/"

-- | Creates a Scanner that will attempt to scan all whitespace characters apart from newline (\n).
-- | If nothing is scanned returns a Left value.
scanWhitespace :: Scanner String
scanWhitespace = scanSpanChar (getAll . ((All . isSpace) <> (All . (/='\n'))))




-- | Creates a Scanner that will attempt to scan Chars as long as the passed function is 
-- | satisfied by the remaining part of the list.
-- | NOTE: This function never returns a Left value, only an empty list.
tryScanSpanString :: (String -> Bool) -> Scanner String
tryScanSpanString f = scanSpanString f <|> pure []

-- | Creates a Scanner that will attempt to scan Chars as long as the passed function is satisfied.
-- | NOTE: This function never returns a Left value, only an empty list.
tryScanSpanChar :: (Char -> Bool) -> Scanner String
tryScanSpanChar f  = scanSpanChar f <|> pure []

-- | Creates a Scanner that will attempt to scan the passed String.
-- | Otherwise returns a Left value.
tryScanString :: String -> Scanner String
tryScanString string = scanString string <|> pure []




-- | Creates a Scanner that will attempt to scan Chars as long as the passed function is satisfied.
-- | If nothing is scanned returns a left value
scanSpanChar :: (Char -> Bool) -> Scanner String
scanSpanChar f  = scanSpanString (f . head)

-- | Creates a Scanner that will attempt to scan the passed String.
-- | Otherwise returns a Left value.
scanString :: String -> Scanner String
scanString string = Scanner $ \input -> 
                        case runScanner (traverse scanChar string) input of
                            Left _ -> Left $ PreprocessorError $ "Expected \"" ++ string ++ "\" but got \"" ++ (take (length string) input) ++ "\""
                            token  -> token

-- | Creates a Scanner that will attempt to scan Chars as long as the passed function is 
-- | satisfied by the remaining part of the list.
-- | If nothing is scanned returns a left value.
scanSpanString :: (String -> Bool) -> Scanner String
scanSpanString f  = Scanner $ scan
            where
                    scan :: String -> Either CompilerError (String, String)
                    scan input  | length token == 0   = Left $ PreprocessorError $ "\"" ++ (fst $ span (/= '\n') input) ++ "\" does not fulfill the scan function"
                                | otherwise           = Right (input', token)
                                where
                                    (input', token, rows, columns) = spanString f input

                    spanString :: (String -> Bool) -> String -> (String, String, Int, Int)
                    spanString _ [] = ([],[], 0, 0)
                    spanString func i@(x:xs)    | take 2 i == "\\\n"    = trimContinue
                                                | func i                = (input', x:token, rows+row_inc, columns+col_inc)
                                                | otherwise             = (i, [], 0, 0)
                                                where 
                                                    (input',token, rows, columns) = spanString func xs

                                                    row_inc = if (x == '\n') then 1 else 0
                                                    col_inc = if (row_inc == 0) then 1 else 0  

                                                    trimContinue :: (String, String, Int, Int)
                                                    trimContinue    = (t_input, t_token, t_rows+1, t_colums+1)
                                                                    where
                                                                        (t_input, t_token, t_rows, t_colums) = spanString func (drop 2 i)

-- | Creates a Scanner that will attempt to scan a single Char if it fulfills the function passed. 
-- | Otherwise returns a Left value.
scanSingle :: (Char -> Bool) -> Scanner String
scanSingle f  = (many scanBackslashNewline) *> (Scanner scan)
            where
                    scan :: String -> Either CompilerError (String, String)
                    scan []    = Left $ PreprocessorError $ "Empty input"
                    scan input = return_value
                                where
                                    target_char  = head input
                                    return_value    | f target_char = Right $ (tail input, [target_char])
                                                    | otherwise     = Left $ PreprocessorError $ "'" ++ [target_char] ++ "' does not fulfill the scan function"

-- | Creates a Scanner that will attempt to scan the passed Char
-- | Otherwise returns a Left value.
scanChar :: Char -> Scanner Char
scanChar char   = (many scanBackslashNewline) *> (Scanner f)
                where
                        f (x:xs)    | x == char = Right (xs, x)
                                    | otherwise = Left $ PreprocessorError $ "Expected '" ++ [char] ++ "' but got '" ++ [x] ++ "'"
                        f []        = Left $ PreprocessorError $ "Expected '" ++ [char] ++ "' but reached end of input"

---------------------------------------------------------------------------------------------------
-- | Scanner Datatypes
---------------------------------------------------------------------------------------------------

newtype Scanner s = Scanner {runScanner :: String -> Either CompilerError (String, s)}

instance Functor Scanner where
    fmap f (Scanner s) = Scanner $ \input -> do
                                        (input', token) <- s input
                                        return (input', f token)

instance Applicative Scanner where
    pure x = Scanner $ \input -> Right (input, x)

    (Scanner s1) <*> (Scanner s2) = Scanner $ \input -> do
                                                    (input', f)  <- s1 input
                                                    (input'', a) <- s2 input'
                                                    return (input'', f a)

instance Alternative (Either CompilerError) where
    empty = Left $ PreprocessorError "Alternative Empty"

    Left _ <|> s2   = s2

    s1 <|> _        = s1

instance Alternative Scanner where
    empty = Scanner $ \_ -> empty

    (Scanner s1) <|> (Scanner s2) = Scanner $ \input -> s1 input <|> s2 input
