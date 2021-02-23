{-# LANGUAGE FlexibleInstances #-}

module Preprocessor.Scanner where 

import Control.Applicative
import Data.Monoid
import Data.Foldable (asum)
import Data.Char (isLetter, isDigit, isSpace)
import Data.Either (isLeft, isRight)

import Utilities.Error
import Utilities.Language (punctuators_list)

import Preprocessor.Common

---------------------------------------------------------------------------------------------------
-- | Scanner Main Functons
---------------------------------------------------------------------------------------------------

-- | Tokenizes passed string into preprocessor tokens
tokenize :: String -> [PreprocessorToken]
tokenize input = case (runScanner $ many (tryScanInvisibleSpace *> scanToken <* tryScanInvisibleSpace)) input of
                    Left compiler_error -> throwCompilerError compiler_error
                    Right ("", tokens)  -> tokens
                    Right (input', _)   -> throwCompilerError $ PreprocessorError $ "Not all input parsed, \"" ++ input' ++ "\" remains.)"

scanToken :: Scanner PreprocessorToken
scanToken = scanDirective <|> scanPunctuator <|> 
            scanIdentifier <|> scanPreprocessingNumber <|> 
            scanStringLiteral <|> scanOther

scanDirective :: Scanner PreprocessorToken
scanDirective   = Directive <$> ((scanString "#") *> tryScanInvisibleSpace *> directive_scan_list)
                where
                    directive_scan_list =   scanInclude <|> scanDefine <|> scanUndefine <|> 
                                            scanIfdef <|> scanIfndef <|> scanEndif

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
                        scan_single_quote   = (\x -> "'" ++ x ++ "'") <$> (scanChar '\'' *>  (concat <$> many scan_single_quote_inner) <* scanChar '\'')
                        scan_double_quote   = (\x -> "\"" ++ x ++ "\"") <$> (scanChar '"'  *> (concat <$> many scan_double_quote_inner) <* scanChar '"')

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
scanInclude =   (uncurry Include) <$> (Scanner $ scan)
            where
                scan_directive_type = scanString "include"

                scan_string_inner_quote     = tryScanSpanChar (/= '"') 
                scan_string_inner_brackets  = tryScanSpanChar (/= '>') 

                scan_double_quote   = scanChar '"'  *> scan_string_inner_quote <* scanChar '"'
                scan_angle_brackets = scanChar '<'  *> scan_string_inner_brackets <* scanChar '>'

                scan_file_include = scan_directive_type *> tryScanInvisibleSpace *> scan_double_quote <* tryScanInvisibleSpace <* scanString "\n"
                scan_lib_include = scan_directive_type *> tryScanInvisibleSpace *> scan_angle_brackets <* tryScanInvisibleSpace <* scanString "\n"

                full_directive_scan = scan_file_include <|> scan_lib_include

                scan :: String -> Either CompilerError (String, (IncludeType, String)) 
                scan input  =   if (isRight $ runScanner scan_directive_type input) && (isLeft $ runScanner full_directive_scan input)
                                then
                                    throwCompilerError $ PreprocessorError "Include directive was of incorrect form"
                                else
                                        ((\(x,y) -> (x, (File, y))) <$> runScanner scan_file_include input) 
                                    <|> ((\(x,y) -> (x, (Library, y))) <$> runScanner scan_lib_include input)

-- | Attempts to scan a Define directive.
-- | If it cannot scan a valid Define directive, returns a left value.
-- | NOTE: Order of scanning is important, as if scanObjectDefine cannot
-- |       scan the directive it throws an error.
scanDefine :: Scanner PreprocessorDirective
scanDefine = scanObjectDefine

-- | Attempts to scan a Function-like Define directive.
-- | If it cannot scan a valid Function-like Define directive, returns a left value.
scanFunctionDefine :: Scanner PreprocessorDirective
scanFunctionDefine  = undefined

-- | Attempts to scan an Object-like Define directive.
-- | If it cannot scan a valid Object-like Define directive, returns a left value.
scanObjectDefine :: Scanner PreprocessorDirective
scanObjectDefine    =   (uncurry ObjectDefine) <$> (defineTokToString <$> (Scanner $ scan))
                    where
                        scan_directive_type = scanString "define"

                        -- | Scans all other tokens apart from a newline character and whitespace.
                        scan_other_define = Other <$> (scanSingle $ getAll . ((All . (/= '\n')) <> (All . (not . isSpace))))

                        scan_define_token = scanPunctuator <|> scanIdentifier <|>
                                            scanPreprocessingNumber <|> scanStringLiteral <|>
                                            scan_other_define

                        scan_define_tokens = (many (tryScanInvisibleSpace *> scan_define_token <* tryScanInvisibleSpace)) <* scanString "\n"

                        scan_define_identifier = (\x y -> (x, y)) <$> scanIdentifier <*> scan_define_tokens

                        full_directive_scan = scan_directive_type *> tryScanInvisibleSpace *> scan_define_identifier

                        scan :: String -> Either CompilerError (String, (PreprocessorToken, [PreprocessorToken]))
                        scan input  =   if (isRight $ runScanner scan_directive_type input) && (isLeft $ runScanner full_directive_scan input)
                                        then
                                            throwCompilerError $ PreprocessorError "Define directive was of incorrect form"
                                        else
                                            runScanner full_directive_scan input

                        defineTokToString :: (PreprocessorToken, [PreprocessorToken]) -> (String, [PreprocessorToken])
                        defineTokToString (Identifier macro, toks)  = (macro, toks)

-- | Attempts to scan a Undefine directive.
-- | If it cannot scan a valid Undefine directive, returns a left value.
scanUndefine :: Scanner PreprocessorDirective
scanUndefine = Undefine <$> (undefineTokToString <$> (Scanner $ scan))
            where
                scan_directive_type = scanString "undef"

                full_directive_scan = scan_directive_type *> tryScanInvisibleSpace *> scanIdentifier

                scan :: String -> Either CompilerError (String, PreprocessorToken)
                scan input  =   if (isRight $ runScanner scan_directive_type input) && (isLeft $ runScanner full_directive_scan input)
                                then
                                    throwCompilerError $ PreprocessorError "Undefine directive was of incorrect form"
                                else
                                    runScanner full_directive_scan input

                undefineTokToString :: PreprocessorToken -> String
                undefineTokToString (Identifier macro)  = (macro)

-- | Attempts to scan an Ifdef directive.
-- | If it cannot scan a valid Ifdef directive, returns a left value.
scanIfdef :: Scanner PreprocessorDirective
scanIfdef   = Ifdef <$> (ifdefTokToString <$> (Scanner $ scan))
            where
                scan_directive_type = scanString "ifdef"

                full_directive_scan = scan_directive_type *> tryScanInvisibleSpace *> scanIdentifier <* tryScanInvisibleSpace <* scanString "\n"

                scan :: String -> Either CompilerError (String, PreprocessorToken)
                scan input  =   if (isRight $ runScanner scan_directive_type input) && (isLeft $ runScanner full_directive_scan input)
                                then
                                    throwCompilerError $ PreprocessorError "Ifdef directive was of incorrect form"
                                else
                                    runScanner full_directive_scan input

                ifdefTokToString :: PreprocessorToken -> String
                ifdefTokToString (Identifier macro)  = macro

-- | Attempts to scan an Ifndef directive.
-- | If it cannot scan a valid Ifndef directive, returns a left value.
scanIfndef :: Scanner PreprocessorDirective
scanIfndef   = Ifndef <$> (ifndefTokToString <$> (Scanner $ scan))
            where
                scan_directive_type = scanString "ifndef"

                full_directive_scan = scan_directive_type *> tryScanInvisibleSpace *> scanIdentifier <* tryScanInvisibleSpace <* scanString "\n"

                scan :: String -> Either CompilerError (String, PreprocessorToken)
                scan input  =   if (isRight $ runScanner scan_directive_type input) && (isLeft $ runScanner full_directive_scan input)
                                then
                                    throwCompilerError $ PreprocessorError "Ifdef directive was of incorrect form"
                                else
                                    runScanner full_directive_scan input

                ifndefTokToString :: PreprocessorToken -> String
                ifndefTokToString (Identifier macro)  = macro

-- | Attempts to scan an Endif directive.
-- | If it cannot scan a valid Endif directive, returns a left value.
scanEndif :: Scanner PreprocessorDirective
scanEndif   = (\_ -> Endif) <$> (Scanner $ scan)
            where
                scan_directive_type = scanString "endif"

                full_directive_scan = scan_directive_type <* tryScanInvisibleSpace <* scanString "\n"

                scan :: String -> Either CompilerError (String, String)
                scan input  =   if (isRight $ runScanner scan_directive_type input) && (isLeft $ runScanner full_directive_scan input)
                                then
                                    throwCompilerError $ PreprocessorError "Ifdef directive was of incorrect form"
                                else
                                    runScanner full_directive_scan input


---------------------------------------------------------------------------------------------------
-- | General Scanning Functons
---------------------------------------------------------------------------------------------------

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
scanSpanString f  = (Scanner scan)
            where
                    scan :: String -> Either CompilerError (String, String)
                    scan input  | length token == 0   = Left $ PreprocessorError $ "\"" ++ (fst $ span (/= '\n') input) ++ "\" does not fulfill the scan function"
                                | otherwise           = Right (input'', token)
                                where
                                    (input', trim_rows, trim_columns)           = trimContinue input
                                    (input'', token, tok_rows, tok_columns)     = spanString f input'

                    spanString :: (String -> Bool) -> String -> (String, String, Int, Int)
                    spanString _ [] = ([],[], 0, 0)
                    spanString func i@(x:_)     | func input'           = (input'', (head input'):token, trim_rows+rows+row_inc, trim_columns+columns+col_inc)
                                                | otherwise             = (i, [], 0, 0)
                                                where 
                                                    (input', trim_rows, trim_columns)   = trimContinue i
                                                    (input'', token, rows, columns)     = spanString func (tail input')

                                                    row_inc = if (x == '\n') then 1 else 0
                                                    col_inc = if (row_inc == 0) then 1 else 0

                    trimContinue :: String -> (String, Int, Int)
                    trimContinue input  = case runScanner scanBackslashNewline input of
                                            Left _                              -> (input, 0, 0)
                                            Right (rem_input, trimmed_input)    -> (rem_input, 1, (length trimmed_input) - 1)

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

-- | Creates a Scanner that will attempt to scan a line continuation.
-- | Otherwise returns a Left value.
-- | Note: This scanner does not depend on any other scanners, to avoid a circular dependency
scanBackslashNewline :: Scanner String
scanBackslashNewline    = (:) <$> (scanCharLim '\\' <* scanInvisibleSpaceLim) <*> ((:[]) <$> (scanCharLim '\n'))
                        where
                            scanInvisibleSpaceLim :: Scanner String
                            scanInvisibleSpaceLim = concat <$> (many (scanWhitespaceLim <|> scanLineCommentLim <|> scanBlockCommentLim))

                            scanWhitespaceLim :: Scanner String
                            scanWhitespaceLim = scanSingleLim (\x -> (isSpace x) && (x /= '\n'))

                            scanLineCommentLim :: Scanner String
                            scanLineCommentLim  = (++) <$> scanLineCommentOpen <*> (concat <$> (many $ scanSingleLim (/= '\n')))
                                                where
                                                    scanLineCommentOpen :: Scanner String
                                                    scanLineCommentOpen = (:) <$> (scanCharLim $ '/') <*> ((:[]) <$> (scanCharLim $ '/'))

                            scanBlockCommentLim :: Scanner String
                            scanBlockCommentLim = (++) <$> scanBlockCommentOpen <*> ((++) <$> scanBlockCommentInterior <*> scanBlockCommentClose)
                                                where
                                                    scanBlockCommentOpen :: Scanner String
                                                    scanBlockCommentOpen = (:) <$> (scanCharLim $ '/') <*> ((:[]) <$> (scanCharLim $ '*'))

                                                    scanBlockCommentClose :: Scanner String
                                                    scanBlockCommentClose = (:) <$> (scanCharLim $ '*') <*> ((:[]) <$> (scanCharLim $ '/'))

                                                    scanBlockCommentInterior :: Scanner String
                                                    scanBlockCommentInterior    = scanSpanStringLim (\x -> (take 2 x) /= "*/")

                            scanSpanStringLim :: (String -> Bool) -> Scanner String
                            scanSpanStringLim f  = (Scanner scan)
                                        where
                                                scan :: String -> Either CompilerError (String, String)
                                                scan input  | length token == 0   = Left $ PreprocessorError $ "\"" ++ (fst $ span (/= '\n') input) ++ "\" does not fulfill the scan function"
                                                            | otherwise           = Right (input', token)
                                                            where
                                                                (input', token, tok_rows, tok_columns)     = spanString f input

                                                spanString :: (String -> Bool) -> String -> (String, String, Int, Int)
                                                spanString _ [] = ([],[], 0, 0)
                                                spanString func i@(x:xs)    | func i                = (input', (head input'):token, rows+row_inc, columns+col_inc)
                                                                            | otherwise             = (i, [], 0, 0)
                                                                            where
                                                                                (input', token, rows, columns)     = spanString func xs

                                                                                row_inc = if (x == '\n') then 1 else 0
                                                                                col_inc = if (row_inc == 0) then 1 else 0

                            scanSingleLim :: (Char -> Bool) -> Scanner String
                            scanSingleLim f = (Scanner scan)
                                            where
                                                scan :: String -> Either CompilerError (String, String)
                                                scan []    = Left $ PreprocessorError $ "Empty input"
                                                scan input = return_value
                                                            where
                                                                target_char  = head input
                                                                return_value    | f target_char = Right $ (tail input, [target_char])
                                                                                | otherwise     = Left $ PreprocessorError $ "'" ++ [target_char] ++ "' does not fulfill the scan function"

                            scanCharLim :: Char -> Scanner Char
                            scanCharLim char    = (Scanner f)
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
