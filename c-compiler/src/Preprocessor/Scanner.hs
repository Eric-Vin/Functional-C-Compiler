{-# LANGUAGE FlexibleInstances #-}

module Preprocessor.Scanner where 

import Control.Applicative
import Data.Monoid
import Data.Foldable (asum)
import Data.Char (isLetter, isDigit, isSpace)

import Utilities.Error
import Utilities.Language (punctuators_list)

import Preprocessor.Common

---------------------------------------------------------------------------------------------------
-- | Scanner Main Functons
---------------------------------------------------------------------------------------------------

-- | Tokenizes passed string into preprocessor tokens
tokenize :: String -> PreprocessedFile
tokenize input_text = undefined

scanToken :: Scanner PreprocessorToken
scanToken = undefined

scanDirective :: Scanner PreprocessorToken
scanDirective = undefined 

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
                                                      (scanSpan isDigit) <|> (scanSpan isLetter) <|>
                                                      (scanString "_") <|> (scanString ".")
                                                      
scanStringLiteral :: Scanner PreprocessorToken
scanStringLiteral   = StringLiteral <$> (scan_single_quote <|> scan_double_quote)
                    where
                        scan_single_quote   = scanChar '\'' *>  (concat <$> many scan_single_quote_inner) <* scanChar '\''
                        scan_double_quote   = scanChar '"'  *> (concat <$> many scan_double_quote_inner) <* scanChar '"'

                        scan_single_quote_inner = (scanString "\\'")   <|> scan_escape_sequences <|> (scanSpan $ getAll . ((All . (/= '\'')) <> (All . (/='\\'))))
                        scan_double_quote_inner = (scanString "\\\"")  <|> scan_escape_sequences <|> (scanSpan $ getAll . ((All . (/= '"'))  <> (All . (/='\\'))))

                        -- | Covers all escape sequences except the appropriate Char/String delimeter
                        -- | NOTE: Add support for remaining escape sequences
                        scan_escape_sequences = (scanString "\\\\")

scanOther :: Scanner PreprocessorToken
scanOther  = scanSingle (const True)

---------------------------------------------------------------------------------------------------
-- | Scanner Helper Functons
---------------------------------------------------------------------------------------------------

-- | Creates a Scanner that will attempt to scan all whitespace characters apart from newline (\n).
-- | NOTE: This function never returns a Left value, only an empty list.
tryScanWhitespace :: Scanner String
scanWhitespace = tryScanSpan ($ getAll . ((All . isSpace) <> (All . (/='\n'))))

-- | Creates a Scanner that will attempt to scan Chars as long as the passed function is satisfied.
-- | NOTE: This function never returns a Left value, only an empty list.
tryScanSpan :: (Char -> Bool) -> Scanner String
tryScanSpan f  = Scanner $ scan
            where
                    scan :: String -> Either CompilerError (String, String)
                    scan input = Right (input', token)
                                where
                                    (token, input') = span f input

-- | Creates a Scanner that will attempt to scan Chars as long as the passed function is satisfied.
-- | If nothing is scanned returns a left value
scanSpan :: (Char -> Bool) -> Scanner String
scanSpan f  = Scanner $ scan
            where
                    scan :: String -> Either CompilerError (String, String)
                    scan input  | length(token) == 0    = Left $ PreprocessorError $ "'" ++ [head input] ++ "' does not fulfill the scan function"
                                | otherwise             = Right (input', token)
                                where
                                    (token, input') = span f input

-- | Creates a Scanner that will attempt to scan a single Char if it fulfills the function passed. 
-- | Otherwise returns a Left value.
scanSingle :: (Char -> Bool) -> Scanner String
scanSingle f  = Scanner $ scan
            where
                    scan :: String -> Either CompilerError (String, String)
                    scan []    = Left $ PreprocessorError $ "Empty input"
                    scan input = return_value
                                where
                                    target_char  = head input
                                    return_value    | f target_char = Right $ (tail input, [target_char])
                                                    | otherwise     = Left $ PreprocessorError $ "'" ++ [target_char] ++ "' does not fulfill the scan function"

-- | Creates a Scanner that will attempt to scan the passed String.
-- | Otherwise returns a Left value.
tryScanString :: String -> Scanner String
tryScanString string = Scanner $ \input -> 
                        case runScanner (traverse scanChar string) input of
                            Left _ -> Right (input, [])
                            token  -> token


-- | Creates a Scanner that will attempt to scan the passed String.
-- | Otherwise returns a Left value.
scanString :: String -> Scanner String
scanString string = Scanner $ \input -> 
                        case runScanner (traverse scanChar string) input of
                            Left _ -> Left $ PreprocessorError $ "Expected \"" ++ string ++ "\" but got \"" ++ (take (length string) input) ++ "\""
                            token  -> token

-- | Creates a Scanner that will attempt to scan the passed Char
-- | Otherwise returns a Left value.
scanChar :: Char -> Scanner Char
scanChar char   = Scanner f
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
