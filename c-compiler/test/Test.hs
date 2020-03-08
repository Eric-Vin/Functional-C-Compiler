module Main where

import Test.Tasty (defaultMain)

import TestCommon

main :: IO ()
main = do
		b <- parseTestFiles--defaultMain $ generateTestSuite (TestSuite [(TestGroup "DemoGroup" [(Test "DemoTest" "test/input_files/return_2.c" "test/output_files/return_2.pp" "test/golden_files/return_2.pp" Preprocessor)])])