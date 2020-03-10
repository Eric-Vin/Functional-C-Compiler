module Main where

import Test.Tasty (defaultMain)

import TestCommon

--Runs the Test Harness
main :: IO ()
main = do
        test_suite <- parseTestFiles
        defaultMain $ generateTestSuite test_suite