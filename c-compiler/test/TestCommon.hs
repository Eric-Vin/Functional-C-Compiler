{-# LANGUAGE RecordWildCards #-}
module TestCommon where 

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsFile, goldenVsString, findByExtension)
import Test.Tasty.ExpectedFailure

import System.FilePath (takeBaseName)

import Data.Aeson
import Data.Text hiding (map)
import qualified Data.ByteString.Lazy as B

import Compiler

---------------------------------------------------------------------------------------------------
--Test Datatypes
---------------------------------------------------------------------------------------------------
data TestSuite =  TestSuite {
                              testGroups       :: [TestGroup]
                            }

data TestGroup =  TestGroup {
                              testGroupName    :: String
                            , groupTests       :: [Test]
                            }

data Test      =       Test {
                              testName         :: String
                            , testInputPath    :: FilePath
                            , testOutputPath   :: FilePath
                            , testGoldenPath   :: FilePath
                            , testType         :: TestType
                            , testError        :: Bool
                            }

data TestType = Preprocessor | Output


---------------------------------------------------------------------------------------------------
--Functions for generating a TestTree from the Test Datatypes
---------------------------------------------------------------------------------------------------

--Generates TestTrees for all Tests in a TestSuite and merges them into a TestTree
generateTestSuite :: TestSuite -> TestTree
generateTestSuite test_suite = testGroup "Tests" (map generateTestGroup (testGroups test_suite))

--Generates TestTrees for all Tests in a TestGroup and merges them into a TestTree
generateTestGroup :: TestGroup -> TestTree
generateTestGroup test_group = testGroup (testGroupName test_group) test_tree
    where
        test_tree = map generateTests (groupTests test_group)

--Generates a TestTree from a Test
generateTests :: Test -> TestTree
generateTests test    = case (testError test) of
                            True  -> generateErrorTest test
                            False -> generateNormalTest test

generateNormalTest :: Test -> TestTree
generateNormalTest test   = case (testType test) of
                                Preprocessor -> (goldenVsFile test_name golden_path output_path (savePreprocessed input_path output_path))
                                otherwise    -> error "Unsupported TestType"
                            where
                                test_name   = testName test
                                input_path  = testInputPath test
                                golden_path = testGoldenPath test
                                output_path = testOutputPath test

generateErrorTest :: Test -> TestTree
generateErrorTest test    = case (testType test) of
                                Preprocessor -> expectFail (goldenVsString test_name golden_path  (errorTestFunctionWrapper input_path output_path savePreprocessed))
                                otherwise    -> error "Unsupported TestType"
                            where
                                test_name   = testName test
                                input_path  = testInputPath test
                                golden_path = "test/golden/error/error.err"
                                output_path = testOutputPath test

---------------------------------------------------------------------------------------------------
--Functions for parsing test/test_files/ for all test file JSONS
---------------------------------------------------------------------------------------------------

--Parses all test file JSONS in test/test_files and returns a TestSuite data type containing that information
parseTestFiles :: IO TestSuite
parseTestFiles = do
                    test_file_paths <- findByExtension [".json"] "test/test_files/"
                    test_groups <- (sequence [parseTestFile test_file_path | test_file_path <- test_file_paths])
                    return $ TestSuite test_groups

--Parses a test file and returns a TestGroup data type containing that information
parseTestFile :: FilePath -> IO TestGroup
parseTestFile file_path = do
                            decode <- (eitherDecode <$> (getJSON file_path)) :: IO (Either String [Test])
                            let tests = case decode of
                                            Left err -> error err
                                            Right test -> test
                            return (TestGroup (takeBaseName file_path) tests)

--Given a file path, reads it as a ByteString
getJSON :: FilePath -> IO B.ByteString
getJSON jsonFile = B.readFile jsonFile

---------------------------------------------------------------------------------------------------
--FromJSON Instance | Instructions for converting a JSON into a list of Test datatypes
---------------------------------------------------------------------------------------------------
instance FromJSON Test where
  parseJSON = withObject "test" $ \o -> do
    testName        <- o .: pack "Name"
    testInputPath   <- o .: pack "Input"
    testOutputPath  <- o .: pack "Output"
    testGoldenPath  <- o .: pack "Golden"
    stringTestType  <- o .: pack "Type"
    
    let testType = case stringTestType of
                        "Preprocessor" -> Preprocessor
                        "Output"       -> Output
                        otherwise      -> error "Invalid testType value"

    errorTestType   <- o .: pack "Error"

    let testError = case errorTestType of
                        "True"         -> True
                        "False"        -> False
                        otherwise      -> error "Invalid testError value"

    return Test{..}

---------------------------------------------------------------------------------------------------
--Error Test Functions
---------------------------------------------------------------------------------------------------
-- | Takes an input path, an output path and a test function that takes those two paramaters and
-- | runs the function. Then returns error_code loaded from the test error file. This is used to 
-- | trigger errors.
errorTestFunctionWrapper :: FilePath -> FilePath -> (FilePath -> FilePath -> IO ()) -> IO B.ByteString
errorTestFunctionWrapper in_path out_path test_func =   do
                                                            test_func in_path out_path                              -- Runs the test function

                                                            error_code <- B.readFile "test/golden/error/error.err"  -- Loads the test error file 

                                                            return error_code
