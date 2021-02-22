{-# LANGUAGE RecordWildCards #-}
module TestCommon where 

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsFileDiff, findByExtension)
import Test.Tasty.HUnit (testCase, assertBool)

import System.FilePath (takeBaseName, takeDirectory, dropExtension)
import System.Directory (createDirectoryIfMissing)
import System.Process
import System.IO
import System.Exit

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
                                Preprocessor -> (goldenVsFileDiff test_name (\ref new -> ["diff", "-u", ref, new]) golden_path output_path (runCompileTest params True))
                                _            -> error "Unsupported TestType"
                            where
                                test_name   = testName test
                                input_path  = testInputPath test
                                golden_path = testGoldenPath test
                                output_path = testOutputPath test
                                params      = CompilerParams input_path output_path True

generateErrorTest :: Test -> TestTree
generateErrorTest test    = case (testType test) of
                                Preprocessor -> testCase test_name (checkTestError params "(Preprocessor Error)")
                                _            -> error "Unsupported TestType"
                            where
                                test_name   = testName test
                                input_path  = testInputPath test
                                output_path = testOutputPath test
                                params      = CompilerParams input_path output_path True

runCompileTest :: CompilerParams -> Bool -> IO ()
runCompileTest params raise_err     = do
                                        stdout_handle <- openFile (output_base_path ++ ".out") WriteMode
                                        stderr_handle <- openFile (output_base_path ++ ".err") WriteMode
                                        let process_params = CreateProcess (ShellCommand command) Nothing Nothing Inherit (UseHandle stdout_handle) (UseHandle stderr_handle) False False False False False False Nothing Nothing False
                                        (_,_,_, phandle)    <- createProcess process_params
                                        exit_code           <- waitForProcess phandle
                                        hClose stdout_handle
                                        hClose stderr_handle
                                        err_file            <- readFile (output_base_path ++ ".err")
                                        if (exit_code == ExitSuccess) || (not raise_err)
                                        then 
                                            return ()
                                        else
                                            errorWithoutStackTrace err_file
                                    where
                                        output_base_path    = dropExtension $ outputFilePath params
                                        pp_flag             =   if savePreprocessed params
                                                                then
                                                                    "-p"
                                                                else
                                                                    ""
                                        command             = "stack exec -- c-compiler-exe " ++ (inputFilePath params) ++ " -o " ++ (outputFilePath params) ++ " " ++ pp_flag

checkTestError :: CompilerParams -> String -> IO ()
checkTestError params err_msg   = do
                                    runCompileTest params False
                                    err_file    <- readFile (output_base_path ++ ".err")
                                    let msg_exist_check = (not $ Prelude.null err_file)
                                    assertBool ("The test did not throw an error.") msg_exist_check
                                    let msg_val_check = (isInfixOf (pack err_msg) (pack err_file))
                                    assertBool ("The file \"" ++ (output_base_path ++ ".err") ++ "\" does not contain " ++ err_msg ++"\nError Message: " ++ err_file) msg_val_check
                                where
                                    output_base_path    = dropExtension $ outputFilePath params

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
                            parsed_file <- (eitherDecode <$> (getJSON file_path)) :: IO (Either String [Test])
                            let tests = case parsed_file of
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
                        _              -> error "Invalid testType value"

    errorTestType   <- o .: pack "Error"

    let testError = case errorTestType of
                        "True"         -> True
                        "False"        -> False
                        _              -> error "Invalid testError value"

    return Test{..}

---------------------------------------------------------------------------------------------------
--Error Test Functions
---------------------------------------------------------------------------------------------------
-- | Takes an input path, an output path and a test function that takes those two paramaters and
-- | runs the function. Then returns error_code loaded from the test error file. This is used to 
-- | trigger errors.
errorTestFunctionWrapper :: IO () -> IO B.ByteString
errorTestFunctionWrapper test_func  =   do
                                        test_func                                               -- Runs the test function

                                        error_code <- B.readFile "test/golden/error/error.err"  -- Loads the test error file 

                                        return error_code

---------------------------------------------------------------------------------------------------
-- Utility Functions
---------------------------------------------------------------------------------------------------
createOutputDirTree :: TestSuite -> IO ()
createOutputDirTree test_suite  = sequence_ $ map createOutputDirGroup (testGroups test_suite)
                                where
                                    createOutputDirGroup :: TestGroup -> IO ()
                                    createOutputDirGroup test_group = sequence_ $ map createOutputDirTest (groupTests test_group)

                                    createOutputDirTest :: Test -> IO ()
                                    createOutputDirTest test    = createDirectoryIfMissing True (takeDirectory $ testOutputPath test)
