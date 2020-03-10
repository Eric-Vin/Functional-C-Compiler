{-# LANGUAGE RecordWildCards #-}
module TestCommon where 

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsFile, findByExtension)

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
generateTests test = (goldenVsFile (testName test) (testGoldenPath test) (testOutputPath test) (savePreprocessed (testInputPath test)))

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
                        otherwise      -> error "Invalid test file"
    return Test{..}
