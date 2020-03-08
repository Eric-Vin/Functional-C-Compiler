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

---------------------------------------------------------------------------------------------------
generateTestSuite :: TestSuite -> TestTree
generateTestSuite test_suite = testGroup "Tests" (map generateTestGroup (testGroups test_suite))

generateTestGroup :: TestGroup -> TestTree
generateTestGroup test_group = testGroup (testGroupName test_group) test_tree
    where
        test_tree = map generateTests (groupTests test_group)

generateTests :: Test -> TestTree
generateTests test = (goldenVsFile (testName test) (testGoldenPath test) (testOutputPath test) (savePreprocessed (testInputPath test)))
---------------------------------------------------------------------------------------------------

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

{-
instance J.JSON TestType where
  showJSON Preprocessor     = J.showJSON "Preprocessor"
  showJSON Output           = J.showJSON "Output"

  readJSON j = toTestType <$> jsonToStr j
    where
      toTestType "Preprocessor" = Preprocessor
      toTestType "Output"       = Output

instance J.JSON Test where
  showJSON (Test{..}) = J.JSObject (J.toJSObject keys)
    where
      keys = [ ("name",    J.showJSON testName)
             , ("input",   J.showJSON testInputPath)
             , ("output",  J.showJSON testOutputPath)
             , ("golden")  J.showJSON testGoldenPath
             , ("type",    J.showJSON testType)
             ]

  readJSON (J.JSObject o) =
    Test
    <$> (get "name"   >>= jsonToStr)
    <*> (get "input"  >>= jsonToStr)
    <*> (get "output" >>= jsonToStr)
    <*> (get "golden" >>= jsonToStr)
    <*> (get "type"   >>= 
  readJSON _ = J.Error "error" -}

---------------------------------------------------------------------------------------------------
parseTestFiles :: IO TestSuite
parseTestFiles = do
                    test_file_paths <- findByExtension [".json"] "test/test_files/"
                    let test_groups = [parseTestFile test_file_path | test_file_path <- test_file_paths]
                    return $ TestSuite test_groups

parseTestFile :: FilePath -> IO TestGroup
parseTestFile file_path = do
                            d <- (eitherDecode <$> (getJSON file_path)) :: IO (Either String [Test])
                            let tests = case d of
                                            Left err -> error err
                                            Right ps -> ps
                            return (TestGroup (takeBaseName file_path) tests)

getJSON :: FilePath -> IO B.ByteString
getJSON jsonFile = B.readFile jsonFile