module Utilities.SourceTracker where

---------------------------------------------------------------------------------------------------
-- | SourceTracker Datatype and Functions
---------------------------------------------------------------------------------------------------
data SourceTracker  = SourceTracker {
                       sourcePath  :: FilePath
                     , lineStart   :: Int
                     , lineEnd     :: Int
                     , columnStart :: Int
                     , columnEnd   :: Int
                    }