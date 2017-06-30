module Settings where

import System.FilePath

-------------------------------------------------------------------------------
-- Settings

data Settings = Settings {
      blackboxDB  :: FilePath    -- local sqlite db
    , binDataDir  :: FilePath    -- compressed code files
    , locDir      :: FilePath    -- cloc tool
    , locOutDir   :: FilePath  
    , pmdSettings :: PMDSettings
    , cpdSettings :: CPDSettings
}

-------------------------------------------------------------------------------
-- PMD Settings

type PMDRule    = String
type PMDRuleset = String

finalPmdRules :: [PMDRuleset]
finalPmdRules = ["java-myrules"]

-- for initial analysis
selPmdRules :: [PMDRuleset]
selPmdRules = [
      "java-basic"
    , "java-codesize"
    , "java-controversial"
    , "java-coupling"
    , "java-design"
    , "java-empty"
    , "java-imports"
    , "java-optimizations"
    , "java-typeresolution"
    , "java-unnecessary"
    , "java-unusedcode"]

data PMDSettings = PMDSettings {
      pmdRuleSets   :: [PMDRuleset]
    , pmdRuleNames  :: [PMDRule]
    , pmdFormat     :: String
    , pmdSaveFile   :: Bool     -- saving the output file?
    , pmdDir        :: FilePath -- PMD jar
    , pmdOutDir     :: FilePath -- to store intermediate PMD output csv
}

-------------------------------------------------------------------------------
-- CPD Settings 

cpdDefaultMinTokens :: Int
cpdDefaultMinTokens = 100

data CPDSettings = CPDSettings {
      cpdOutDir     :: FilePath -- to store intermediate CPD output csv
    , cpdMinTokens  :: Int      -- the minimum token length which should be reported as a duplicate
    , cpdDir        :: FilePath -- custom CPDRunner
}

