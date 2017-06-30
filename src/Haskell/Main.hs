module Main where

import System.FilePath
import Data.Time
import Control.Monad

import Utils
import Tracking
import Settings

import Processing.CodeFiles
import Processing.Extract
import Processing.PMD
import Processing.CPD

import Reporting.Blackbox
import Reporting.Reports

-------------------------------------------------------------------------------
-- Settings 

mySettings:: Settings
mySettings = Settings {
      blackboxDB  = "<dir>//<file>.db"
    , binDataDir  = "<dir>"
    , locDir      = "<dir>//cloc"
    , locOutDir   = "<dir>"
    , pmdSettings = myPMDSettings
    , cpdSettings = myCPDSettings
}

myPMDSettings = PMDSettings {
      pmdRuleSets  = undefined
    , pmdFormat    = "csv"
    , pmdRuleNames = undefined
    , pmdSaveFile  = True
    , pmdDir       = "<dir>//pmd-bin-5.5.2//lib"
    , pmdOutDir    = "<dir>"
}

myCPDSettings = CPDSettings {
      cpdOutDir     = "<dir>"
    , cpdMinTokens  = cpdDefaultMinTokens
    , cpdDir        = "<dir>"
}

sett :: Settings
sett = mySettings

-------------------------------------------------------------------------------
-- Main

main = myMain sett
    
myMain :: Settings -> IO ()
myMain sett = do
    t1 <- getCurrentTime

    -- Testing
    --testExtract1
    --testPMD
    --testCPD

    -- * Issue selection
    --issueSelection "<dir>" sett
    --issueSelectionCPD "<dir>" sett
    --freqAnalysis "<dir>//<file>.csv"

    -- * end issue selection
    
    getCurrentTime >>= printElapsedTime t1 

-- | Initial issue selection
-- Assumes java code files are extracted into the exFp folder
-- [Elapsed time 5631s]
issueSelection :: FilePath -> Settings -> IO () 
issueSelection exFp sett = void $ runPMD (pmdSettings sett) { pmdRuleSets = selPmdRules } exFp

-- | Initial CPD
-- Assumes java code files are extracted into the exFp folder
issueSelectionCPD :: FilePath -> Settings -> IO () 
issueSelectionCPD exFp sett = runCPD' 50 >>= processCPDFile
    where
        runCPD' max = runCPD (cpdSettings sett) { cpdMinTokens = max } exFp