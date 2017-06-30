------------------------------------------------------------
-- Batch processing files
------------------------------------------------------------

module Processing.CodeFiles (processAll, processDay) where

import System.FilePath
import System.Directory

import Data.List
import Data.Time

import Utils
import Tracking
import Settings

import Processing.PMD
import Processing.CPD
import Processing.Extract
import Processing.LOC

weekSep = makeWeek 2014 9 8
weekDec = makeWeek 2014 12 8
weekMar = makeWeek 2015 3 9
weekJun = makeWeek 2015 6 8

makeWeek = makeDays 7

makeDays:: Int -> Integer -> Int -> Int -> [Day]
makeDays nrOfDays yr mnth firstDay = take nrOfDays $ unfoldr (\d -> Just (d, addDays 1 d)) (fromGregorian yr mnth firstDay)

processAll :: Settings -> IO ()
processAll sett = do
    let sett' = sett { pmdSettings = (pmdSettings sett) { pmdRuleSets = finalPmdRules } }
    mapM_ (processDay sett') weekMar
    mapM_ (processDay sett') weekJun
    mapM_ (processDay sett') weekSep
    mapM_ (processDay sett') weekDec

-- | Info for a folder of Java files
uniqueFiles :: FilePath -> IO ()
uniqueFiles dir = do
    javaFiles <- getJavaFilesFromDir dir
    putStrLn $ sLength javaFiles ++ " Java files in folder"

    let uniqueSourceFiles = nub $ map (sfid . fromFileName) javaFiles
        uniqueEvents      = nub $ map (evid . fromFileName) javaFiles
    putStrLn $ sLength uniqueSourceFiles ++ " unique source files in folder"
    putStrLn $ sLength uniqueEvents ++ " unique events in folder"

-- Extract files for one day, run PMD/CPD/cloc and store results in CSV
-- 203s for a day of ~40.800 files (22 rules, 2xCPD)
processDay :: Settings -> Day -> IO ()
processDay sett day = do
    t1 <- getCurrentTime

    putStrLn $ "Processing day " ++ showGregorian day
    createDirectoryIfMissing False tempDir

    putStrLn "Extract code files to temp dir"
    extract day sett tempDir

    putStrLn "Run PMD"
    (Right (Just pmdOutFile, _)) <- runPMD pmdSett tempDir
    pmdToDB sett pmdOutFile

    putStrLn "Run CPD 50"
    let cpd50Sett =  cpdSett { cpdMinTokens = 50 }
    cpdOutFile <- runCPD cpd50Sett tempDir
    cpdToDB (sett { cpdSettings = cpd50Sett}) (cpdOutDir cpd50Sett </> cpdOutFile)

    putStrLn "Run cloc"
    runLOC sett tempDir
    locToDB sett (locOutFile sett tempDir)

    putStrLn "Remove temp files + dir"
    removeDirectoryRecursive tempDir

    getCurrentTime >>= printElapsedTime t1

    where
        tempDir = binDataDir sett </> name
        name = "-day-" ++ showGregorian day
        cpdSett = cpdSettings sett
        pmdSett = pmdSettings sett

-------------------------------------------------------------------------------
-- Frequency analysis (which issues occur the most)

freqAnalysisOnFiles :: [Day] -> Settings -> IO ()
freqAnalysisOnFiles days sett = do
    t1 <- getCurrentTime

    putStrLn $ "Processing " ++ sLength days ++ " days" 
    createDirectoryIfMissing False tempDir

    putStrLn "Extract code files to temp dir"
    mapM_ (\day -> extract day sett tempDir) days
    uniqueFiles tempDir

    putStrLn "Run PMD"
    _ <- runPMD (pmdSett { pmdRuleSets = selPmdRules }) tempDir
    
    putStrLn "Run CPD"
    _ <- runCPD (cpdSettings sett) tempDir
   
    putStrLn "Remove temp files + dir"
    removeDirectoryRecursive tempDir

    getCurrentTime >>= printElapsedTime t1

    where
        tempDir = binDataDir sett </> name
        name    = sLength days ++ "days"
        pmdSett = pmdSettings sett
      

