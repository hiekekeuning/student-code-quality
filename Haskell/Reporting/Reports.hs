------------------------------------------------------------
-- Collecting and analyzing data
------------------------------------------------------------

module Reporting.Reports where

import Control.Monad
import Data.List
import Data.Time
import Data.Maybe
import Data.Function
import qualified Data.Map as M
import Text.Printf

import Utils
import Reporting.Blackbox
import Tracking
import Settings

----------------------------------------------------------------------------
-- General data

-- | Events, files etc.
-- used for table 'data set summary' RQ1
generalInfo :: Settings -> IO ()
generalInfo sett = do
    t1 <- getCurrentTime

    (usf, sfs, e) <- dbCountSourceFiles sett
    putStrLn $ show usf ++ " unique source files in total" 
    putStrLn $ show sfs ++ " source files snapshots" 
    putStrLn $ show e ++ " events" 

    ev <- dbNrEventsPerSF sett
    putStrLn $ show (average ev) ++ " average events per source file, max " ++ show (maximum ev) ++ " events"
    putStrLn $ show (median (map fromIntegral ev)) ++ " median events per source file"

    getCurrentTime >>= printElapsedTime t1

-- | used for table 'data set summary' RQ1
generalLocInfo :: Settings -> IO ()
generalLocInfo sett = do
    (max, avg, cnt, sum) <- dbLOCStats sett
    median' <- dbLOCMedian sett
    putStrLn $ "Highest LOC: " ++ show max 
        ++ ", average LOC: " ++ printf "%.2f" avg
        ++ ", total LOC: " ++ show sum ++ " in " ++ show cnt ++ " files"
    putStrLn $ "Median LOC: " ++ show median'

    -- Per Source file
    locAvgs <- dbLOCAvgPerSF sett
    putStrLn $ "average LOC per SF: " ++ printf "%.2f" (average locAvgs :: Double)
    putStrLn $ "Median LOC per SF: " ++ show (median locAvgs)

----------------------------------------------------------------------------
-- Issues

-- | Per issue, in how many unique source files does it occur (at least once)?
-- used for RQ1 table 'occurences per issue'
-- 165s
issueOccs :: Settings -> IO ()
issueOccs sett = do 
    t1        <- getCurrentTime
    occIssRes <- dbOccIssues sett
    avgKLOC   <- dbAvgIssuesPerKLOC sett
    
    -- latex
    putStrLn $ unlines 
        $ map (\(name, n) -> unwords (intersperse "&" 
            [ take 1 (issueType name), -- first letter of category
             Utils.left 35 name, 
             Utils.right 5 (printf "%.1f" n), 
             Utils.right 9 (printf "%.1f" (fromJust $ lookup name avgKLOC))]) ++ " \\\\") occIssRes    

    getCurrentTime >>= printElapsedTime t1