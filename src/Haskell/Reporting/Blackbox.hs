------------------------------------------------------------
-- Accessing the local database with Blackbox data
------------------------------------------------------------

module Reporting.Blackbox where

import Database.HDBC.Sqlite3
import Database.HDBC
import Control.Monad
import Control.Arrow

import Utils
import Tracking
import Settings

------------------------------------------------------------
-- Meta info (all used)

-- | Nr of unique sessions
dbSessionCount :: Settings -> IO Int
dbSessionCount = doSelectInt "SELECT COUNT(DISTINCT session_id) FROM event"

-- | The dates for which issues are collected
dbIssuesForDates :: Settings -> IO [(String, Int)]
dbIssuesForDates sett = doSelect sett sql conv
    where
        sql = "SELECT DISTINCT strftime('%Y-%m-%d', time/1000, 'unixepoch') d, COUNT(*)"
                ++ "FROM event INNER JOIN issue2 ON id=event_id GROUP BY d ORDER BY d"
        conv [date, nrIss] = (fromSql date :: String, fromSql nrIss :: Int)

-- | Returns (unique source files in total, source files snapshots, events)
dbCountSourceFiles :: Settings -> IO (Int, Int, Int)
dbCountSourceFiles sett = do
    [[cdi, c, cde]] <- doSelect sett sql1 (map sqlToInt)
    return (cdi, c, cde)
    where
        sql1 = "SELECT count(distinct sf_id), count(*), count(distinct event_id) FROM snapshot"

-- |
dbNrEventsPerSF :: Settings -> IO [Int]
dbNrEventsPerSF sett = doSelect sett sql sql1ToInt 
    where
        sql = "SELECT COUNT(*) c FROM snapshot GROUP BY sf_id ORDER BY c"

------------------------------------------------------------
-- Issue aggregates

-- | Unique source files with issues
dbSfWithIssues :: Settings -> IO Int
dbSfWithIssues = doSelectInt "SELECT COUNT(DISTINCT source_file_id) from issue2"

-- |  Distinct issues on average per unique source file
dbAvgIssues :: Settings -> IO Double
dbAvgIssues sett = liftM head $ doSelect sett sql sql1ToDouble 
    where
        sql = "SELECT AVG(c) FROM (" ++ 
            "SELECT COUNT (distinct m.name) c " ++ 
            "FROM snapshot ss LEFT OUTER JOIN issue2 m " ++ 
            "ON m.source_file_id = ss.sf_id AND m.event_id = ss.event_id " ++
            "GROUP BY ss.sf_id)" 

-- | Distinct issues on average per snapshot
dbAvgIssues' :: Settings -> IO Double
dbAvgIssues' sett = liftM head $ doSelect sett sql sql1ToDouble 
    where
        sql = "SELECT AVG(c) FROM (" ++ 
            "SELECT COUNT (distinct m.name) c " ++ 
            "FROM snapshot ss LEFT OUTER JOIN issue2 m " ++ 
            "ON m.source_file_id = ss.sf_id AND m.event_id = ss.event_id " ++
            "GROUP BY ss.sf_id, ss.event_id)"

-- | Per SF the total number of issues
dbIssuesPerSnapshotFreq :: Settings -> IO [(Double, Int)]
dbIssuesPerSnapshotFreq sett = doSelect sett sql conv
    where
        sqld = "SELECT c, COUNT(*) FROM "
            ++ "(SELECT sum(ifnull(i.count, 0)) c "
            ++ "FROM snapshot ss LEFT OUTER JOIN issue2 i "
            ++ "ON i.filename = ss.name "
            ++ "GROUP BY ss.name) "
            ++ "GROUP BY c"
        sql = "SELECT a, count(*) from ( "
            ++ "SELECT sf_id, x, round(sum(ifnull(count, 0))*100000/sum(loc)) a FROM "
            ++ "(snapshot s INNER JOIN loc t ON t.filename=s.name INNER JOIN (SELECT DISTINCT name x from issue2) ) "
            ++ "LEFT JOIN issue2 i ON (i.event_id=s.event_id AND sf_id=source_file_id AND i.name=x) "
            ++ "GROUP BY sf_id "
            ++ ") GROUP BY a;"
        conv[x,y] = (fromSql x, fromSql y) :: (Double, Int)

------------------------------------------------------------
-- Per Issue

-- | Per issue, in how many unique source files does it occur (at least once)?
dbOccIssues :: Settings -> IO [(Issue, Double)]
dbOccIssues sett = doSelect sett sql conv
    where
        sql =  "SELECT name, COUNT(DISTINCT source_file_id)*100.0/(SELECT count(distinct sf_id) FROM snapshot) c "
            ++ "FROM issue2 GROUP BY name ORDER BY c DESC"
        conv [name, unCount] = (fromSql name, fromSql unCount) :: (Issue, Double)

-- | Per issue, how many occurences per source file on average?
-- First average per source file, then average per issue (avoid problem with sf's with many snapshots)
-- not used, needed for calculating 5-number-summary
dbIssuesPerSF :: Settings -> IO [(Issue, Double)]
dbIssuesPerSF sett = doSelect sett sql conv
    where
        sql =  "SELECT x, sf_id, sum(ifnull(count, 0))*1000/sum(loc) a "
            ++ "FROM (snapshot s INNER JOIN loc t ON t.filename=s.name INNER JOIN (SELECT DISTINCT name x from issue2) ) "
            ++ "LEFT JOIN issue2 i ON (i.event_id=s.event_id AND sf_id=source_file_id AND i.name=x) "
            ++ "GROUP BY x, sf_id ORDER BY x, a"
        conv [iss, _, avg] = (fromSql iss, fromSql avg) :: (Issue, Double)

-- | Used for rq1, second column
dbAvgIssuesPerKLOC :: Settings -> IO [(Issue, Double)]
dbAvgIssuesPerKLOC sett = doSelect sett sql conv
    where
        sql = "SELECT n, sum(a)*1000.0/453526 from ( " -- of all snapshots
            ++ "SELECT i.name n, total(count)/(SELECT sum(loc) FROM loc WHERE sfid=sf_id) a FROM "
            ++ "snapshot s INNER JOIN issue2 i ON (i.event_id=s.event_id AND sf_id=source_file_id) "
            ++ "GROUP BY i.name, sf_id "
            ++ ") GROUP BY n;"
        conv [iss, avgLoc] = (fromSql iss, fromSql avgLoc) :: (Issue, Double)

----------------------------------------------------------------------------
-- LOC queries

-- | Returns max, avg, count, sum
dbLOCStats :: Settings -> IO (Int, Double, Int, Int)
dbLOCStats sett = liftM head $ doSelect sett sql conv
    where
        sql =   "SELECT max(loc), avg(loc), count(loc), sum(loc) FROM loc"
        conv [max, avg, cnt, sum] = (fromSql max, fromSql avg, fromSql cnt, fromSql sum) :: (Int, Double, Int, Int)

dbLOCMedian :: Settings -> IO Double
dbLOCMedian sett = liftM head $ doSelect sett sql sql1ToDouble
    where
        sql = "SELECT AVG(loc) "
            ++ "FROM (SELECT loc FROM loc WHERE loc IS NOT NULL ORDER BY loc "
            ++ "LIMIT 2 - (SELECT COUNT(loc) FROM loc) % 2 OFFSET (SELECT (COUNT(loc) - 1) / 2 FROM loc))"

dbLOCFreq :: Settings -> IO [(Int, Int)]
dbLOCFreq sett = doSelect sett sql sqlToIntInt
    where
        sql = "SELECT loc, COUNT(*) FROM loc "
            ++ "WHERE loc IS NOT NULL "
            ++ "GROUP BY loc"

-- | LOC Averages per source file
dbLOCAvgPerSF :: Settings -> IO [Double]
dbLOCAvgPerSF sett = doSelect sett sql sql1ToDouble
    where
        sql = "SELECT AVG(loc) a FROM loc l INNER JOIN snapshot s ON s.name=l.filename "
            ++ "GROUP BY sf_id"
----------------------------------------------------------------------------
-- Queries over time

-- Unique source files per month, used for RQ1 Figure 1
dbUniqueSFPerMonth :: Settings -> IO [(Int, Int)]
dbUniqueSFPerMonth sett = doSelect sett sql sqlToIntInt
    where
        sql = "SELECT strftime('%m', time/1000, 'unixepoch') month, count(distinct sf_id) "
            ++ "FROM snapshot s INNER JOIN event e ON s.event_id=e.id "
            ++ "GROUP BY month;"

-- LOC + avg per month
dbLOCPerMonth :: Settings -> IO [(Int, Int, Int)]
dbLOCPerMonth sett = doSelect sett sql conv
    where
        sql = "SELECT strftime('%m', time/1000, 'unixepoch') month, sum(loc), sum(loc)/count(filename) as avgLocPerFile "
            ++ "FROM snapshot s INNER JOIN event e ON s.event_id=e.id INNER JOIN loc on loc.filename=s.name "
            ++ "GROUP BY month;"
        conv [mnth, sum, avg] = (fromSql mnth, fromSql sum, fromSql avg) :: (Int, Int, Int)


dbLOCOverTime :: Settings -> IO ([(Int, Int)], [(Int, Double)])
dbLOCOverTime = liftM (map (\(h,s,_) -> (h,s)) &&& map (\(h,_,a) -> (h,a))) . dbLOCOverTime' 

dbLOCOverTime' :: Settings -> IO [(Int, Int, Double)]
dbLOCOverTime' sett = doSelect sett sql conv
    where
        sql = "SELECT strftime('%m', time/1000, 'unixepoch') AS m, SUM(loc), AVG(loc) " 
                ++ "FROM loc INNER JOIN event e ON e.id=substr(filename, 0, length(filename)-instr(filename, '-') - 6)  "
                ++ "GROUP BY m"
        conv [hour, sum, avg] = (fromSql hour, fromSql sum, fromSql avg) :: (Int, Int, Double)

-----------------------------------------------------------
-- Utils

doSelect :: Settings -> String -> ([SqlValue] -> b) -> IO [b]
doSelect sett sql conv = do
    conn <- connectSqlite3 (blackboxDB sett)
    liftM (map conv) $ quickQuery' conn sql []

-- For queries that return 1 int
doSelectInt ::  String -> Settings -> IO Int
doSelectInt sql sett  = liftM head $ doSelect sett sql sql1ToInt

------------------------------
-- Common conversion functions

sqlToInteger :: SqlValue -> Integer
sqlToInteger x = fromSql x ::Integer

sqlToInt :: SqlValue -> Int
sqlToInt x = fromSql x ::Int

-- | retrieve result from query with one int as a result
sql1ToInt :: [SqlValue] -> Int
sql1ToInt [x] = sqlToInt x
sql1ToInt _   = error "Expected 1 field"

-- | retrieve result from query with one float as a result
sql1ToDouble :: [SqlValue] -> Double
sql1ToDouble [x] = fromSql x :: Double
sql1ToDouble _   = error "Expected 1 field"

sqlToIntInt :: [SqlValue] -> (Int, Int)
sqlToIntInt [x, y] = (fromSql x, fromSql y) :: (Int, Int)
sqlToIntInt _      = error "Expected 2 fields"


-------------------------------------------------------------------------------
-- Checks

checkLoc :: Settings -> IO ()
checkLoc sett = do 
    files <- doSelect sett sql1 conv1
    putStrLn $ "Nr of files in loc not in snapshot " ++ sLength files
    --print files

    files <- doSelect sett sql2 conv1
    putStrLn $ "Nr of files in snapshot not in loc " ++ sLength files
    --print files
    return ()
    where
        sql1 = "SELECT filename FROM loc WHERE filename NOT IN (SELECT name FROM snapshot);" --0
        
        sql2 = "SELECT name FROM snapshot WHERE name NOT IN (SELECT filename FROM loc)" --164,882

        conv1 [fn] = fromSql fn :: String