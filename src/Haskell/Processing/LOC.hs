------------------------------------------------------------
-- Counting LOC
------------------------------------------------------------

module Processing.LOC (runLOC, locOutFile, locToDB) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Data.Time
import Data.Csv
import Database.HDBC.Sqlite3
import Database.HDBC
import System.FilePath
import System.Process
import System.Exit
import Control.Monad

import Settings
import Utils


locOutFile :: Settings -> FilePath -> FilePath
locOutFile sett inDir = locOutDir sett </> takeBaseName inDir <.> "csv"   

-- | Running loc tool and saving as csv
runLOC :: Settings -> FilePath -> IO ()
runLOC sett dir = do
    putStrLn $ "Running cloc on folder " ++ dir
    time1 <- getCurrentTime
    let options = [   dir                 -- input dir
                    , "--skip-uniqueness", "--by-file", "--windows", "--csv", "--out"
                    , locOutFile sett dir -- output file
                    ] 
        loc = locDir sett </> "cloc-1.66"
    (exitCode, out, err) <- readProcessWithExitCode loc options []

    when (exitCode == ExitSuccess) $ do
        putStrLn out
        writeFile (locOutDir sett </> takeBaseName dir <.> "txt") out

    getCurrentTime >>= printElapsedTime time1

-- | language, filename, blank, comment, code,
type LocRecord = (String, FilePath, Int, Int, Int)

getFilename :: LocRecord -> FilePath
getFilename (_, fn, _, _, _) = fn

getLOC :: LocRecord -> Int
getLOC (_, _, _, _, loc) = loc

-- | Write results from CSV to db
locToDB :: Settings -> FilePath -> IO ()
locToDB sett fp = liftM (decode HasHeader) (BL.readFile fp) >>= either putStrLn toDB 
    where
        toDB d = dbAddLoc sett (V.toList d) >> putStrLn "Stored locs"

-- | Storing LOC in db
dbAddLoc :: Settings -> [LocRecord] -> IO ()
dbAddLoc sett locData = do
    t1   <- getCurrentTime
    conn <- connectSqlite3 (blackboxDB sett)
    stmt <- prepare conn sql  
    executeMany stmt (map args locData)
    commit conn
    getCurrentTime >>= printElapsedTime t1
        where
        sql = "INSERT INTO loc (filename, loc) VALUES (?,?)"
        --sql = "UPDATE snapshot SET loc=? WHERE name=?" -- does not work
        args :: LocRecord -> [SqlValue]
        args r = [toSql (takeFileName (getFilename r)), toSql (getLOC r)] 