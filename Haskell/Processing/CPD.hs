------------------------------------------------------------
-- Executing the CPD tool on code files
------------------------------------------------------------

module Processing.CPD (testCPD, runCPD, cpdToDB, processCPDFile) where

import System.Process
import System.FilePath
import System.Exit
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as V
import Data.Time
import Data.Csv
import Data.List
import Control.Monad
import Database.HDBC.Sqlite3
import Database.HDBC

import Utils
import Tracking
import Settings

-------------------------------------------------------------------------------
-- Running CPD tool

testCPD sett = runCPD (cpdSettings sett) "<folder>"

-- | Run the custom CPD-runner (jcpd.bat) once on a folder
-- Output saved to csv-files
-- Returns name of output file
-- ~1 minute for 100.000 files
runCPD :: CPDSettings -> FilePath -> IO FilePath
runCPD sett dir = do
    putStrLn $ "Running CPD on folder " ++ dir
    time1 <- getCurrentTime

    -- Run custom CPD
    let outFilename = takeBaseName dir ++ "-cpd" ++ show (cpdMinTokens sett) <.> "csv"
        options = [   dir                       -- input dir
                    , show (cpdMinTokens sett)  -- minimum tokens
                    , cpdOutDir sett            -- output dir
                    , outFilename
                    ] 
        loc = cpdDir sett </> "jcpd.bat"
    (exitCode, out, err) <- readProcessWithExitCode loc options []

    when (exitCode == ExitFailure 1) $ putStrLn $ "* Error running CPD: " ++ show err
    when (exitCode == ExitSuccess) $ putStrLn . unlines . drop 2 $ lines out
    
    getCurrentTime >>= printElapsedTime time1

    return outFilename

-------------------------------------------------------------------------------
-- Processing CPD outout 

data CPDRecord = CPDRecord {
      dupLines         :: Int
    , dupTokens        :: Int
    , dupOccurences    :: [(Int, FilePath)] -- (startline, file)
    } 

nrDuplicates :: CPDRecord -> Int
nrDuplicates  = length . dupOccurences

getFirstFile :: CPDRecord -> FilePath
getFirstFile = snd . head . dupOccurences

instance Show CPDRecord where
    show CPDRecord { dupLines = l, dupTokens = t, dupOccurences = occs } = 
        "Lines: " ++ show l ++ ", tokens: " ++ show t ++ ", file(s): " ++ unwords (files occs) ++ 
            ", nr of occurences " ++ show (length occs)
        where
            files = nub . map (takeBaseName . snd)

instance FromRecord CPDRecord where
    parseRecord r = do
        nrOcc <- r .! 2
        CPDRecord <$> r .! 0 <*> r .! 1 <*> mapM getOcc (take nrOcc [3, 5..]) 
            where
                getOcc i = do
                    line <- r .! i
                    file <- r .! (i + 1)
                    return (line, file)                  

-- | Process csv for issue selection
processCPDFile :: FilePath -> IO ()
processCPDFile  = dec <=< BS.readFile
    where
        dec :: BS.ByteString -> IO ()
        dec  = either fail process . decode NoHeader 
        process :: V.Vector CPDRecord  -> IO () 
        process cpdRecs = do
            putStrLn $ "unique source files 100: " ++ show (nrUniqueFiles . getSF . filterMinTok 100 $ cpdRecs)
            putStrLn $ "unique source files 75: " ++ show (nrUniqueFiles . getSF . filterMinTok 75 $ cpdRecs)
            putStrLn $ "unique source files 50: " ++ show (nrUniqueFiles . getSF . filterMinTok 50 $ cpdRecs)
            where
                nrUniqueFiles = length . nub . V.toList -- slow
                filterMinTok minTokens = V.filter (\r -> dupTokens r >= minTokens)
                getSF = V.map (sfid . fromFileName . getFirstFile) -- group per source file!

-------------------------------------------------------------------------------
-- Storing results in DB

-- | Write results from CSV to db
cpdToDB :: Settings -> FilePath -> IO ()
cpdToDB sett fp = liftM (decode NoHeader) (BS.readFile fp) >>= either putStrLn toDB 
    where
        toDB cpd = dbAddDups sett (V.toList cpd) >> putStrLn "Stored duplicate issues"

-- | Storing duplicate issues
dbAddDups :: Settings -> [CPDRecord] -> IO ()
dbAddDups sett cpdData = do
    t1   <- getCurrentTime
    conn <- connectSqlite3 (blackboxDB sett)
    stmt <- prepare conn sql    
    executeMany stmt (map args cpdData)
    commit conn
    putStrLn $ "Stored " ++ show (length cpdData) ++ " duplicate records" 
    getCurrentTime >>= printElapsedTime t1 
        where
        -- DB: name, count, info, sfid, evid
        sql = "INSERT INTO issue VALUES (?, ?, ?, ?, ?)"
        args cpdRec =
            let (SnapshotID sfid evid) = fromFileName (getFirstFile cpdRec)
                name = "Duplicate" ++ show (cpdMinTokens $ cpdSettings sett)
            in [toSql name, toSql (nrDuplicates cpdRec), -- other use of 'count' column
                toSql (show (dupTokens cpdRec) ++ "-" ++ show (dupLines cpdRec)), -- [tokens]-[lines] in info field
                toSql sfid, toSql evid]
        firstOcc = fst . head . dupOccurences -- unused

