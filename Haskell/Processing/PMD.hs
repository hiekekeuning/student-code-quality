------------------------------------------------------------
-- Executing the PMD tool on code files
------------------------------------------------------------

module Processing.PMD (freqAnalysis, runPMD, pmdToDB) where

import System.Process
import System.FilePath
import System.Directory
import Control.Monad
import Data.Csv
import qualified Data.Vector as V
import qualified Data.Map as Map
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.ByteString.Lazy as LBS (readFile, ByteString, filter, writeFile)
import Data.List
import Data.Time
import Data.Char (ord)
import Data.Function (on)
import Database.HDBC.Sqlite3
import Database.HDBC

import Utils
import Tracking
import Settings

-----------------------------------------------------------
-- Paths
testFile = "<dir>\\BadCode.java"
testFile2 = "<dir>\\<file>.java"
testDir1000 = "<dir>"

-- record in csv: Problem,Package,File,Priority,Line,Rule set,Rule
type PMDData = (Int, String, FilePath, Int, Int, String, PMDRule)

getRule :: PMDData -> PMDRule
getRule (_, _, _, _, _, _, rule) = rule

getFileName :: PMDData -> FilePath
getFileName (_, _, fn, _, _, _, _) = fn

getRuleSet :: PMDData -> String
getRuleSet (_, _, _, _, _, set, _) = set

-------------------------------------------------------------------------------
-- Running PMD

testPMD :: PMDSettings -> IO ()
testPMD pmdSett = void $ runPMD (pmdSett { pmdRuleSets = selPmdRules } ) testDir1000 --testFile


-- | Run PMD on a file or folder and return a csv-string with results and the name of the csv-file
-- store csv if in settings
-- 18s for 1000 files
runPMD :: PMDSettings -> FilePath -> IO (Either String (Maybe FilePath, LBS.ByteString))
runPMD settings fp = do
    isDir <- doesDirectoryExist fp
    when isDir $ do 
        javaFiles <- getJavaFilesFromDir fp
        putStrLn $ sLength javaFiles ++ " Java files in folder"

    time1 <- getCurrentTime
    (exitCode, output, err) <- readProcessWithExitCode "java" args []
    getCurrentTime >>= printElapsedTime time1

    unless (null err) $ putStrLn ("Error running PMD: " ++ show err)
   
    return $ if null err
        then Right (if pmdSaveFile settings then Just (outFileName fp settings) else Nothing, pack output)
        else Left err
    where
        args = ["-cp", (pmdDir settings) ++ "\\*",
            "net.sourceforge.pmd.PMD",
            "-R", intercalate "," (pmdRuleSets settings),
            "-d", fp,
            "-P", "desc=true", -- "-shortnames", -- omit description field and file paths | Shortnames trims first 2 chars in some file names
            "-f", pmdFormat settings] ++ toFile

        toFile
            | pmdSaveFile settings = ["-r", outFileName fp settings ]
            | otherwise = []

        outFileName fp sett = pmdOutDir sett </> takeBaseName fp ++ "-pmd" ++ sLength (pmdRuleSets sett) ++ "rs" <.> "csv"

-------------------------------------------------------------------------------
-- CSV

tryCsv :: LBS.ByteString -> Either String (V.Vector PMDData)
tryCsv = decode HasHeader

-------------------------------------------------------------------------------
-- Storing results in DB

-- | Write results from CSV to db
pmdToDB :: Settings -> FilePath -> IO ()
pmdToDB sett fp = do
    maybeCsv <- liftM (tryCsv . LBS.filter (<= 127)) (LBS.readFile fp) 
    either putStrLn toDB maybeCsv
    where
        toDB pmd = dbAddIssues sett (V.toList pmd) >> putStrLn "Stored metrics"

dbAddIssues :: Settings -> [PMDData] -> IO ()
dbAddIssues sett pmdData = do
    t1   <- getCurrentTime
    conn <- connectSqlite3 (blackboxDB sett)
    stmt <- prepare conn sql    
    --executeMany stmt (map args pmdData)

    executeMany stmt (map args $ process' pmdData)
    commit conn
    putStrLn $ "Stored " ++ show (length pmdData) ++ " issues" 
    getCurrentTime >>= printElapsedTime t1 
        where
        -- DB: name, count, info, sfid, evid
        sql = "INSERT INTO issue VALUES (?, ?, ?, ?, ?)"
        
        args :: ((PMDRule, FilePath), Integer) -> [SqlValue]
        args ((name, file), count) =
            let (SnapshotID sfid evid) = fromFileName file
            in [toSql name, toSql count, toSql (""::String), 
                toSql sfid, toSql evid]
        
        -- combine multiple instances of the same issue into 1 record with count=nr of instances
        process' :: [PMDData] -> [((PMDRule, FilePath), Integer)]
        process' = Map.toList . Map.fromListWith (+) . map dropInfo
          where
            dropInfo (_, _, file, _, _, _, rule) = ((rule, file), 1)

-------------------------------------------------------------------------------
-- Analysis of rule frequency in unique files

-- Filepath with PMD csv-output
freqAnalysis :: FilePath -> IO ()
freqAnalysis fp = do
    csvData <- LBS.readFile fp
    let res             = fmap processRecs . tryCsv $ LBS.filter (<= 127) csvData -- 
        endcodeOptions  = defaultEncodeOptions { encDelimiter = fromIntegral (ord ';') }
        csvRes          = fmap (encodeWith endcodeOptions . map FreqData) res
    either fail (LBS.writeFile (replaceFileName fp $ "Freq-" ++ takeFileName fp)) csvRes

    where
        processRecs :: V.Vector PMDData -> [((PMDRule, String), Integer)]
        processRecs = sortBy freqDesc . combineRules . combineSourcefile . makeList
            where
                makeRecord rec    = ((getSfid rec, getRule rec, getRuleSet rec), 1)
                makeList          = V.toList . V.map makeRecord
                combineSourcefile = Map.toList . Map.fromList -- removes double occs of the same issue
                combineRules      = Map.toList . Map.fromListWith (+) . map dropFileInfo -- group per issue
                freqDesc          = flip compare `on` snd
                getSfid           = sfid . fromFileName . getFileName
                dropFileInfo ((_, rule, set), occ) = ((rule, set), occ)
            
-- temp
data FreqData = FreqData ((PMDRule, String), Integer)

instance ToRecord FreqData where  
    toRecord (FreqData ((rule,set),occ)) = record [toField rule, toField set, toField occ]

