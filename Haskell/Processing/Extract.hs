------------------------------------------------------------
-- Extracting code files from the payload-files
------------------------------------------------------------

{-# LANGUAGE BangPatterns #-}
module Processing.Extract (extract, testExtract, testExtract1) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Data.Binary.Get
import Data.Word
import Data.Int
import Data.Time
import System.FilePath
import System.Directory

import Tracking
import Utils
import Settings

testExtract1 = testExtract (fromGregorian 2014 9 8) 

testExtract :: Day -> Settings -> IO ()
testExtract day sett = do
    t1 <- getCurrentTime

    putStrLn $ "Processing day " ++ showGregorian day
    createDirectoryIfMissing False tempDir

    putStrLn "Extract code files to temp dir"
    extract day sett tempDir

    getCurrentTime >>= printElapsedTime t1

    where
        tempDir = binDataDir sett </> name
        name = "day-" ++ showGregorian day

extract :: Day -> Settings -> FilePath -> IO ()
extract day sett tempDir = do
    payload <- BL.readFile $ binDataDir sett </> "payload-" ++ showGregorian day
    index   <- BL.readFile $ binDataDir sett </> "index-" ++ showGregorian day

    let headers = reverse $ runGet extractHeader index
    putStrLn $ "Files found " ++ sLength headers

    let codeFiles = extractSnapshots headers payload   
        writeSnapshot (SnapshotFile ssid code) = BL.writeFile (tempDir </> toFileName ssid) code
    mapM_ writeSnapshot codeFiles

-----------------------------------------------------------
-- Reading index and payload files

data SnapshotFile = SnapshotFile !SnapshotID !BL.ByteString
-- sourceId, eventId, start, length
data Header = Header !Word64 !Word64 !Int64 !Int32

-- | Get header data for all successful compilations
extractHeader :: Get [Header]
extractHeader = extractHeader' []
    where
        extractHeader' xs = do
            !sourceId <- getWord64be
            !eventId  <- getWord64be
            !start    <- getInt64be
            !length   <- getInt32be
            !success  <- getInt32be

            let ss = Header sourceId eventId start length
                nw = if success == 1 then (ss:xs) else xs

            empty <- isEmpty
            if empty 
                then return $! nw
                else extractHeader' nw

extractSnapshots :: [Header] -> BL.ByteString -> [SnapshotFile]
extractSnapshots headers = go (Header 0 0 0 0) headers decoder . BL.toChunks
  where
    decoder p h = runGetIncremental (extractSnapshot p h)
    
    go :: Header -> [Header] -> (Header -> Header -> Decoder SnapshotFile) -> [BS.ByteString] -> [SnapshotFile]
    go _ [] _ _ = []
    go prevH (header:rest) decF input = rec' (decF prevH header) input  
        where
            rec' :: Decoder SnapshotFile -> [BS.ByteString] -> [SnapshotFile]
            rec' _ []       = []
            rec' dec (b:bs) = case pushChunk dec b of
                Done b' o ssf -> ssf : go header rest decF (b' : bs)
                Partial f     -> case bs of
                                  (x:xs) -> rec' (f $ Just x) xs
                                  []     -> []
                Fail _ _ s    -> error s

-- | Input: previous + current header
extractSnapshot :: Header -> Header -> Get SnapshotFile
extractSnapshot (Header _ _ startP lengthP) (Header sfid evid start length) = do
    skip (fromIntegral start - (fromIntegral startP + fromIntegral lengthP)) 
    code <- getLazyByteString (fromIntegral length)
    return $! SnapshotFile (SnapshotID (fromIntegral sfid) (fromIntegral evid)) code

