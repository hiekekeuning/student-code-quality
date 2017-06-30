------------------------------------------------------------
-- Utils
------------------------------------------------------------

module Utils where

import System.FilePath
import System.Directory
import Control.Monad
import Data.List
import Data.Time
import Control.Arrow ((&&&))


-- Moves all files into one folder
makeFileFolder :: [FilePath] -> FilePath -> IO ()
makeFileFolder files folderName = do
  createDirectory folderName
  mapM_ copy files
  where
      copy file = do
          let fileName = takeFileName file
          fileExists <- doesFileExist file 
          when fileExists $ copyFile file (folderName </> fileName)
  
-- | List all files (recursively) in a directory
-- Source: http://therning.org/magnus/archives/228
listFilesR :: FilePath -> IO [FilePath]
listFilesR path = do
    allfiles <- getDirectoryContents path
    no_dots <- filterM (return . isDODD) (map (joinFN path) allfiles)
    dirs <- listDirs no_dots
    -- subdirfiles <- liftM concat (mapM listFilesR dirs)
    files <- listFiles no_dots
    return $ files -- ++ subdirfiles

isDODD :: String -> Bool
isDODD f = not $ isSuffixOf "." f || isSuffixOf ".." f

listDirs :: [FilePath] -> IO [FilePath]
listDirs = filterM doesDirectoryExist

listFiles :: [FilePath] -> IO [FilePath]
listFiles = filterM doesFileExist

joinFN :: String -> String -> FilePath
joinFN p1 p2 = joinPath [p1, p2]


-- non-recursive
getJavaFilesFromDir :: FilePath -> IO [FilePath]
getJavaFilesFromDir = liftM (filter (isSuffixOf ".java")) . listFilesR 


printElapsedTime :: UTCTime -> UTCTime -> IO ()
printElapsedTime t1 t2 = putStrLn $ "[Elapsed time " ++ show (diffUTCTime t2 t1) ++ "]"

sLength :: [a] -> String
sLength = show . length

average xs = realToFrac (sum xs) / genericLength xs

-- Align and add spaced for a fixed width
left, right :: Int -> String -> String
left width s = take width s ++ replicate (width - length s) ' '
right width s = replicate (width - length s) ' ' ++ take width s

-- Make a frequency list
freqList :: Ord a => [a] -> [(a, Int)]
freqList  = map (head &&& length) . group . sort 


eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

divF :: Integral a => a -> a -> Float
divF x y = fromIntegral x / fromIntegral y

median :: (Floating a, Ord a) => [a] -> a
median x | odd n  = head $ drop (n `div` 2) x'
         | even n = mean $ take 2 $ drop i x'
                  where i = (length x' `div` 2) - 1
                        x' = sort x
                        n  = length x
mean :: Floating a => [a] -> a
mean x = fst $ foldl' (\(m, n) x -> (m+(x-m)/(n+1),n+1)) (0,0) x

loc2kloc :: (Integral a, Integral b) => a -> b
loc2kloc loc = round (loc `divF` 1000)

percOf x y = divF (100 * x) y


fiveNumSumm sortedList = (q0, q1, q2, q3, q4, avg)
  where
    q0 = head sortedList
    q1 = middle (take (n `div` 2) sortedList)
    q2 = middle sortedList
    q3 = middle (drop (n `div` 2) sortedList)
    q4 = last sortedList
    avg = average sortedList

    n = length sortedList
    even x = x `mod` 2 == 0

    middle l = if even (length l)
        then (l !! mid + l !! (mid - 1) ) / 2
        else l !! mid
        where
          mid = length l `div` 2

testFNS = fiveNumSumm [53,  79,  80,  82,   87,   91,   93,  9] --(53.0,79.5,84.5,92.0,98.0