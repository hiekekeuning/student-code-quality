------------------------------------------------------------
-- Source files, snapshots, events etc.
------------------------------------------------------------

module Tracking where

import System.FilePath

------------------------------------------------------------
-- Data types

type Issue = String

data SnapshotID = SnapshotID { sfid :: Int, evid :: Int } deriving (Eq,Ord)
      
-- Snapshot file names ("f123-e456.java")
toFileName :: SnapshotID -> FilePath
toFileName (SnapshotID sfid evid) = "f" ++ show sfid ++ "-e" ++ show evid <.> "java"

-- | extract master_event_id and source_file_id from filename
-- assumes a valid filename
fromFileName :: FilePath -> SnapshotID
fromFileName = (\(x, y) -> SnapshotID { sfid = sfid x, evid = evid y } ) . span (/= '-') . dropExtension . takeFileName 
    where
        sfid = read . tail   -- remove "f"
        evid = read . drop 2 -- remove "-e"

----------------------------------------------------------------------------
-- Issues

flowIssues, exprIssues, decompIssues, modIssues, allIssues, idiomIssues :: [Issue]
--Flow
flowIssues = 
    ["ModifiedCyclomaticComplexity", "NPathComplexity"
    , "TooFewBranchesForASwitchStatement", "AvoidDeeplyNestedIfStmts", "CyclomaticComplexity", "EmptyIfStmt",
    "PrematureDeclaration"]  
idiomIssues = ["SwitchStmtsShouldHaveDefault", "MissingBreakInSwitch", "AvoidInstantiatingObjectsInLoops"]
--Expressions 
exprIssues =
    ["AvoidReassigningParameters", "ConfusingTernary", "PositionLiteralsFirstInComparisons", "SimplifyBooleanExpressions"
    , "SimplifyBooleanReturns", "PositionLiteralsFirstInCaseInsensitiveComparisons", "IdempotentOperations"
    , "CollapsibleIfStatements", "UselessParentheses"] 
-- Decomposition
decompIssues = 
    ["ExcessiveMethodLength", "SwitchDensity", "SingularField", "NcssMethodCount", "NcssMethodCount50"] ++ ["Duplicate50", "Duplicate100"]
--Modularization
modIssues =
    ["GodClass", "TooManyMethods", "TooManyFields", "LawOfDemeter", "LooseCoupling"]
allIssues = flowIssues ++ exprIssues ++ decompIssues ++ modIssues ++ idiomIssues

issueType :: Issue -> String
issueType i
   | i `elem` flowIssues   = "Flow"
   | i `elem` exprIssues   = "Expressions"
   | i `elem` decompIssues = "Decomposition"
   | i `elem` modIssues    = "Modularization"
   | i `elem` idiomIssues  = "Idiom"
   | otherwise             = "Other"

----------------------------------------------------------------------------
-- Extensions

type Extension = String

extensions :: [Extension]
extensions = ["Checkstyle", "PMD", "PMD plug-in", "FindBugs", "PatternCoder"]