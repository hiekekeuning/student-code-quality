# Student code quality

This document describes the steps to perform the automated analysis of student programs, as presented in the paper 'Code Quality Issues in Student Programs' (in [Proceedings of ITiCSE 2017](http://dl.acm.org/citation.cfm?id=3059061)).
The code is provided to perform checks, extend the analysis and allow replication. Comments and questions can be emailed to the first author.
Both Java and Haskell are used for the automated analysis.

## Installation

The following resources are needed for the analysis.

### Blackbox database

The database is not publicly available. Permission to access the [Blackbox database](https://bluej.org/blackbox/) needs to be requested with the maintainers.

### [PMD Version 5.5.2](http://pmd.sourceforge.io/pmd-5.5.2/)
Add custom ruleset [myrules](./other/myrules.xml) to pmd-java-5.5.2.jar.

### [CPD Version 5.4.1](http://pmd.sourceforge.io/pmd-5.4.1/usage/cpd-usage.html)
A custom CPDRunner ([CPDRunner.java](./src/Java/spa/CPDRunner.java)) has been created, which runs CPD on all files in a folder separately, avoiding the overhead of restarting CPD for each file. The runner can be executed using a [bat file](./other/jcpd.bat).

### [cloc](https://github.com/AlDanial/cloc)
Used for counting lines of code.

### [SQLite database](https://www.sqlite.org/)
Used for storing results locally.

### Java code
The following libraries are needed to run the Java code:
* blackboxAnalyser Java library (for connecting to the Blackbox DB)
* PMD 5.4.1 libraries (for custom CPD runner)
* JUnit (for unit tests in [BBTests](./src/Java/spa/BBTests.java))

Set the values of a number of constants to store local settings in [Main.java](./src/Java/spa/Main.java), such as data directories and executable directories:
* `Main.dbUrl`
* `Main.inDir4days` and `Main.outDir4days`
* `Main.outFileNameFix`

### Haskell code
The following libraries are needed to run the Haskell code:
* [HDBC-Sqlite3](https://hackage.haskell.org/package/HDBC-sqlite3)
* [HDBC](https://hackage.haskell.org/package/HDBC)
* [cassava](https://hackage.haskell.org/package/cassava) (for csv processing)

Create the following values in [Main](./src/Haskell/Main.hs) to store your local settings:
* `mySettings` of type `Settings`
* `myPMDSettings` of type `CPDSettings`
* `myCPDSettings` of type `PMDSettings`

## Pre-processing

### Issue selection

1. Copy the payload and index files of the days below from the Blackbox server to a local folder
   * 8 September 2014 
   * 8 December 2014
   * 9 March 2015
   * 8 June 2015

2. [Java] 4Extract the code files from these days into folder '4daysJ' using `Main.extract4Days(<inFolder>, <outFolder>)`.

3. [Haskell] Run `Main.issueSelection mySettings` that produces a csv-file with PMD output.
5. [Haskell] Run `Processing.PMD.freqAnalysis "dir\\<filename>.csv"` with the name of the csv-file.
4. [Haskell] Run `Main.issueSelectionCPD`.
6. The results are combined into an excel-file for further processing.

### Extension selection

[Java] To retrieve the results described in 3.2.3, run `showAllExtensionsSelected4Weeks()` in `BlackboxDB`.

### Preparing local database

A local SQLite database is used to store the data needed for the analysis. The purple tables contain copied
data from Blackbox, the green tables contain data from running CMD and CPD, the pink table from running cloc
and the black table contains the names of the issues and the first letter of the corresponding category
([data](./other/categories.csv)).

![erd](./img/ERD.png)

The database can be created using queries from [this file](./src/SQL/createDatabase.sql).

#### Storing Blackbox data
[Java] Run `Main.fillSpaDB(BlackboxDB db)` to store (startup) events, snapshots and extensions.

#### Storing code file analyses

1. Retrieve the payloads and indices for week 37, 50 of 2014 and week 11 and 24 of 2015 from the Blackbox server and store them in the binDataDir.

2. [Haskell] Run `Processing.CodeFiles.processAll` to extract code, run cloc/PMD/CPD, store the results into the database and remove the
code (this takes a long time!).

3. The issue table contains one record for each duplicate, instead of the aggregated number of issues from PMD. The view issue2 is created to provide consistent information by aggregating duplicates (duplicate50 and duplicate100). Fill the issue3 table with the data from view issue2.

#### Optimising and cleaning database

* Add indices.
* [SQL] Add filenames to the issue table with [query](./src/SQL/addFileNames.sql).
* [SQL] Cleaning database [query](./src/SQL/cleaning.sql).

## Reporting

### General

[Haskell] For *Table 2: Data set summary* run `Reporting.Reports.generalInfo mySettings` and `Reporting.Reports.generalLocInfo mySettings`.

### RQ1
Table 3: Summary of initial PMD run and Table 4: Top 10 issues, see section on Issue selection above.

[Haskell, Java] For *Table 5: Issue occurrence* run `Reporting.Reports.issueOccs mySettings`.
Set `Main.csvFile` to the location of the csv-file and run [CSVR.java](./src/Java/spa/CSVR.java).

[Haskell, SQL] For *Figure 1: Issues over time* run [query](./src/SQL/rq1Figure1.sql). The number of unique source files per month, used in these queries, are calculated by `dbUniqueSFPerMonth sett`.

### RQ2

[Java] For *Table 6: Issues Fixes*  run `Main.issueFixing()`.

### RQ3

[SQL] For *Table 7: Extension use* execute [query](./src/SQL/rq3Table7.sql).

[SQL] For *Figure 2: Issues and extension use* run [queries](./src/SQL/rq3Figure2.sql).

