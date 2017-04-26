[in progress]

# Student code quality

This document describes the steps to perform the automated analysis of student programs, as presented in the paper 'Code Quality Issues in Student Programs' [to appear in proceedings of ITiCSE 2017, [online](http://www.cs.uu.nl/research/techreps/repo/CS-2017/2017-006.pdf)].
The code is provided to perform checks, extend the analysis and allow replication. Comments and questions can be emailed to the first author.
Both Java and Haskell is used for the automated analysis.

## Installation

### Blackbox database

The database used is not publicly available. Permission to access the [Blackbox database](https://www.bluej.org/blackbox.html) needs to be requested with the maintainers.

### [PMD Version 5.5.2](http://pmd.github.io/pmd-5.5.2/)
Add custom ruleset [myrules](./PMD/myrules.xml) to pmd-java-5.5.2.jar.

### [CPD Version 5.4.1](http://pmd.github.io/pmd-5.4.1/usage/cpd-usage.html)
A custom CPDRunner `CPDRunner.java` /bat file

### [cloc](https://github.com/AlDanial/cloc)

### [SQLite database](https://www.sqlite.org/)

### Java code
The following libraries are needed to run the Java code:
* blackboxAnalyser Java library (for connecting to the Blackbox DB)
* PMD 5.4.1 libraries (for CPD)
* JUnit (for unit tests in `BBTests.java`)

### Haskell code
The following libraries are needed to run the Haskell code:
* [HDBC-Sqlite3](https://hackage.haskell.org/package/HDBC-sqlite3)
* [HDBC](https://hackage.haskell.org/package/HDBC)
* [cassava](https://hackage.haskell.org/package/cassava) (for csv processing)

Create the following values to store your local settings, such as data directories and executable directories:
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

2. [Java] Extract the code files from these days into folder '4daysJ' using `Main.extract4Days()`.

3. [Haskell] Run `Main.issueSelection mySettings` that produces a csv-file with PMD output
4. [Haskell] Run `Main.issueSelectionCPD`,
5. [Haskell] Run `processCPDFile "dir\\4daysJ-cpd50.csv"`
6. xlsx for averages etc.

### Extension selection

To retrieve the results described in 3.2.3, run `showNrOfStartupEventsFourWeeks()` in `BlackboxDB`.

### Preparing local database

A local SQLite database is used to store the data needed for the analysis.
<schema>

#### Storing Blackbox data
[Java] Run `Main.fillSpaDB(BlackboxDB db)` to store (startup) events, snapshots and extensions.

#### Storing code file analyses
The payloads and indices for week 37, 50 of 2014 and week 11 and 24 from 2015 were retrieved from the Blackbox server.

##### cloc
##### PMD
##### CPD

#### Cleaning database
[..]

## Reporting

### General

Table 2: Data set summary
[Haskell] `Reports.generalInfo mySettings` + `Reports.generalLocInfo mySettings`

### RQ1
Table 3: Summary of initial PMD run

Table 4: Top 10 issues

Table 5: Issue occurrence
[Haskell] `Reports.issueOccs mySettings`
[Java]

Figure 1: Issues over time
[SQL]
### RQ2

Table 6: Issues Fixes
[Java] `Main.issueFixing()`

### RQ3

Table 7: Extension use
[Haskell]

Figure 2: Issues and extension use
[SQL]
