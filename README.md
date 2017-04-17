# Student code quality

This document describes the steps to perform the automated analysis of student programs, as presented in the paper ‘Code Quality Issues in Student Programs’ [to be published].
The code is provided to perform checks, extend the analysis and allow replication. Comments and questions can be emailed to the first author.
Both Java and Haskell is used for the automated analysis.

## Installation

### Blackbox database

The database used is not publicly available. Permission to access the [Blackbox database](https://www.bluej.org/blackbox.html) needs to be requested with the maintainers.

### PMD
[PMD Version 5.5.2](http://pmd.github.io/pmd-5.5.2/)

### CPD
[CPD Version 5.4.1](http://pmd.github.io/pmd-5.4.1/usage/cpd-usage.html)

### cloc
[cloc](https://github.com/AlDanial/cloc)

SQLite, Blackbox Java API, PMD API, CPD API.

## Pre-processing

### Settings

Create the following values to store your local settings, such as data directories and executable directories:
* `mySettings` of type `Settings`.
* `myPMDSettings` of type `CPDSettings`
* `myCPDSettings` of type `PMDSettings`

### Issue selection

1. Copy the payload and index files of the days below from the Blackbox server to a local folder
   * 8 September 2014 
   * 8 December 2014
   * 9 March 2015
   * 8 June 2015

2. Extract the code files from these days.
3. 
3. Run `Main.issueSelection mySettings` that produces a csv-file with PMD output
4. Run.. CPD
5. 

### Preparing local database
A local SQLite database is used to store the data needed for the analysis.
<schema>

#### Cleaning database
[..]

#### Extracting code files

The payloads and indices for week 37, 50 of 2014 and week 11 and 24 from 2015 were retrieved from the Blackbox server.


## Reporting

### RQ1
Table 3 Summary of initial PMD run

Table 4 Top 10 issues

Figure 1 Issues over time

### RQ2

Table 6 Issues Fixes

### RQ3

Table 7 Extension use

Figure 6 Issues and extension use
