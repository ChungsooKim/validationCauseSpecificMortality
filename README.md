Cause-specific mortality Validation package
========================================================


<img src="https://img.shields.io/badge/Study%20Status-Design%20Finalized-brightgreen.svg" alt="Study Status: Design Finalized">

- Analytics use case(s): **Patient-Level Prediction**
- Study type: **Clinical Application**
- Tags: **-**
- Study lead: **Chungsoo Kim**, **Seng Chan You**, **Rae Woong Park***
- Study lead forums tag: **[[Chungsoo_Kim]](https://forums.ohdsi.org/u/Chungsoo_Kim)**, **[[SCYou]](https://forums.ohdsi.org/u/SCYou)**, **[[RWPark]](https://forums.ohdsi.org/u/rwpark)**
- Study start date: **June 1, 2019**
- Study end date: **-**
- Protocol: **-**
- Publications: **-**
- Results explorer: **-**

Validation of cause of death predictive model to other databases.


Instructions To Build Package
===================

- Build the package by clicking the R studio 'Install and Restart' button in the built tab 



Instructions To Run Package
===================


```r
library(ValidationCauseSpecificMortality)

# USER INPUTS
#=======================
# The folder where the study intermediate and result files will be written:
outputFolder <- "./ValidationResult"

# Specify where the temporary files (used by the ff package) will be created:
options(fftempdir = "location with space to save big data")

# Details for connecting to the server:
dbms <- "you dbms"
user <- 'your username'
pw <- 'your password'
server <- 'your server'
port <- 'your port'

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                server = server,
                                                                user = user,
                                                                password = pw,
                                                                port = port)

# Add the database containing the OMOP CDM data
cdmDatabaseSchema <- 'cdm database schema'
# Add a database with read/write access as this is where the cohorts will be generated
cohortDatabaseSchema <- 'work database schema'

oracleTempSchema <- NULL

# table name where the cohorts will be generated
cohortTable <- 'ValidationCohort'

#=======================

execute(connectionDetails,
        databaseName,
        cdmDatabaseSchema,
        cohortDatabaseSchema,
        oracleTempSchema,
        cohortTable,
        outputFolder,
        createCohorts = T,
        runValidation = T,
        createValidationTable = F,
        causeValidation = F,
        packageResults = F,
        minCellCount = 1,
        sampleSize = NULL)

PatientLevelPrediction::viewMultiplePlp("outputFolder")



```
- For validation of cause prediction model, please run causeValidation function after running createValidationTable function.
  
```r

TAR <- c(30,60,90,180,365)
lapply(TAR, function(x) createValidationTable(outputFolder, TAR = x))
lapply(TAR, function(y) causeValidation(outputFolder, TAR = y))

```


# Development status
Under development. Do not use