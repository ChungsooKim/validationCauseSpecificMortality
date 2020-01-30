#' Execute the validation study
#' @NAME createValidationTable
#' @details This function will create the table for validation
#' @import dplyr 
#' @import ROCR
#' @import pROC
#' @import caret
#' @import ParallelLogger
#'
#' @param TAR                  Time at risk for determining risk window
#' @param outputFolder         Name of local folder to place results; make sure to use forward slashes
#'                             (/)
#' @export


createValidationTable <- function(outputFolder, TAR = 60) {
  
  ### Set save folder directory
  ParallelLogger::logInfo("Setting save folder...")
  saveFolder <- file.path(outputFolder, "CausePredictionValidationResults")
  if (!file.exists(saveFolder))
    dir.create(saveFolder)
  
  outpath <- file.path(getwd(), "inst", "settings", "settings.csv")
  settings <- utils::read.csv(outpath)
  
  ### Check Directory
  ParallelLogger::logInfo("Check Directory...")
  for (analysisNum in 1: length(settings$analysisId)) {
    saveFolder <- file.path(outputFolder, databaseName, "Analysis")
    saveFolder <- paste(saveFolder, analysisNum, sep = "_")
    if (!file.exists(saveFolder))
      dir.create(saveFolder)
  }
  
  ### Read setting file in plpResult folder
  ParallelLogger::logInfo("Read a setting file in plpResult folder...")
  settings <- settings %>% filter(riskWindowEnd == TAR)
  length <- length(settings$plpResultFolder)
  id <- settings$analysisId
  
  outList <- vector(mode = "list", length= length)
  
  for(i in 1:length) {
    analysispath <- paste(outputFolder, databaseName, "Analysis", sep = "/")
    analysispath <- paste(analysispath, id[i], sep = "_")
    outList[i] <- analysispath
  }
  
  ### Making blank result for No outcome models
  ParallelLogger::logInfo("Making blank result for no outcome model...")
  blankNum <- 1
  ifelse(length(list.files(file.path(outList[[blankNum]]))) == 0, blankNum <- sample(x = 1 : length, 1), blankNum)
  
  validationResult <- readRDS(file.path(outList[[blankNum]], "validationResult.rds"))
  count <- length(validationResult$prediction$subjectId)
  rowId <- unlist(validationResult$prediction$rowId)
  subjectId <- unlist(validationResult$prediction$subjectId)
  outcomeCount <- rep(0, time = count)
  value <- rep(0, time = count)
  blankResult <- data.frame(rowId = rowId, subjectId = subjectId, outcomeCount = outcomeCount, value = value)
  
  ### Read RDS files
  for(j in 1:length){
    if (length(list.files(file.path(outList[[j]]))) == 1) {
      rds <- readRDS(file.path(outList[[j]], "validationResult.rds"))
      outList[[j]] <- rds$prediction
    } else {outList[[j]] <- blankResult}
    
    names(outList)[j] <- paste("prediction", j, sep = "_")
  }
  
  
  ### Merge prediction values and outcomes
  ParallelLogger::logInfo("Creating validation table...")
  outDFvalue1 <- data.frame()
  outDFvalue2 <- data.frame()
  model1 <- which(settings$modelSettingId == 1)
  model2 <- which(settings$modelSettingId == 2)
  
  for (j in model1) {
    df1 <- outList[[j]] %>% select(subjectId, value)
    colnames(df1)[2]<- paste(settings$outcomeName[j], settings$modelSettingsId[j], sep = "_")
    if (length(outDFvalue1) == 0) {
      outDFvalue1 <- df1
    }
    else{
      outDFvalue1 <- dplyr::left_join(outDFvalue1, df1, by = "subjectId")
    }
  }
  valueName <- c("subjectId", "DeathValue1", "CancerValue1",
                 "IHDValue1", "CerebroValue1", "PneumoValue1",
                 "DMValue1", "LiverValue1", "CLRDValue1", "HTValue1")
  names(outDFvalue1) <- valueName
  
  for (j in model2) {
    df2 <- outList[[j]] %>% select(subjectId, value)
    colnames(df2)[2]<- paste(settings$outcomeName[j], settings$modelSettingsId[j], sep = "_")
    if (length(outDFvalue2) == 0) {
      outDFvalue2 <- df2
    }
    else{
      outDFvalue2 <- dplyr::left_join(outDFvalue2, df2, by = "subjectId")
    }
  }
  
  valueName <- c("subjectId", "DeathValue2", "CancerValue2",
                 "IHDValue2", "CerebroValue2", "PneumoValue2",
                 "DMValue2", "LiverValue2", "CLRDValue2", "HTValue2")
  names(outDFvalue2) <- valueName
  
  
  outDFoutcome <- data.frame()
  for (j in model1) {
    df3 <- outList[[j]] %>% select(subjectId, outcomeCount)
    colnames(df3)[2]<- paste(paste("Label", settings$outcomeName[j], sep = "_"),
                             settings$modelSettingsId[j], sep = "_")
    if (length(outDFoutcome) == 0) {
      outDFoutcome <- outList[[j]] %>% select(subjectId, outcomeCount)
      colnames(outDFoutcome)[2] <- paste(paste("Label", settings$outcomeName[j], sep = "_"),
                                         settings$modelSettingsId[j], sep = "_")
    }
    else{
      outDFoutcome <- left_join(outDFoutcome, df3, by = "subjectId")
    }
  }
  
  labelName <- c("subjectId", "DeathLabel", "CancerLabel",
                 "IHDLabel", "CerebroLabel", "PneumoLabel",
                 "DMLabel", "LiverLabel", "CLRDLabel", "HTLabel")
  names(outDFoutcome) <- labelName
  
  outDF <- left_join(outDFoutcome, outDFvalue1, by = "subjectId")
  outDF <- left_join(outDF, outDFvalue2, by = "subjectId")
  
  ### save file in save directory
  ParallelLogger::logInfo("Save validation table file in save folder...")
  savepath <- file.path(outputFolder, "out_df_")
  savepath <- paste(savepath,TAR,".rds", sep = "")
  saveRDS(outDF, file = savepath)
}
