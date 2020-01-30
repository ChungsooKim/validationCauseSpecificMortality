#' Execute the validation study
#' @NAME causeValidation
#' @details This function will run the random forest model for classify causes of death
#' @import dplyr 
#' @import ROCR
#' @import pROC
#' @import caret
#' @import ParallelLogger
#' @importFrom magrittr %>%
#'
#' @param TAR                  Time at risk for determining risk window
#' @param outputFolder         Name of local folder to place results; make sure to use forward slashes
#'                             (/)
#' @export


causeValidation <- function(outputFolder, TAR = 60){
  
  saveFolder <- file.path(outputFolder, "causePredictionValidationResults")
  if (!file.exists(saveFolder))
    dir.create(saveFolder, recursive = T)
  
  savepath <- file.path(outputFolder, "out_df_")
  savepath <- paste(savepath,TAR,".rds", sep = "")
  outDF <- readRDS(savepath)
  
  ParallelLogger::logInfo("Validation Start...")
  ParallelLogger::logInfo("Read validation table in save folder...")
  
  
  ### Cause Labeled in Database
  ParallelLogger::logInfo("Creating cause label column...")
  labelStart <- 3
  labelEnd <- (length(outDF)-1)/3+1
  labelNum <- labelEnd - labelStart + 1
  sum <- apply(outDF[,labelStart:labelEnd], 1, sum)
  outDF$sum <- sum
  outDF <- outDF %>% filter(sum < 2)
  
  max <- apply(outDF[,labelStart:labelEnd], 1, which.max)
  max <- as.numeric(max)
  CauseLabel <- max
  if(max(sum) != 1) warning("label is wrong")
 
  CauseLabel <- ifelse(outDF[,2] == 0 , 0, CauseLabel)
  CauseLabel <- ifelse(outDF[,2] == 1 & outDF$sum == 0, 99, CauseLabel)
  outDF$CauseLabel <- CauseLabel
  OtherLabel <- ifelse(CauseLabel == 99, 1, 0)
  outDF$OtherLabel <- OtherLabel
  outDF <- outDF %>% select(-sum)
  
  ### Run Validation
  ParallelLogger::logInfo("Read the cause prediction model...")
  modelpath <- paste(getwd(), "inst", "finalModels", "final_model", sep = "/")
  modelpath <- paste(modelpath, TAR, sep = "_")
  modelpath <- paste(modelpath, "rds", sep = ".")
  fitModel <- readRDS(modelpath)
  
  ### Result
  ParallelLogger::logInfo("Predicting response and calculating prediction values...")
  dataTest <- outDF
  dataTest$CauseLabel <- as.character(dataTest$CauseLabel)
  dataTest$CauseLabel <- as.factor(dataTest$CauseLabel)
  
  dataTestResult <- dataTest
  dataTestResult$cause.prediction <- predict(fitModel, dataTest, type = "response")
  dataTestResult$cause.value <- predict(fitModel, dataTest, type = "prob")
  
  for(j in 0:(labelNum+1)){
    colname <- paste("cause.value", j, sep = ".")
    dataTestResult[,colname] <- predict(fitModel, dataTest, type = "prob")[,j+1]
  }
  
  dataTestValue <- predict(fitModel, dataTest, type = "prob")
  colnames(dataTestValue)<-c("NoDeath","Cancer","IHD", "Cerebro",
                             "Pneumonia", "DM", "Liver", "CLRD", "HT", "Others")
  
  ### Measuring model performance
  ParallelLogger::logInfo("Measuring a model performance...")
 
   ### Accuracy
  ParallelLogger::logInfo("Calculating accuracy...")
  dfAccuracy <- dataTestResult
  levels(dfAccuracy$CauseLabel) <- c("0", "1", "2", "3", "4", "6", "7", "99", "5", "8")
  calculate.accuracy <- function(predictions, ref.labels) {
    return(length(which(predictions == ref.labels)) / length(ref.labels))
  }
  calculate.w.accuracy <- function(predictions, ref.labels, weights) {
    lvls <- levels(ref.labels)
    if (length(weights) != length(lvls)) {
      stop("Number of weights should agree with the number of classes.")
    }
    if (sum(weights) != 1) {
      stop("Weights do not sum to 1")
    }
    accs <- lapply(lvls, function(x) {
      idx <- which(ref.labels == x)
      return(calculate.accuracy(predictions[idx], ref.labels[idx]))
    })
    accs <- unlist(accs)
    accs <- accs[is.nan(accs) == FALSE]
    acc <- mean(accs)
    return(acc)
  }
  acc <- calculate.accuracy(dfAccuracy$cause.prediction, dfAccuracy$CauseLabel)
  print(paste0("Accuracy is: ", round(acc, 4)))
  
  weights <- rep(1 / length(levels(dfAccuracy$cause.prediction)), length(levels(dfAccuracy$CauseLabel)))
  w.acc <- calculate.w.accuracy(dfAccuracy$cause.prediction, dfAccuracy$CauseLabel, weights)
  print(paste0("Weighted accuracy is: ", round(w.acc, 4)))
  
  ### Confusion Matrix
  cm <- vector("list", length(levels(dfAccuracy$CauseLabel)))
  for (i in seq_along(cm)) {
    positive.class <- levels(dfAccuracy$CauseLabel)[i]
    cm[[i]] <- confusionMatrix(dfAccuracy$cause.prediction, dfAccuracy$CauseLabel,
                               positive = positive.class)
  }
  
  print(paste0("Confusion Matrix"))
  table1 <- cm[[1]]$table
  print(table1)
  table2 <- cm[[1]]$byClass
  print(table2)
  
  get.conf.stats <- function(cm) {
    out <- vector("list", length(cm))
    for (i in seq_along(cm)) {
      x <- cm[[i]]
      tp <- x$table[x$positive, x$positive]
      fp <- sum(x$table[x$positive, colnames(x$table) != x$positive])
      fn <- sum(x$table[colnames(x$table) != x$positive, x$positive])
      # TNs are not well-defined for one-vs-all approach
      elem <- c(tp = tp, fp = fp, fn = fn)
      out[[i]] <- elem
    }
    df <- do.call(rbind, out)
    rownames(df) <- unlist(lapply(cm, function(x) x$positive))
    return(as.data.frame(df))
  }
  
  ### Micro F1
  ParallelLogger::logInfo("Calculating F1 scores...")
  get.micro.f1 <- function(cm) {
    cm.summary <- get.conf.stats(cm)
    tp <- sum(cm.summary$tp)
    fn <- sum(cm.summary$fn)
    fp <- sum(cm.summary$fp)
    pr <- tp / (tp + fp)
    re <- tp / (tp + fn)
    f1 <- 2 * ((pr * re) / (pr + re))
    return(f1)
  }
  micro.f1 <- get.micro.f1(cm)
  print(paste0("Micro F1 is: ", round(micro.f1, 4)))
  
  ### Macro F1
  get.macro.f1 <- function(cm) {
    c <- cm[[1]]$byClass # a single matrix is sufficient
    c <- na.omit(c)
    re <- sum(c[, "Recall"]) / nrow(c)
    pr <- sum(c[, "Precision"]) / nrow(c)
    f1 <- 2 * ((re * pr) / (re + pr))
    return(f1)
  }
  macro.f1 <- get.macro.f1(cm)
  print(paste0("Macro F1 is: ", round(macro.f1, 4)))
  
  
  ### Precision Recall curve (PR curve)
  ParallelLogger::logInfo("Creating Precision-Recall curve...")
  classes <- dataTestResult$CauseLabel
  levels(classes) <- c("0", "1","2","3","4","6","7","99","5","8")
  
  savepath <- paste("PR curve", TAR, sep = "_")
  savepath <- paste0(savepath, ".tiff")
  savepath <- file.path(saveFolder, savepath)
  
  tiff(savepath, 3200, 3200, units = "px", res = 800)
  
  plot(x=NA, y=NA, xlim=c(0,1), ylim=c(0,1), ylab="Precision", xlab="Recall", bty="n")
  
  colors <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99",
              "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6","#6a3d9a")
  
  aucs <- rep(NA, length(levels(classes)))
  for (i in seq_along(levels(classes))) {
    cur.classes <- levels(classes)[i]
    test.labels <- dataTestResult$cause.prediction == cur.classes
    try({pred <- prediction(dataTestValue[,i], test.labels)
    perf <- performance(pred, "prec", "rec")
    roc.x <- unlist(perf@x.values)
    roc.y <- unlist(perf@y.values)
    # for baseline
    # ab <- get.conf.stats(cm)
    # ab <- ab %>% mutate(p = tp + fn, total = length(dfAccuracy$CauseLabel)) %>% mutate(baseline = p/total)
    # abline(a= ab$baseline[i], b=0, col = colors[i], lwd = 2)
    lines(roc.y ~ roc.x, col = colors[i], lwd = 2)})
    dataTest_true <- as.data.frame(dataTestValue)
    dataTest_true$trueClass <- ifelse(dataTestResult$cause.prediction == cur.classes, 1 ,0)
    dataTest_pos <- dataTest_true %>% dplyr::filter(trueClass == 1)
    dataTest_neg <- dataTest_true %>% dplyr::filter(trueClass == 0)
    pr <- PRROC::pr.curve(scores.class0 = dataTest_pos[,i], scores.class1 = dataTest_neg[,i], curve = T)
    aucs[i] <- pr$auc.integral
  }
  
  legend("bottomleft", bty = "n",
         legend=c("Survival", "Malignant neoplastic disease", "Ischemic heart disease",
                  "Cerebrovascular disease", "Pneumonia", "Diabetes mellitus", "Liver disease", 
                  "Chronic lower respiratory disease", "Hypertensive disease"),
         col=c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99",
               "#e31a1c", "#fdbf6f","#ff7f00", "#cab2d6","#6a3d9a"), lwd = 2)

  dev.off()
  
  print(paste0("Mean AUC under the precision-recall curve is :", round(mean(aucs[is.nan(aucs)==F]), 4)))
  
  ### Receiver Operating Characteristics Plot
  ParallelLogger::logInfo("Creating ROC curves...")
  dfRoc <- dataTest
  levels(dfRoc$CauseLabel) <- c("0", "1","2","3", "4", "6", "7", "99", "5", "8")
  dfRoc$CauseLabel <- as.character(dfRoc$CauseLabel)
  dfRoc$CauseLabel <- ifelse(dfRoc$CauseLabel=="0", "NoDeath", dfRoc$CauseLabel)
  dfRoc$CauseLabel <- ifelse(dfRoc$CauseLabel=="1", "Cancer", dfRoc$CauseLabel)
  dfRoc$CauseLabel <- ifelse(dfRoc$CauseLabel=="2", "IHD", dfRoc$CauseLabel)
  dfRoc$CauseLabel <- ifelse(dfRoc$CauseLabel=="3", "Cerebro", dfRoc$CauseLabel)
  dfRoc$CauseLabel <- ifelse(dfRoc$CauseLabel=="4", "Pneumonia", dfRoc$CauseLabel)
  dfRoc$CauseLabel <- ifelse(dfRoc$CauseLabel=="5", "DM", dfRoc$CauseLabel)
  dfRoc$CauseLabel <- ifelse(dfRoc$CauseLabel=="6", "Liver", dfRoc$CauseLabel)
  dfRoc$CauseLabel <- ifelse(dfRoc$CauseLabel=="7", "CLRD", dfRoc$CauseLabel)
  dfRoc$CauseLabel <- ifelse(dfRoc$CauseLabel=="8", "HT", dfRoc$CauseLabel)
  dfRoc$CauseLabel <- ifelse(dfRoc$CauseLabel=="99", "Others", dfRoc$CauseLabel)
  
  savepath <- paste("ROC curve", TAR, sep = "_")
  savepath <- paste(savepath, ".tiff")
  savepath <- file.path(saveFolder, savepath)
  
  tiff(savepath, 3200, 3200, units = "px", res = 800)
  
  auroc<- pROC::multiclass.roc(dfRoc$CauseLabel, dataTestValue)
  print("The receiver operating characteristics curve :")
  print(auroc$auc)

  par(pty = "s")
  try(plot0 <- plot.roc(dataTestResult$DeathLabel, dataTestResult$cause.value.0, 
                        legacy.axes = TRUE, percent = F, col = "#a6cee3"))
  try(plot1 <- lines.roc(dataTestResult$CancerLabel, dataTestResult$cause.value.1, col = "#1f78b4"))
  try(plot2 <- lines.roc(dataTestResult$IHDLabel, dataTestResult$cause.value.2, col = "#b2df8a"))
  try(plot3 <- lines.roc(dataTestResult$CerebroLabel, dataTestResult$cause.value.3, col = "#33a02c"))
  try(plot4 <- lines.roc(dataTestResult$PneumoLabel, dataTestResult$cause.value.4, col = "#fb9a99"))
  try(plot5 <- lines.roc(dataTestResult$DMLabel, dataTestResult$cause.value.5, col = "#e31a1c"))
  try(plot6 <- lines.roc(dataTestResult$LiverLabel, dataTestResult$cause.value.6, col = "#fdbf6f"))
  try(plot7 <- lines.roc(dataTestResult$CLRDLabel, dataTestResult$cause.value.7, col = "#ff7f00"))
  try(plot8 <- lines.roc(dataTestResult$HTLabel, dataTestResult$cause.value.8, col = "#cab2d6"))
  try(plot99 <- lines.roc(dataTestResult$OtherLabel, dataTestResult$cause.value.9, col = "#6a3d9a"))
  
  legend("bottomright", bty = "n",
         legend=c("Survival", "Malignant neoplastic disease", "Ischemic heart disease", 
                  "Cerebrovascular disease", "Pneumonia", "Diabetes mellitus", 
                  "Liver disease", "Chronic lower respiratory disease", "Hypertensive disease", "Others"),
         col=c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", 
               "#e31a1c", "#fdbf6f","#ff7f00", "#cab2d6","#6a3d9a"), lwd = 2)
  
  dev.off()
  
  
  ### Save files in saveFolder
  ParallelLogger::logInfo("Saving the results...")
  
  savepath <- paste("dataTestResult", TAR, sep = "_")
  savepath <- paste(savepath, ".rds")
  savepath <- file.path(saveFolder, savepath)
  saveRDS(dataTestResult, file = savepath)
  
  savepath <- paste("dataTestValue", TAR, sep = "_")
  savepath <- paste(savepath, ".rds")
  savepath <- file.path(saveFolder, savepath)
  saveRDS(dataTestValue, file = savepath)
  
  ParallelLogger::logInfo("DONE")
  
}

