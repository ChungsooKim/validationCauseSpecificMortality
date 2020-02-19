#' Execute the validation study
#' @NAME causeValidation
#' @details This function will run the random forest model for classify causes of death
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


causeValidation <- function(outputFolder, TAR){
  
  `%notin%` <- Negate(`%in%`)
  
  saveFolder <- file.path(outputFolder, "causePredictionValidationResults")
  if (!file.exists(saveFolder))
    dir.create(saveFolder, recursive = T)
  
  savepath <- file.path(outputFolder, "out_df_")
  savepath <- paste(savepath,TAR,".rds", sep = "")
  outDF <- readRDS(savepath)
  
  ParallelLogger::logInfo("Validation Start...")
  ParallelLogger::logInfo("Read validation table file in save folder...")
  
  
  ### Cause Labeled in Database
  ParallelLogger::logInfo("Creating cause label column...")
  labelStart <- 3
  labelEnd <- (length(outDF)-1)/3+1
  labelNum <- labelEnd - labelStart + 1
  sum <- apply(outDF[,labelStart:labelEnd], 1, sum)
  if(max(sum) != 1) warning("label is wrong")
  
  outDF$sum <- sum
  outDF <- outDF %>% filter(sum < 2)
  
  max <- apply(outDF[,labelStart:labelEnd], 1, which.max)
  max <- as.numeric(max)
  CauseLabel <- max
  
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
  cause.model <- readRDS(modelpath)
  # finalModelPath <- system.file(file.path(finalModel,sprintf("final_model/%s.rds",TAR)), package = "CauseSpecificMortalityValidation")
  # cause.model <- readRDS(finalModelPath)
  
  ### Result
  ParallelLogger::logInfo("Predicting response and calculating prediction values...")
  dataTest <- outDF
  dataTest$CauseLabel <- as.character(dataTest$CauseLabel)
  dataTest$CauseLabel <- as.factor(dataTest$CauseLabel)
  
  dataTestResult <- dataTest
  dataTestResult$cause.prediction <- predict(cause.model, dataTest)
  dataTestResult$cause.value <- predict(cause.model, dataTest, type = "prob")
  
  ### Measuring model performance
  ParallelLogger::logInfo("Measuring a model performance...")
  
  ### Accuracy
  ParallelLogger::logInfo("Calculating accuracy...")
  dfPerformance <- dataTestResult
  
  lev <- as.character(seq(0,labelNum))
  lev <- c(lev, "99")
  levels(dfPerformance$CauseLabel) <- c(intersect(lev, levels(dfPerformance$CauseLabel)), 
                                        setdiff(lev, levels(dfPerformance$CauseLabel)))
  
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
  acc <- calculate.accuracy(dfPerformance$cause.prediction, dfPerformance$CauseLabel)
  print(paste0("Accuracy is: ", round(acc, 4)))
  
  weights <- rep(1 / length(levels(dfPerformance$cause.prediction)), length(levels(dfPerformance$CauseLabel)))
  w.acc <- calculate.w.accuracy(dfPerformance$cause.prediction, dfPerformance$CauseLabel, weights)
  print(paste0("Weighted accuracy is: ", round(w.acc, 4)))
  
  ### Confusion Matrix
  cm <- vector("list", length(levels(dfPerformance$CauseLabel)))
  for (i in seq_along(cm)) {
    positive.class <- levels(dfPerformance$CauseLabel)[i]
    cm[[i]] <- caret::confusionMatrix(dfPerformance$cause.prediction, dfPerformance$CauseLabel,
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
  
  classes <- dfPerformance$CauseLabel
  
  savepath <- paste("PRcurve", TAR, sep = "_")
  savepath <- paste(savepath, ".tiff")
  savepath <- file.path(saveFolder, savepath)
  
  tiff(savepath, 3200, 3200, units = "px", res = 800)
  
  plot(x=NA, y=NA, xlim=c(0,1), ylim=c(0,1), ylab="Precision", xlab="Recall", bty="o")
  colors <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6","#6a3d9a")
  aucs <- rep(NA, length(levels(classes)))
  for (i in seq_along(levels(classes))) {
    cur.classes <- levels(classes)[i]
    test.labels <- dfPerformance$cause.prediction == cur.classes
    try({pred <- ROCR::prediction(dfPerformance$cause.value[,i], test.labels)
    perf <- ROCR::performance(pred, "prec", "rec")
    roc.x <- unlist(perf@x.values)
    roc.y <- unlist(perf@y.values)
    # for baseline
    # ab <- get.conf.stats(cm)
    # ab <- ab %>% mutate(p = tp + fn, total = length(dfPerformance$CauseLabel)) %>% mutate(baseline = p/total)
    # abline(a= ab$baseline[i], b=0, col = colors[i], lwd = 2)
    lines(roc.y ~ roc.x, col = colors[i], lwd = 2)})
    dataTest_true <- as.data.frame(dfPerformance$cause.value)
    dataTest_true$trueClass <- ifelse(dfPerformance$cause.prediction == cur.classes, 1 ,0)
    dataTest_pos <- dataTest_true %>% dplyr::filter(trueClass == 1)
    dataTest_neg <- dataTest_true %>% dplyr::filter(trueClass == 0)
    pr <- PRROC::pr.curve(scores.class0 = dataTest_pos[,i], scores.class1 = dataTest_neg[,i], curve = T)
    aucs[i] <- pr$auc.integral
  }
  
  legend(0.02,0.5, bty = "n",
         legend=c("No Death", "Malignant cancer", "Ischemic heart disease", "Cerebrovascular disease",
                  "Pneumonia", "Diabetes", "Liver disease", "Chronic lower respiratory disease", "Hypertensive disease"),
         col=c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f","#ff7f00", "#cab2d6","#6a3d9a"), lwd = 2)
  
  print(paste0("Mean AUC under the precision-recall curve is :", round(mean(aucs[is.nan(aucs)==F]), 4)))
  
  dev.off()
  
  
  ### Receiver Operating Characteristics Plot
  ParallelLogger::logInfo("Creating ROC curves...")
  
  dfPerformance$CauseLabel <- as.character(dfPerformance$CauseLabel)
  auroc<- pROC::multiclass.roc(dfPerformance$CauseLabel, dfPerformance$cause.value)
  print("The receiver operating characteristics curve :")
  print(auroc$auc)
  
  savepath <- paste("ROCcurve", TAR, sep = "_")
  savepath <- paste(savepath, ".tiff")
  savepath <- file.path(saveFolder, savepath)
  
  tiff(savepath, 3200, 3200, units = "px", res = 800)
  colorset <- c("#a6cee3","#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a")
  
  par(pty = "s")
  try(pROC::plot.roc(dfPerformance[,2], dfPerformance$cause.value[,1], legacy.axes = T, percent = F, col = colorset[1], identity = F))
  for (i in 2:labelNum+1){
    try(pROC::lines.roc(dfPerformance[,i+1], dfPerformance$cause.value[,i], col = colorset[i], identity = F))
  }
  try(pROC::lines.roc(dfPerformance$OtherLabel, dfPerformance$cause.value[,labelNum + 2], col = colorset[labelNum+1], identity = F))
  
  legend("bottomright", bty = "n",
         legend=c("No Death", "Malignant cancer", "Ischemic heart disease", "Cerebrovascular disease",
                  "Pneumonia", "Diabetes", "Liver disease", "Chronic lower respiratory disease", "Hypertensive disease", "Others"),
         col=c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f","#ff7f00", "#cab2d6","#6a3d9a"), lwd = 2)
  # 
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
  saveRDS(dfPerformance$cause.value, file = savepath)
  
  savepath <- paste("table1", TAR, sep = "_")
  savepath <- paste(savepath, ".csv")
  savepath <- file.path(saveFolder, savepath)
  write.csv(table1, file = savepath)
  
  savepath <- paste("table2", TAR, sep = "_")
  savepath <- paste(savepath, ".csv")
  savepath <- file.path(saveFolder, savepath)
  write.csv(table2, file = savepath)
  
  ParallelLogger::logInfo("DONE")
  
}

