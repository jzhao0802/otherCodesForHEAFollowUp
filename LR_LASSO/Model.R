library(doParallel)
library(parallel)

source("functions/Model_1Class_SVM.R")
source("functions/Model_2Class_SVM.R")
source("functions/Model_LR_LASSO.R")

CON_CLASS_TYPE <- list(SVM_ONE_CLASS=1,
                       SVM_TWO_CLASS=2, 
                       LR_LASSO=3)

SaveResult <- function(result, resultDir)
{
  saveRDS(list(model=result$modelForFuture,
               type=result$modelType,
               additionalInfo=result$additionalInfo), 
          file=paste(resultDir, "model.rds", sep=""))
  
  accuracies <- c(result$accuracyPos, 
                  result$accuracyNeg)
  names(accuracies) <- c("Positive", "Negative")
  write.table(accuracies, sep=",", 
              file=paste(resultDir, "accuracies.csv", sep=""), col.names=NA)
  
  write.table(result$bestParamsAllEvalFolds, sep=",", 
              file=paste(resultDir, "bestParamsAllEvalFolds.csv", sep=""),
              col.names=NA)
  
  write.table(result$predsNeg, sep=",", 
              file=paste(resultDir, "predsNegData.csv", sep=""),
              col.names=F, row.names=F)
  
  write.table(result$predsPos, sep=",", 
              file=paste(resultDir, "predsPosData.csv", sep=""),
              col.names=F, row.names=F)
}

Model <- function(arglist)
{
  #
  # parse
  
  bParallel <- arglist$bParallel
  if (bParallel)
  {
    if (is.null(arglist$nCores2Use))
      nCores2Use <- detectCores() - 1
    else
      nCores2Use <- arglist$nCores2Use
    
    cl <- makeCluster(nCores2Use)
    registerDoParallel(cl, cores = nCores2Use)
  }
  
  dataDir <- arglist$dataDir
  
  
  if (is.null(arglist$kEvalFolds))
    stop("Error! kEvalFolds is missing. ")
  kEvalFolds <- arglist$kEvalFolds
  
  if (is.null(arglist$kValiFolds))
    stop("Error! kValiFolds is missing. ")
  kValiFolds <- arglist$kValiFolds
  
  bCleanData <- arglist$bCleanData
  
  #
  
  classType <- arglist$classType
  
  # 
  
  if (classType == CON_CLASS_TYPE$SVM_ONE_CLASS)
  {
    if (is.null(arglist$nus))
      stop("Error! nus must be specified for a one-class model. ")
    nus <- arglist$nus
    
    cvCriterion <- arglist$cvCriterion
    if (cvCriterion == CON_CV_CRITERIA$SPEC_ACCURACY)
    {
      if (is.null(arglist$specAccuracy))
        stop("Error! A specAccuracy must be given if CON_CV_CRITERIA$SPEC_ACCURACY is chosen.")
      specAccuracy <- arglist$specAccuracy
    } else
      specAccuracy <- NULL
  }
  else if (classType == CON_CLASS_TYPE$SVM_TWO_CLASS)
  {
    if (is.null(arglist$logCs))
      stop("Error! logCs must be specified for a SVM two-class model. ")
    logCs <- arglist$logCs
    Cs <- 10^logCs
  } else if (classType == CON_CLASS_TYPE$LR_LASSO)
  {
    if (is.null(arglist$logLambdas))
      stop("Error! logLambdas must be specified for a LR LASSO model. ")
    logLambdas <- arglist$logLambdas
    lambdas <- 10^logLambdas
  }
  else
    stop("Error! Invalid classType.")
  
  if ((classType == CON_CLASS_TYPE$SVM_ONE_CLASS) |
      (classType == CON_CLASS_TYPE$SVM_TWO_CLASS))
  {
    if (is.null(arglist$kernel))
      stop("Error! kernel must be specified. Candidate values: 'radial' and 'linear'. ")
    kernel <- arglist$kernel
    
    if (kernel == "radial")
    {
      logGammas <- arglist$logGammas
      gammas <- 10^(logGammas)
    } else if (kernel == "linear")
    {
      gammas <- NULL
    } else
      stop("Error! Invalid kernel!")
  }
  
  
  #
  ptm <- proc.time()
  if (is.null(arglist$resultDir))
  {
    timeStamp <- as.character(Sys.time())
    timeStamp <- gsub(":", ".", timeStamp)  # replace ":" by "."
    resultDir <- paste("./Results/", timeStamp, "/", sep = '')
    dir.create(resultDir, showWarnings = TRUE, recursive = TRUE, mode = "0777")
  } else
    resultDir <- arglist$resultDir
  
  #
  ## save all the input arguments
  
  fileArgList <- file(paste(resultDir, "arglist.txt", sep=""), "w")
  for (name in names(arglist))
  {
    if ((name == "nus") | (name == "logGammas") | (name == "Cs"))
    {
      n_candidates <- length(arglist[[name]])
      start <- arglist[[name]][1]
      end <- arglist[[name]][n_candidates]
      writeLines(paste(name, ": bounds [", start, ", ", end, "], nCandidates: ", 
                       n_candidates, "\n", sep=""), fileArgList)
    } else if (name == "classType")
    {
      value <- names(CON_CLASS_TYPE)[as.vector(CON_CLASS_TYPE) == arglist[[name]]]
      writeLines(paste(name,": ", value, "\n", sep=""), fileArgList)
    } else if (name == "cvCriterion")
    {
      value <- names(CON_CV_CRITERIA)[as.vector(CON_CV_CRITERIA) == arglist[[name]]]
      writeLines(paste(name,": ", value, "\n", sep=""), fileArgList)
    } else
    {
      writeLines(paste(name,": ", arglist[[name]], "\n", sep=""), fileArgList)
    }
  }
  close(fileArgList)
  
  #
  ## read data
  
  load(paste(dataDir, "dat_hae_1111_rf_nov26.RData", sep=""))
  # load(paste(dataDir, "dat_nonhae_1111_rf.RData", sep=""))
  load(paste(dataDir, "nonhae_tmp10000.RData", sep=""))
  
  
  
  if (bCleanData)
  {
    # remove patient_id and region
    nonhae_tmp10000 <- 
      nonhae_tmp10000[, !(colnames(nonhae_tmp10000) %in% c("HAE_PATIENT", "REGION"))]
    dat_hae_1111_rf_nov26 <- 
      dat_hae_1111_rf_nov26[, !(colnames(dat_hae_1111_rf_nov26) %in% c("PATIENT_ID", "REGION"))]
    
    # combine positive and negative
    data <- rbind(dat_hae_1111_rf_nov26, nonhae_tmp10000)
    
    # gender
    data <- data[data$GENDER != "U", ] # remove 'U'
    numGender <- rep(0, nrow(data)) # convert to numerics
    numGender[data$GENDER == "M"] <- 1
    data$GENDER <- numGender
    
    # binary-only variables
    binonlyInits <- c("PRC_60", "PRC_64", "DIAG_32")
    binonlyVars <- c(paste(binonlyInits, "FREQ", sep="_"), 
                     paste(binonlyInits, "AFREQ", sep="_"))
    
    data <- data[, !(colnames(data) %in% binonlyVars)]
    
    # remove constant variables
    sds <- apply(data, 2, sd)
    nonconstCols <- !(sds == 0)
    data <- data[, nonconstCols]
    
    
  }
  else
  {
    data <- rbind(dat_hae_1111_rf_nov26, nonhae_tmp10000)
  }
  
  # remove LOOKBACK_DAYS
  data <- data[, !(colnames(data) %in% c("LOOKBACK_DAYS", "lookback_days"))]
  
  #
  
  # for teste
  dataPos <- data[data$HAE==1,]
  dataNeg <- data[data$HAE==0, ]
  dataNeg <- dataNeg[1:1000,]
  data <- rbind(dataPos, dataNeg)
  # test ends
  
#   # for test
#   XPos <- iris[iris$Species=="setosa", 1:4]
#   XNeg <- iris[iris$Species!="setosa", 1:4]
#   # test ends
  
  if (classType == CON_CLASS_TYPE$SVM_ONE_CLASS)
  {
    XPos <- data[data$HAE==1, ]
    XNeg <- data[data$HAE==0, ]
    XPos$HAE <- NULL
    XNeg$HAE <- NULL
    
    result <- Model_1Class_SVM(XModel=XPos, XNew=XNeg, yNew=rep(F, nrow(XNeg)),
                               kEvalFolds=kEvalFolds, kValiFolds=kValiFolds, 
                               nus=nus, gammas=gammas, 
                               kernel=kernel,
                               cvCriterion=cvCriterion, specAccuracy=specAccuracy,
                               bParallel=bParallel)
  } else if (classType == CON_CLASS_TYPE$SVM_TWO_CLASS)
  {
    y <- data$HAE
    X <- data[, colnames(data) != "HAE"]
    result <- Model_2Class_SVM(y=y, X=X, 
                               kEvalFolds=kEvalFolds, kValiFolds=kValiFolds, 
                               Cs=Cs, gammas=gammas,
                               kernel=kernel,
                               bParallel=bParallel)
  } else if (classType == CON_CLASS_TYPE$LR_LASSO)
  {
    y <- data$HAE
    X <- data.matrix(data[, colnames(data) != "HAE"])
    result <- Model_LR_LASSO(y=y, X=X, 
                             kEvalFolds=kEvalFolds, kValiFolds=kValiFolds, 
                             lambdas=lambdas,
                             bParallel=bParallel)
  } else
  {
    stop("Error! Invalid classType! ")
  }
  
  
  SaveResult(result, resultDir)
  
  print(paste("accuracyPos: ", result$accuracyPos))
  print(paste("accuracyNeg: ", result$accuracyNeg))
  
  runtime <- proc.time() - ptm
  print(paste("Time elapsed:", round(runtime[3],1), "seconds."))
  fileRunTime <- file(paste(resultDir, "runtime.txt", sep=""), "w")
  writeLines(paste(round(runtime["elapsed"],1), " seconds.", "\n", sep=""), fileRunTime)
  close(fileRunTime)
  
  if (bParallel)
  {
    stopCluster(cl)
  }
}