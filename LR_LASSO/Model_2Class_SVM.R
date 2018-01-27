library(foreach)
library(doParallel)
library(pROC)
library(e1071)

source("functions/manualStratify.R")

Swap2MakeFirstPositive <- function(yTrain, trainIDs)
{
  firstPosLoc <- (which(yTrain==1))[1]
  tmp <- trainIDs[1]
  trainIDs[1] <- trainIDs[firstPosLoc]
  trainIDs[firstPosLoc] <- tmp
  
  return (trainIDs)
}

CrossValidate_2Class_SVM <- function(y, X, kValiFolds, Cs, gammas, 
                                     kernel,
                                     bParallel)
{
  nData <- nrow(X)
  valiFolds <- stratifySmallSample(y, kValiFolds)
  
  nCs <- length(Cs)
  if (kernel == "radial")
  {
    nGammas <- length(gammas)
    nParamSets <- nCs * nGammas
  } else
    nParamSets <- nCs
  
  if (bParallel)
  {
    AUCsAllParamSets <- 
      foreach(iParamSet=(1:nParamSets), .combine="c", 
              .packages=c("e1071", "pROC")) %dopar%
              {
                if (kernel == "radial")
                {
                  iC <- ceiling(iParamSet / nGammas)
                  iGamma <- iParamSet %% nGammas
                  if (iGamma == 0)
                    iGamma <- nGammas
                  gamma <- gammas[iGamma]
                } else if (kernel == "linear")
                  iC <- iParamSet
                C <- Cs[iC]
                
                predsAllData <- rep(1e5, nrow(X))
                
                for (iFold in 1:kValiFolds)
                {
                  trainIDs <- valiFolds[[iFold]]
                  yTrain <- y[trainIDs]
                  
                  # swap to make sure the first training datum is labelled 1
                  if (yTrain[1] != 1)
                  {
                    trainIDs <- Swap2MakeFirstPositive(yTrain, trainIDs)
                  }
                  
                  XTrain <- X[trainIDs,]
                  XVali <- X[-trainIDs,]
                  yTrain <- y[trainIDs]
                  yVali <- y[-trainIDs]
                  
                  # 
                  if (kernel == "radial")
                  {
                    model <- svm(y=yTrain, x=XTrain, 
                                 type="C-classification", kernel=kernel,
                                 gamma=gamma, cost=C,
                                 class.weights=1/table(yTrain), probability=F)
                  } else if (kernel == "linear")
                  {
                    model <- svm(y=yTrain, x=XTrain, 
                                 type="C-classification", kernel=kernel,
                                 cost=C,
                                 class.weights=1/table(yTrain), probability=F)
                  }
                  
                  
                  rawPreds <- 
                    predict(model, XVali, decision.values=T)
                  predsAllData[-trainIDs] <- 
                    attr(rawPreds, "decision.values")
                  
#                   if (yTrain[1] == 0)
#                     predsAllData[-trainIDs] <- -predsAllData[-trainIDs]
                   
                }
                
                roc_obj <- 
                  roc(response=as.vector(y), 
                      predictor=as.vector(predsAllData),
                      direction="<")
                auc <- roc_obj$auc
              }
  } else 
  {
    AUCsAllParamSets <- 
      foreach(iParamSet=(1:nParamSets), .combine="c", 
              .packages=c("e1071", "pROC")) %do%
              {
                if (kernel == "radial")
                {
                  iC <- ceiling(iParamSet / nGammas)
                  iGamma <- iParamSet %% nGammas
                  if (iGamma == 0)
                    iGamma <- nGammas
                  gamma <- gammas[iGamma]
                } else if (kernel == "linear")
                  iC <- iParamSet
                C <- Cs[iC]
                
                predsAllData <- rep(1e5, nrow(X))
                
                for (iFold in 1:kValiFolds)
                {
                  trainIDs <- valiFolds[[iFold]]
                  yTrain <- y[trainIDs]
                  
                  # swap to make sure the first training datum is labelled 1
                  if (yTrain[1] != 1)
                  {
                    trainIDs <- Swap2MakeFirstPositive(yTrain, trainIDs)
                  }
                  
                  XTrain <- X[trainIDs,]
                  XVali <- X[-trainIDs,]
                  yTrain <- y[trainIDs]
                  yVali <- y[-trainIDs]
                  
                  # 
                  if (kernel == "radial")
                  {
                    model <- svm(y=yTrain, x=XTrain, 
                                 type="C-classification", kernel=kernel,
                                 gamma=gamma, cost=C,
                                 class.weights=1/table(yTrain), probability=F)
                  } else if (kernel == "linear")
                  {
                    model <- svm(y=yTrain, x=XTrain, 
                                 type="C-classification", kernel=kernel,
                                 cost=C,
                                 class.weights=1/table(yTrain), probability=F)
                  }
                  
                  
                  rawPreds <- 
                    predict(model, XVali, decision.values=T)
                  predsAllData[-trainIDs] <- 
                    attr(rawPreds, "decision.values")
                  
                  #                   if (yTrain[1] == 0)
                  #                     predsAllData[-trainIDs] <- -predsAllData[-trainIDs]
                  
                }
                
                roc_obj <- 
                  roc(response=as.vector(y), 
                      predictor=as.vector(predsAllData),
                      direction="<")
                auc <- roc_obj$auc
              }
  }
  
  bestAUC <- max(AUCsAllParamSets)
  bestParamSetID <- which.max(AUCsAllParamSets)
  
  if (kernel == "radial")
  {
    bestCID <- ceiling(bestParamSetID / nGammas)
    bestGammaID <- bestParamSetID %% nGammas
    if (bestGammaID == 0)
      bestGammaID <- nGammas
    bestC <- Cs[bestCID]
    bestGamma <- gammas[bestGammaID]
    print(paste("bestAUC:", bestAUC))
    return (list(bestAUC=bestAUC, 
                 bestC=bestC,
                 bestGamma=bestGamma))
  } else if (kernel == "linear")
  {
    bestCID <- bestParamSetID
    bestC <- Cs[bestCID]
    print(paste("bestAUC:", bestAUC))
    return (list(bestAUC=bestAUC, 
                 bestC=bestC))
  }
}

Model_2Class_SVM <- function(y, X, 
                             kEvalFolds, kValiFolds, Cs, gammas, 
                             kernel,
                             bParallel)
{
  if (nrow(X) != length(y))
    stop("Error! The sizes of X and y are incompatible. " )
  
  if ((kernel == "radial") & (is.null(gammas)))
    stop("Error! A radial kernel requires at least one gamma value.")
  
  nData <- nrow(X)
  
  # divide into evaluation folds
  evalFolds <- stratifySmallSample(y, kEvalFolds)
  
  predsAllData <- rep(1e5,length(y))
  if (kernel == "radial")
  {
    bestParamsAllEvalFolds <- matrix(0, nrow=2, ncol=kEvalFolds)
    rownames(bestParamsAllEvalFolds) <- c("C", "gamma")
  } else if (kernel == "linear")
  {
    bestParamsAllEvalFolds <- matrix(0, nrow=1, ncol=kEvalFolds)
    rownames(bestParamsAllEvalFolds) <- c("C")
  }
  else
  {
    stop("Error! Invalid kernel.")
  }
  
  # model for every fold
  
  for (iFold in 1:kEvalFolds)
  {
    trainValiIDs <- evalFolds[[iFold]]
    yTrainVali <- y[trainValiIDs]
    
    # swap to make sure the first training datum is labelled 1
    if (yTrainVali[1] != 1)
      trainValiIDs <- Swap2MakeFirstPositive(yTrainVali, trainValiIDs)
    
    
    XTrainVali <- X[trainValiIDs, ]
    XEval <- X[-trainValiIDs, ]
    yTrainVali <- y[trainValiIDs]
    yEval <- y[-trainValiIDs]
    
    cvResult <- CrossValidate_2Class_SVM(y=yTrainVali, X=XTrainVali, 
                                         kValiFolds=kValiFolds, 
                                         Cs=Cs, gammas=gammas, kernel=kernel,
                                         bParallel=bParallel)
    # train with the best params 
    if (kernel == "radial")
    {
      model <- svm(y=yTrainVali, x=XTrainVali, type="C-classification", 
                   kernel=kernel,
                   gamma=cvResult$bestGamma, cost=cvResult$bestC,
                   class.weights=1/table(yTrainVali),
                   probability=T)
    } else if (kernel == "linear")
    {
      model <- svm(y=yTrainVali, x=XTrainVali, type="C-classification", 
                   kernel=kernel,
                   cost=cvResult$bestC,
                   class.weights=1/table(yTrainVali),
                   probability=T)
    } 
    
    
    # evaluation
    preds <- predict(model, XEval, decision.values=T)
    predsAllData[-trainValiIDs] <- attr(preds, "decision.values")
#     if (yTrainVali[1] == 0)
#       predsAllData[evalIDs] <- -predsAllData[evalIDs]
    
    bestParamsAllEvalFolds["C", iFold] <- cvResult$bestC
    if (kernel == "radial")
      bestParamsAllEvalFolds["gamma", iFold] <- cvResult$bestGamma
  }
  
  # the aggregated evaluation result
  
  accuracyPos <- sum((predsAllData > 0) & (y == 1)) / length(predsAllData)
  accuracyNeg <- sum((predsAllData < 0) & (y == 0)) / length(predsAllData)
  
  # build a model for future use
  
  # swap to make sure the first training datum is labelled 1
  if (y[1] != 1)
    IDs <- Swap2MakeFirstPositive(y, 1:length(y))
  else
    IDs <- 1:length(y)
  
  finalC <- mean(bestParamsAllEvalFolds["C",])
  if (kernel == "radial")
  {
    finalGamma <- mean(bestParamsAllEvalFolds["gamma",])
    modelForFuture <- 
      svm(y=y[IDs], x=X[IDs,], type="C-classification", kernel=kernel,
          gamma=finalGamma, cost=finalC,
          class.weights=1/table(y),
          probability=F)
  } else if (kernel == "linear")
  {
    modelForFuture <- 
      svm(y=y[IDs], x=X[IDs,], type="C-classification", kernel=kernel,
          cost=finalC,
          class.weights=1/table(y),
          probability=F)
  }
  
  # 
  
  result <- list(accuracyPos=accuracyPos,
                 accuracyNeg=accuracyNeg,
                 predsPos=predsAllData[y==1],
                 predsNeg=predsAllData[y==0],
                 bestParamsAllEvalFolds=bestParamsAllEvalFolds,
                 modelForFuture=modelForFuture,
                 modelType="SVM_TWO_CLASS",
                 additionalInfo=NULL)
  
  return (result)
  
}