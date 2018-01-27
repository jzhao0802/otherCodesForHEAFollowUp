library(foreach)
library(doParallel)
library(e1071)

source("functions/SplitData.R")

CON_CV_CRITERIA <- list(MAX_ACCURACY=1,
                        SPEC_ACCURACY=2)

CrossValidate_1Class_SVM <- function(X, kValiFolds, nus, gammas, kernel,
                                     cvCriterion, specAccuracy, bParallel)
{
  nData <- nrow(X)
  valiFolds <- ManualSampleFolds(nData, kValiFolds)
  
  nNus <- length(nus)
  if (kernel == "radial")
  {
    nGammas <- length(gammas)
    nParamSets <- nNus * nGammas
  } else
    nParamSets <- nNus
  
  if (bParallel)
  {
    accuraciesAllParamSets <- 
      foreach(iParamSet=(1:nParamSets), .combine="c", 
              .packages=c("e1071")) %dopar%
      {
        if (kernel == "radial")
        {
          iNu <- ceiling(iParamSet / nGammas)
          iGamma <- iParamSet %% nGammas
          if (iGamma == 0)
            iGamma <- nGammas
          gamma <- gammas[iGamma]
        } else if (kernel == "linear")
          iNu <- iParamSet
        
        nu <- nus[iNu]
        
        predsAllData <- rep(1e5, nrow(X))
        
        for (iFold in 1:kValiFolds)
        {
          trainIDs <- valiFolds[[iFold]]
          XTrain <- X[trainIDs,]
          XVali <- X[-trainIDs,]
          
          if (kernel == "radial")
          {
            model <- svm(XTrain, type="one-classification", kernel=kernel,
                         gamma=gamma, nu=nu, probability=F)
          } else if (kernel == "linear")
          {
            model <- svm(XTrain, type="one-classification", kernel=kernel,
                         nu=nu, probability=F)
          }
          
          predsAllData[-trainIDs] <- predict(model, XVali)
        }
        
        accuracy <- sum(predsAllData > 0) / length(predsAllData)
      }
  } else 
  {
    accuraciesAllParamSets <- 
      foreach(iParamSet=(1:nParamSets), .combine="c", 
              .packages=c("e1071")) %do%
              {
                if (kernel == "radial")
                {
                  iNu <- ceiling(iParamSet / nGammas)
                  iGamma <- iParamSet %% nGammas
                  if (iGamma == 0)
                    iGamma <- nGammas
                  gamma <- gammas[iGamma]
                } else if (kernel == "linear")
                  iNu <- iParamSet
                
                nu <- nus[iNu]
                
                predsAllData <- rep(1e5, nrow(X))
                
                for (iFold in 1:kValiFolds)
                {
                  trainIDs <- valiFolds[[iFold]]
                  XTrain <- X[trainIDs,]
                  XVali <- X[-trainIDs,]
                  
                  if (kernel == "radial")
                  {
                    model <- svm(XTrain, type="one-classification", kernel=kernel,
                                 gamma=gamma, nu=nu, probability=F)
                  } else if (kernel == "linear")
                  {
                    model <- svm(XTrain, type="one-classification", kernel=kernel,
                                 nu=nu, probability=F)
                  }
                  
                  predsAllData[-trainIDs] <- predict(model, XVali)
                }
                
                accuracy <- sum(predsAllData > 0) / length(predsAllData)
              }
  }
  
  if (cvCriterion == CON_CV_CRITERIA$MAX_ACCURACY)
  {
    bestAccuracy <- max(accuraciesAllParamSets)
    bestParamSetID <- which.max(accuraciesAllParamSets)
  } else if (cvCriterion == CON_CV_CRITERIA$SPEC_ACCURACY)
  {
    diff2Spec <- abs(accuraciesAllParamSets-specAccuracy)
    bestParamSetID <- which.min(diff2Spec)
    bestAccuracy <- accuraciesAllParamSets[bestParamSetID]
  }
  else
  {
    stop("Error! Invalid cvCriterion value.")
  }
  
  
  if (kernel == "radial")
  {
    bestNuID <- ceiling(bestParamSetID / nGammas)
    bestNu <- nus[bestNuID]
    bestGammaID <- bestParamSetID %% nGammas
    if (bestGammaID == 0)
      bestGammaID <- nGammas
    bestGamma <- gammas[bestGammaID]
    
    return (list(bestAccuracy=bestAccuracy, 
                 bestNu=bestNu,
                 bestGamma=bestGamma))
  } else if (kernel == "linear")
  {
    bestNuID <- bestParamSetID
    bestNu <- nus[bestNuID]
    return (list(bestAccuracy=bestAccuracy, 
                 bestNu=bestNu))
  }
}

Model_1Class_SVM <- function(XModel, XNew, yNew,
                             kEvalFolds, kValiFolds, nus, gammas, 
                             cvCriterion, specAccuracy,
                             kernel,
                             bParallel)
{
  if (nrow(XNew) != length(yNew))
    stop("Error! The sizes of XNew and yNew are incompatible. " )
  
  if ((kernel == "radial") & (is.null(gammas)))
    stop("Error! A radial kernel requires at least one gamma value.")
  
  nData <- nrow(XModel)
  
  # divide into evaluation folds
  evalFolds <- ManualSampleFolds(nData, kEvalFolds)
  
  predsAllData <- rep(1e5,nrow(XModel))
  if (kernel == "radial")
  {
    bestParamsAllEvalFolds <- matrix(0, nrow=2, ncol=kEvalFolds)
    rownames(bestParamsAllEvalFolds) <- c("nu", "gamma")
  } else if (kernel == "linear")
  {
    bestParamsAllEvalFolds <- matrix(0, nrow=1, ncol=kEvalFolds)
    rownames(bestParamsAllEvalFolds) <- c("nu")
  }
  else
  {
    stop("Error! Invalid kernel.")
  }
    
  colnames(bestParamsAllEvalFolds) <- paste("Fold", 1:kEvalFolds)
  
  
  # model for every fold
  
  for (iFold in 1:kEvalFolds)
  {
    trainValiIDs <- evalFolds[[iFold]]
    XTrainVali <- XModel[trainValiIDs, ]
    XEval <- XModel[-trainValiIDs, ]
    
    cvResult <- CrossValidate_1Class_SVM(X=XTrainVali, 
                                         kValiFolds=kValiFolds, 
                                         nus=nus, gammas=gammas, 
                                         kernel=kernel,
                                         cvCriterion=cvCriterion, 
                                         specAccuracy=specAccuracy, 
                                         bParallel=bParallel)
    # train with the best params 
    if (kernel == "radial")
    {
      model <- svm(XTrainVali, type="one-classification", kernel=kernel,
                   gamma=cvResult$bestGamma, nu=cvResult$bestNu,
                   probability=F)
    } else if (kernel == "linear")
    {
      model <- svm(XTrainVali, type="one-classification", kernel=kernel,
                   nu=cvResult$bestNu,
                   probability=F)
    }
   
    
    # evaluation
    preds <- predict(model, XEval, decision.values=T)
    predsAllData[-trainValiIDs] <- attr(preds, "decision.values")
    
    bestParamsAllEvalFolds["nu", iFold] <- cvResult$bestNu
    if (kernel == "radial")
      bestParamsAllEvalFolds["gamma", iFold] <- cvResult$bestGamma
  }
  
  # the aggregated evaluation result
  
  accuracyCrossEval <- sum(predsAllData > 0) / length(predsAllData)
  
  # test it on complete new data
  
  finalNu <- mean(bestParamsAllEvalFolds["nu",])
  
  if (kernel == "radial")
  {
    finalGamma <- mean(bestParamsAllEvalFolds["gamma",])
    modelFinal <- svm(XModel, type="one-classification", kernel=kernel,
                      gamma=finalGamma, nu=finalNu,
                      probability=F)
  } else if (kernel == "linear")
  {
    modelFinal <- svm(XModel, type="one-classification", kernel=kernel,
                      nu=finalNu,
                      probability=F)
  }
  
  predsNewStruct <- predict(modelFinal, XNew, decision.values=T)
  decision.values_New <- attr(predsNewStruct, "decision.values")
  predsNew <- (decision.values_New > 0)
  accuracyNew <- sum(predsNew == yNew) / length(yNew)
  
  
  
  result <- list(accuracyPos=accuracyCrossEval,
                 accuracyNeg=accuracyNew,
                 predsPos=predsAllData,
                 predsNeg=decision.values_New,
                 bestParamsAllEvalFolds=bestParamsAllEvalFolds,
                 modelForFuture=modelFinal, 
                 modelType="SVM_ONE_CLASS",
                 additionalInfo=NULL)
  
  return (result)
  
}