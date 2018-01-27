library(glmnet)

source("functions/manualStratify.R")
source("functions/computeWeights.R")

CrossValidate_LR_LASSO <- function(y, X, 
                                   kValiFolds, lambdas, weightVec,
                                   bParallel)
{
  foldIDs <- stratifyFoldIDs(y, kValiFolds)
  cv.fit <- cv.glmnet(x=X, y=y, family="binomial",
                      type.measure="auc", alpha=1,
                      weights=weightVec, foldid=foldIDs,
                      lambda=lambdas, parallel=bParallel)
  return (cv.fit$lambda.min)
}

Model_LR_LASSO <- function(y, X, 
                           kEvalFolds, kValiFolds, 
                           lambdas, bParallel)
{
  nData <- nrow(X)
  
  # divide into evaluation folds
  evalFolds <- stratifySmallSample(y, kEvalFolds)
  
  predsAllData <- rep(1e5,length(y))
  
  bestParamsAllEvalFolds <- matrix(0, nrow=1, ncol=kEvalFolds)
  rownames(bestParamsAllEvalFolds) <- c("lambda")
  
  # model for every fold
  
  for (iFold in 1:kEvalFolds)
  {
    trainValiIDs <- evalFolds[[iFold]]
    XTrainVali <- X[trainValiIDs, ]
    XEval <- X[-trainValiIDs, ]
    yTrainVali <- y[trainValiIDs]
    yEval <- y[-trainValiIDs]
    
    weightVec <- computeWeights(yTrainVali)
    
    bestLambda <- CrossValidate_LR_LASSO(y=yTrainVali, X=XTrainVali, 
                                         kValiFolds=kValiFolds, 
                                         lambdas=lambdas,
                                         weightVec=weightVec,
                                         bParallel=bParallel)
    # train with the best params 
    model <- glmnet(XTrainVali,yTrainVali, family="binomial", 
                    weights=weightVec,
                    alpha=1, lambda=lambdas)
    
    
    # evaluation
    predsAllData[-trainValiIDs] <- 
      predict(model, newx=XEval, type="response", s=bestLambda)
    
    bestParamsAllEvalFolds["lambda", iFold] <- bestLambda
  }
  
  # the aggregated evaluation result
  
  accuracyPos <- sum((predsAllData > 0) & (y == 1)) / length(predsAllData)
  accuracyNeg <- sum((predsAllData < 0) & (y == 0)) / length(predsAllData)
  
  # build a model for future use
  
  finalLambda <- mean(bestParamsAllEvalFolds["lambda",])
  modelForFuture <- 
    glmnet(X, y, family="binomial", 
           weights=computeWeights(y),
           alpha=1, lambda=lambdas)
  # 
  
  result <- list(accuracyPos=accuracyPos,
                 accuracyNeg=accuracyNeg,
                 predsPos=predsAllData[y==1],
                 predsNeg=predsAllData[y==0],
                 bestParamsAllEvalFolds=bestParamsAllEvalFolds,
                 modelForFuture=modelForFuture, 
                 modelType="LR_LASSO",
                 additionalInfo=list(bestLambda=finalLambda))
  
  return (result)
}