rm(list=ls())

source("functions/Model.R")

main.arglist <- list()
main.arglist$dataDir <- "C:/Work/Projects/HAE/data/from Dong/"

main.arglist$kEvalFolds <- 2
main.arglist$kValiFolds <- 3

main.arglist$bCleanData <- T

main.arglist$classType <- CON_CLASS_TYPE$LR_LASSO

# hyper-parameter grid

# main.arglist$nus <- seq(from=0.01,to=0.9, by=0.4)
# main.arglist$logGammas <- seq(from=-4, to=0, by=2)
# main.arglist$logCs <- seq(from=-4,to=0, by=1)
main.arglist$logLambdas <- seq(from=-4, to=3, by=0.5)

# main.arglist$kernel <- "linear"
# 
# main.arglist$cvCriterion <- CON_CV_CRITERIA$SPEC_ACCURACY
# main.arglist$specAccuracy <- 0.8

main.arglist$bParallel <- F

Model(main.arglist)