rm(list=ls())
library(xlsx)
library(ROCR)
library(plyr)
library(caret)
library(dplyr)
library(glmnet)
library(snow)
library(snowfall)
library(caTools)

source("F:\\Jie\\Shire_follow_up\\02_Code\\clean_split_featureSelection_lasso.R")

n.simu <- 5
n.folds <- 5
#step1:  b.	All other steps in my cleaning and feature selection code need to be performed
#outside the evaluation / simulation loop (i.e., on the whole ~1K positives + ~200K negatives).
arglist <- list(bTestMode=F
                , dataDir='F:\\Lichao\\work\\Projects\\HAE\\data\\from Dong\\'
                , result_dir= "F:\\Jie\\Shire_follow_up\\03_Results\\for200+a\\"
)
Clean(arglist)
#[1] 216392     86


#step2: split 5 simulation
arglist <- list(dataDir='F:\\Jie\\Shire_follow_up\\03_Results\\'
                , resultDir="F:\\Jie\\Shire_follow_up\\03_Results\\" 
                , n.simu=5
)

split(arglist=arglist)

#step3: for each simulation on training+validation do   a.	Cleanings / feature selection to perform inside the evaluation / simulation loop (i.e., the initial processing need to be performed on the training + validation data, then for the test data they need to be processed according to the conclusion from the training + validation data): 

for(simu in 1:n.simu){
  clean2(dataDir="F:\\Jie\\Shire_follow_up\\03_Results\\for200+a\\" 
         , resultDir="F:\\Jie\\Shire_follow_up\\03_Results\\for200+a\\" 
         , i=simu)
}



#step4: normalize splited data after clean2 and save the min and range matrics for later ussage on corresponding test data
#scale_metrics <<- list()

for(simu in 1:n.simu){
  normlize(dataDir='F:\\Jie\\Shire_follow_up\\03_Results\\for200+a\\'
           , resultDir="F:\\Jie\\Shire_follow_up\\03_Results\\for200+a\\" 
           , i=simu)
  
}

#step5: split data into 5 folds for cv

split_cv(dataDir='F:\\Jie\\Shire_follow_up\\03_Results\\for200+a\\'
         , resultDir="F:\\Jie\\Shire_follow_up\\03_Results\\for200+a\\" 
         , n.simu=5
         , n.folds=5
         , norm=T)
split_cv(dataDir='F:\\Jie\\Shire_follow_up\\03_Results\\for200+a\\'
         , resultDir="F:\\Jie\\Shire_follow_up\\03_Results\\for200+a\\" 
         , n.simu=5
         , n.folds=5
         , norm=F)

#run the main function for cv lasso on simulations
run_lasso(dataDir='F:\\Jie\\Shire_follow_up\\03_Results\\for200+a\\'
          , resultDir="F:\\Jie\\Shire_follow_up\\03_Results\\for200+a\\" 
          , norm=T
          , n.folds=5
          , wt=0.005
          , crit=seq(0.03, 0.07, 0.01)
          , Btest=F
          , crit_nm='aupr0.01_0.1')

#summarize the performance on test
target_recall <- seq(0.5, 0.05, -0.05)
ms_on200K_avg_nn <- summarize_perf(resultDir="F:\\Jie\\Shire_follow_up\\03_Results\\for200+a\\" 
                                   , n.simu=5
                                   , target_recall = target_recall
                                   , wt=0.005
                                   , norm=F
                                   , crit_nm='auc')
ms_on200K_avg_nm <- summarize_perf(resultDir="F:\\Jie\\Shire_follow_up\\03_Results\\for200+a\\" 
                                   , n.simu=5
                                   , target_recall = target_recall
                                   , wt=0.005
                                   , norm=T
                                   , crit_nm='aupr0.01_0.1')

ms_on_avg_aupr <- cbind(withoutNorm=ms_on200K_avg_nn, withNorm=ms_on200K_avg_nm)

resultDir="F:\\Jie\\Shire_follow_up\\03_Results\\for200+a\\" 
wt <- 1/200
write.csv(ms_on_avg_aupr, paste0(resultDir, 'ms_avg_wt=', wt, '_aupr.csv'))

