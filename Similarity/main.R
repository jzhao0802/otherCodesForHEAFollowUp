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

source("F:\\Jie\\Shire_follow_up\\02_Code\\Similarity\\funs_similarity.R")

n.simu <- 5
n.folds <- 5

run_lasso_S(resultDir="F:\\Jie\\Shire_follow_up\\03_Results\\for200+a\\"
            , dataDir="F:\\Jie\\Shire_follow_up\\03_Results\\for200+a\\"
            , norm=T
            , n.folds=5
            , wt=1/200
            , crit=seq(0.03, 0.07, 0.01)
            , Btest=F
            , crit_nm='aupr0.01_0.1'
            )


temp <- split_cv4F(dataDir='F:\\Jie\\Shire_follow_up\\03_Results\\for200+a\\'
                       , n.simu=5
                       , n.folds=5
                       , norm=T
                       , wt=1/200
                        , crit_nm='aupr0.01_0.1')
run_lasso_F(resultDir="F:\\Jie\\Shire_follow_up\\03_Results\\for200+a\\"
            , dataDir="F:\\Jie\\Shire_follow_up\\03_Results\\for200+a\\"
            , norm=T
            , n.folds=5
            , wt=1/200
            , crit=seq(0.03, 0.07, 0.01)
            , Btest=F
            , crit_nm='aupr0.01_0.1'
)

#summarize the performance on test
 
S_ms_on200K_avg_nm <- summarize_perf_200K(
                    resultDir="F:\\Jie\\Shire_follow_up\\03_Results\\for200+a\\"
                    , n.simu=5
                    , target_recall=seq(0.5, 0.05, -0.05)
                    , wt=1/200
                    , norm=T
                    , crit_nm='aupr0.01_0.1'
                    )

#Mar24 modify the threshold to select data for final model
S_reSelectData4F(resultDir="F:\\Jie\\Shire_follow_up\\03_Results\\for200+a\\"
                   , dataDir="F:\\Jie\\Shire_follow_up\\03_Results\\for200+a\\"
                   , norm=T
                   , n.simu=5
                   , n.folds=5
                   , wt=1/200
                   , crit=seq(0.03, 0.07, 0.01)
                   , Btest=F
                   , crit_nm='aupr0.01_0.1'
                   ,threshold = 0.3
                   )
temp <- split_cv4F(dataDir='F:\\Jie\\Shire_follow_up\\03_Results\\for200+a\\'
                   , n.simu=5
                   , n.folds=5
                   , norm=T
                   , wt=1/200
                   , crit_nm='aupr0.01_0.1')
run_lasso_F(resultDir="F:\\Jie\\Shire_follow_up\\03_Results\\for200+a\\"
            , dataDir="F:\\Jie\\Shire_follow_up\\03_Results\\for200+a\\"
            , norm=T
            , n.folds=5
            , wt=1/200
            , crit=seq(0.03, 0.07, 0.01)
            , Btest=F
            , crit_nm='aupr0.01_0.1'
)

