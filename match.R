#matching algrithm
library(dplyr)
library(plyr)
library(caret)
library(snowfall)
dataDir='F:\\Lichao\\work\\Projects\\HAE\\data\\from Dong\\'
result_dir= "F:\\Jie\\Shire_follow_up\\03_Results\\similarity\\"

dat_hae_1111_rf_nov26 <- 
    read.csv("F:/Lichao/work/Projects/HAE/data/Zhenxing/dat_hae_1111_rf_nov26_flag.csv", header=T, sep=",", check.names=F)
load(paste(dataDir, "dat_nonhae_1111_rf.RData", sep=""))

# by_days <- data.frame(ids=1:nrow(dat_nonhae_1111_rf), lookback_days=dat_nonhae_1111_rf[, "lookback_days"]) %>%
#     group_by(., lookback_days)

dat_neg <- data.frame(ids = 1:nrow(dat_nonhae_1111_rf), lookback_days=dat_nonhae_1111_rf$lookback_days)
dat_neg <- dat_neg[order(dat_neg$lookback_days),]

days_neg <- unique(dat_neg$lookback_days) #[1] 1391
neg_nested <- count(dat_neg$lookback_days)
names(neg_nested)[1] <- 'days' #[1] 1391    2

dat_pos <- data.frame(ids = 1:nrow(dat_hae_1111_rf_nov26), lookback_days=dat_hae_1111_rf_nov26$LOOKBACK_DAYS)
dat_pos <- dat_pos[order(dat_pos$lookback_days), ]
days_pos <- unique(dat_pos$lookback_days) #[1] 768

match <- function(i){
    d <- neg_nested[i, 1]
    d_lw=d-30
    d_up=d+30
    freq <- neg_nested[i, 2]
    
    posId <- c()
    posDay <- c()
    for(d_pos in days_pos){
        if(d_pos <= d_up & d_pos >= d_lw){
            posDay <- c(posDay, d_pos)
        }
    }
    if(length(posDay)>0){
        posId <- dat_pos$ids[dat_pos$lookback_days %in% posDay]
        posIds_forMatch <- sample(posId, size=freq, replace=T)
        
    }else{
        posIds_forMatch <- NA
    }
    temp <- list(neg_days=d, posIds=posId, posIds_forMatch=posIds_forMatch)
    return(temp)
}

sfInit(parallel=TRUE, cpus=39, type='SOCK')
#sfSource("F:\\Jie\\Shire\\03_code\\subFunc_v3.R")
#sfSource("F:\\Jie\\Shire_follow_up\\02_Code\\clean_split_featureSelection_lasso.R")
sfExport('neg_nested', 'days_pos', 'dat_pos')
sfExport('match')
#sfClusterEval(library("glmnet"))
t0 <- proc.time()
result <- sfClusterApplyLB(1:nrow(neg_nested), match)
sfStop()
cat('time used:', (proc.time()-t0)[3]/60, 'min!\n')

neg_days <- unlist(lapply(result, function(x)x[[1]]))
pos_days_list <- lapply(result, function(x)x[[2]])
posIds_forMatch <- lapply(result, function(x)x[[3]])
bUnmatched <- unlist(lapply(posIds_forMatch, function(x){
    any(is.na(x))
}))
idx_unmatched <- which(bUnmatched==T)
neg_days_unmatched <- neg_days[idx_unmatched]
posIds_forMatch_vct <- unlist(posIds_forMatch)
posIds_forMatched <- posIds_forMatch_vct[!is.na(posIds_forMatch_vct)] #[1] 246550

# dat_neg_matched <- dat_neg %>%
#     filter(., lookback_days %in% neg_days_unmatched)
dat_neg_matched <- dat_neg[!(dat_neg$lookback_days %in% neg_days_unmatched), ] #[1] 246550      2
dat_neg_unmatched <- dat_neg[dat_neg$lookback_days %in% neg_days_unmatched, ]

dat_neg$posIdMatched <- c(rep(NA, nrow(dat_neg)-length(posIds_forMatched)), posIds_forMatched)
posIdMatched <- c(rep(NA, nrow(dat_neg)-length(posIds_forMatched)), posIds_forMatched)
save(posIdMatched, file=paste0(result_dir, 'posIdMatched.RData'))
