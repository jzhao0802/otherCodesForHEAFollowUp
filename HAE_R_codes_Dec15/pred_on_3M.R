#apply the trained model using traing data for Ddong's bagging forestto the 3M data set

#########################################################################
### Purpose:
#########################################################################

# Clear work space
rm(list=ls())
lasso_rf_iters = 20

# R work directory
wk_dir = "D:\\jzhao\\Shire_followup\\02_Code\\HAE_R_codes_Dec15\\"
# Data read and save path
data_path = "D:\\jzhao\\Shire_followup\\01_Data\\newdata_200K_3M\\"
# Plot save path
# plots_path1 = "D:\\jzhao\\Shire_followup\\03_Results\\for_new200K\\"
plots_path1 = "D:\\jzhao\\Shire_followup\\03_Results\\for_old200K\\"
plots_path <- paste0(plots_path1, 'iters=', lasso_rf_iters, '\\')
if(!dir.exists(plots_path)) dir.create(plots_path, showWarnings = T, recursive = TRUE)

#########################################################################
### Source functions + Load data
#########################################################################

# Setup R work directory
setwd(wk_dir)
# Load R packages
source("loadpackage.R")
# Auxiliary functions
source("auxfunctions.R")
# Load training/testing data: dat_hae_trn, dat_hae_tst, dat_nonhae_trn, dat_nonhae_tst
load(paste(plots_path1, "dat_hae_trn_tst_split_Mar31.RData", sep=""))


dat_nonhae_3M <- read.table("D:\\jzhao\\Shire_followup\\01_Data\\newdata_200K_3M\\nonhae_3M.csv"
                            , sep=','
                            , stringsAsFactors = F
                            , head=T
)
x_3M <- dat_nonhae_3M %>% mutate(LOOKBACK_DAYS=lookback_days) %>% select(-patient_id) %>% select(-lookback_days)

tst_label = y_tst
tst_prob_logit = rep(0, length(y_tst))
tst_prob_wgtlasso = rep(0, length(y_tst))
tst_prob_lasso = rep(0, length(y_tst))
tst_prob_rf = rep(0, length(y_tst))
tst_prob_rf_hae = rep(0, sum(dat_tst$HAE==1)) #218
Sys.time()->start

# Logistic regression
# tst_prob_logit = predict(trn_logit_fit, dat_tst[,-match('HAE', names(dat_tst))], type="response")

# Bagging LASSO, Bagging Random Forest
for (i in 1:lasso_rf_iters){
    # 	tst_prob_lasso = tst_prob_lasso + predict(trn_undbag_lasso_fit[[i]], as.matrix(x_tst), s="lambda.min", type="response")/lasso_rf_iters
    cat('i=', i, '!\n')
     tst_prob_rf = tst_prob_rf + predict(trn_undbag_rf_fit[[i]], x_3M, type = "prob")[,2]/lasso_rf_iters
    tst_prob_rf_hae = tst_prob_rf_hae + predict(trn_undbag_rf_fit[[i]], dat_hae_tst[,-match(c('HAE', 'PATIENT_ID'), names(dat_hae_tst))], type = "prob")[,2]/lasso_rf_iters
}

print(Sys.time()-start)
# Time difference of 12.26336 mins

save( tst_prob_rf_hae, tst_prob_rf, file=paste(plots_path, "tst_rf_prob_haeTs&3M.RData", sep=""))

resp <- c(rep(1, length(tst_prob_rf_hae)), rep(0, length(tst_prob_rf)))

pred <- c(tst_prob_rf_hae, tst_prob_rf)

source('D:\\jzhao\\Shire_followup\\02_Code\\Similarity\\funs_similarity.R')
library(ROCR)
perf_result <- msOnTest_sep_v2(pred, resp, recall_tar=seq(0.5, 0.05, -0.05))
write.csv(perf_result$ms, paste0(plots_path, 'perf_on3M.csv'))
