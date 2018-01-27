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
# 
dat_hae_1111_rf_nov26 <- 
    read.csv("D:\\jzhao\\Shire_followup\\01_Data\\dat_hae_1111_rf_nov26_flag.csv"
             , header=T, sep=",", check.names=F)
# load(paste(dataDir, "dat_nonhae_1111_rf.RData", sep=""))
load("D:\\jzhao\\Shire_followup\\01_Data\\HAE1087_ptid.RData")
# dat_nonhae_200K <- read.table("D:\\jzhao\\Shire_followup\\01_Data\\newdata_200K_3M\\nonhae_200K.csv"
#                             , sep=','
#                             , stringsAsFactors = F
#                             , head=T
# )
load("D:\\jzhao\\Shire_followup\\01_Data\\dat_nonhae_1111_rf.RData") #dat_nonhae_1111_rf
hae <- dat_hae_1111_rf_nov26 %>% filter(FLAG_==0) %>% select(-c(REGION, FLAG_))
# %>%
# {
#     .$GENDERM=ifelse(.$GENDER=='M', 1, 0)
#     .
# } %>% select(-GENDER)
hae <- hae[hae$PATIENT_ID %in% HAE1087_ptid, ]#1087
nonhae <- dat_nonhae_1111_rf %>% mutate(LOOKBACK_DAYS=lookback_days) %>% select(-lookback_days) %>% select(-REGION) %>% mutate(hae_patient_id=HAE_PATIENT) %>% select(-HAE_PATIENT)
nonhae$hae_patient_id <- as.numeric(nonhae$hae_patient_id)
# sample parts of nonhae according to the times which is the repeated times of nonhae to hae
# times <- 3
# nonhae_sample <- nonhae %>% 
#     group_by(hae_patient_id) %>%
#     sample_n(times)
# nonhae <- as.data.frame(nonhae_sample)


set.seed(20)
tr_idx <- createFolds(hae$PATIENT_ID, 5, returnTrain=T)[[1]]
dat_hae_trn <- hae[tr_idx, ]
dat_hae_tst <- hae[-tr_idx, ]

dat_nonhae_trn <- nonhae[nonhae$hae_patient_id %in% dat_hae_trn$PATIENT_ID,]
# dat_nonhae_tst <- nonhae[!(nonhae$hae_patient_id %in% dat_hae_trn$PATIENT_ID),]
dat_nonhae_tst <- nonhae[nonhae$hae_patient_id %in% dat_hae_tst$PATIENT_ID, ]
dat_nonhae_trn$HAE <- 0
dat_nonhae_tst$HAE <- 0

save(dat_hae_trn, dat_hae_tst, dat_nonhae_trn , dat_nonhae_tst
     , file=paste(plots_path, "dat_hae_trn_tst_split_Mar31.RData", sep=""))

# Load training/testing data: dat_hae_trn, dat_hae_tst, dat_nonhae_trn, dat_nonhae_tst
load(paste(plots_path1, "dat_hae_trn_tst_split_Mar31.RData", sep=""))

x_hae_trn = dat_hae_trn[,-match(c('PATIENT_ID', 'HAE'), names(dat_hae_trn))]
y_hae_trn = dat_hae_trn[,match('HAE', names(dat_hae_trn))]
x_hae_tst = dat_hae_tst[,-match(c('PATIENT_ID', 'HAE'), names(dat_hae_tst))]
y_hae_tst = dat_hae_tst[,match('HAE', names(dat_hae_tst))]


x_nonhae_trn = dat_nonhae_trn[,-match(grep('patient_id|hae', names(dat_nonhae_trn), valu=T, perl=T, ignore.case = T)
                                      , names(dat_nonhae_trn))]
y_nonhae_trn = dat_nonhae_trn[,match('HAE', names(dat_nonhae_trn))]
x_nonhae_tst = dat_nonhae_tst[,-match(grep('patient_id|hae', names(dat_nonhae_tst), valu=T, perl=T, ignore.case = T)
                                      , names(dat_nonhae_tst))]
y_nonhae_tst = dat_nonhae_tst[,match('HAE', names(dat_nonhae_tst))]

x_nonhae_tst <- x_nonhae_tst[, match(names(x_hae_tst), names(x_nonhae_tst))]
x_nonhae_trn <- x_nonhae_trn[, match(names(x_hae_trn), names(x_nonhae_trn))]
x_tst = rbind(x_hae_tst, x_nonhae_tst)
y_tst = c(y_hae_tst, y_nonhae_tst)
dat_tst = data.frame(y_tst, x_tst)
names(dat_tst)[1]='HAE'

x_trn = rbind(x_hae_trn, x_nonhae_trn)
y_trn = c(y_hae_trn, y_nonhae_trn)
dat_trn = data.frame(y_trn, x_trn)
names(dat_trn)[1]='HAE'


#########################################################################
### Model training (by 5-fold CV)
#########################################################################

n_hae_trn = dim(dat_hae_trn)[1]
n_nonhae_trn = dim(dat_nonhae_trn)[1]

kfold_out = 5
set.seed(20)
flds_out_hae = createFolds(1:n_hae_trn, kfold_out)
idx_hae_out = data.frame(id=1:n_hae_trn, flds_out=0)
for (m in 1:kfold_out){idx_hae_out$flds_out[flds_out_hae[[m]]]=m}

flds_out_nonhae = list()
for (m in 1:kfold_out){
    flds_out_nonhae[[m]] = which(dat_nonhae_trn$hae_patient_id %in% dat_hae_trn$PATIENT_ID[flds_out_hae[[m]]])
}
idx_nonhae_out = data.frame(id=1:n_nonhae_trn, flds_out=0)
for (m in 1:kfold_out){idx_nonhae_out$flds_out[flds_out_nonhae[[m]]]=m}

# random forest formula
rf.formula=as.simple.formula(names(dat_trn)[which(names(dat_trn)!='HAE')], 'HAE')

Sys.time()->start

cv_label = list()
# cv_prob_lasso = list()
cv_prob_rf = list()
# cv_prob_logit = list()

for (m in 1:kfold_out){
    
    print(paste(m,"/",kfold_out,sep=""))
    start1 <- Sys.time()
    
    idx_hae_val = which(idx_hae_out$flds_out==m)
    dat_hae_val = dat_hae_trn[idx_hae_val,]
    dat_hae_subtrn = dat_hae_trn[-idx_hae_val,]
    
    idx_nonhae_val = which(idx_nonhae_out$flds_out==m)
    dat_nonhae_val = dat_nonhae_trn[idx_nonhae_val,]
    dat_nonhae_subtrn = dat_nonhae_trn[-idx_nonhae_val,]
    
    x_hae_subtrn = dat_hae_subtrn[,-match(c('PATIENT_ID', 'HAE'), names(dat_hae_subtrn))]
    y_hae_subtrn = dat_hae_subtrn$HAE
    x_nonhae_subtrn = dat_nonhae_subtrn[,-match(grep('patient_id|hae', names(dat_nonhae_subtrn), valu=T, perl=T, ignore.case = T)
                                                , names(dat_nonhae_subtrn))]
    y_nonhae_subtrn = dat_nonhae_subtrn$HAE
    
    x_subtrn = rbind(x_hae_subtrn, x_nonhae_subtrn)
    row.names(x_subtrn) <- NULL
    y_subtrn = c(y_hae_subtrn, y_nonhae_subtrn)
    dat_subtrn = data.frame(as.numeric(y_subtrn), x_subtrn)
    names(dat_subtrn)[1]='HAE'
    
    x_hae_val = dat_hae_val[,-match(c('PATIENT_ID', 'HAE'), names(dat_hae_val))]
    y_hae_val = dat_hae_val$HAE
    x_nonhae_val = dat_nonhae_val[,-match(grep('patient_id|hae', names(dat_nonhae_val), valu=T, perl=T, ignore.case = T)
                                          , names(dat_nonhae_val))]
    y_nonhae_val = dat_nonhae_val$HAE
    
    x_val = rbind(x_hae_val, x_nonhae_val)
    row.names(x_val) <- NULL
    y_val = c(y_hae_val, y_nonhae_val)
    dat_val = data.frame(as.numeric(y_val), x_val)
    names(dat_val)[1]='HAE'
    
    # (1) Underbagging LASSO / Random Forest
    ans_lasso_rf = undbag_lasso_rf(rf_formula=rf.formula, dat_pos=dat_hae_subtrn, dat_neg=dat_nonhae_subtrn, iters=lasso_rf_iters, mtry=25, ntree=300)
    save(ans_lasso_rf, file=pste0(plots_path, 'rf_fit_on'))
    prob_lasso = rep(0, length(y_val))
    prob_rf = rep(0, length(y_val))
    
    for (i in 1:lasso_rf_iters){ ###
        #print(paste(i,"/",lasso_rf_iters,sep=""))
        # 		prob_lasso = prob_lasso + predict(ans_lasso_rf$undbag_lasso_fit[[i]], as.matrix(x_val), s="lambda.min", type="response")/lasso_rf_iters
        prob_rf = prob_rf + predict(ans_lasso_rf$undbag_rf_fit[[i]], dat_val[, -1], type = "prob")[,2]/lasso_rf_iters
    }
    
    
    
    
    
    # (2) Logistic regression
    #         logit_fit = glm(HAE~., family=binomial(), data=dat_subtrn)
    # 
    #         prob_logit = rep(0, length(y_val))
    # 	prob_logit = predict(logit_fit, dat_val[,-1], type="response")
    
    
    # Save results for each fold
    cv_label[[m]] = y_val
    # 	cv_prob_lasso[[m]] = prob_lasso
    cv_prob_rf[[m]] = prob_rf
    # 	cv_prob_logit[[m]] = prob_logit
    
    print(Sys.time()-start1)
    
}

print(Sys.time()-start)

# save(cv_label, cv_prob_lasso, cv_prob_rf, cv_prob_logit, file=paste(data_path, "cv_lasso_rf_logit_prob_nov26.RData", sep=""))
save(cv_label, cv_prob_rf, file=paste(plots_path, "cv_rf_prob_Mar30.RData", sep=""))


#########################################################################
### Model training (all training data)
#########################################################################

Sys.time()->start

# (1) Underbagging LASSO / Random Forest
trn_ans_lasso_rf = undbag_lasso_rf(rf_formula=rf.formula, dat_pos=dat_hae_trn, dat_neg=dat_nonhae_trn, iters=lasso_rf_iters, mtry=25, ntree=300)
# trn_undbag_lasso_fit = trn_ans_lasso_rf$undbag_lasso_fit
trn_undbag_rf_fit = trn_ans_lasso_rf$undbag_rf_fit

# (2) Logistic regression
# trn_logit_fit = glm(HAE~., family=binomial(), data=dat_trn)

print(Sys.time()-start)

save(dat_hae_trn, dat_nonhae_trn, trn_undbag_rf_fit, file=paste(plots_path, "trn_rf_fit_Mar31.RData", sep=""))


#########################################################################
### Apply all-training model to testing data
#########################################################################

# Load: dat_hae_trn, dat_nonhae_trn, trn_undbag_lasso_fit, trn_undbag_rf_fit, trn_logit_fit
load(paste(plots_path, "trn_rf_fit_Mar31.RData", sep=""))
# Load training/testing data: dat_hae_trn, dat_hae_tst, dat_nonhae_trn, dat_nonhae_tst
load(paste(plots_path1, "dat_hae_trn_tst_split_Mar31.RData", sep=""))

x_hae_trn = dat_hae_trn[,-match(c('PATIENT_ID', 'HAE'), names(dat_hae_trn))]
y_hae_trn = dat_hae_trn[,match('HAE', names(dat_hae_trn))]
x_hae_tst = dat_hae_tst[,-match(c('PATIENT_ID', 'HAE'), names(dat_hae_trn))]
y_hae_tst = dat_hae_tst[,match('HAE', names(dat_hae_trn))]

dat_nonhae_trn$HAE <- 0
dat_nonhae_tst$HAE <- 0

x_nonhae_trn = dat_nonhae_trn[,-match(grep('patient_id|hae', names(dat_nonhae_trn), valu=T, perl=T, ignore.case = T)
                                      , names(dat_nonhae_trn))]
y_nonhae_trn = dat_nonhae_trn[,match('HAE', names(dat_nonhae_trn))]
x_nonhae_tst = dat_nonhae_tst[,-match(grep('patient_id|hae', names(dat_nonhae_tst), valu=T, perl=T, ignore.case = T)
                                      , names(dat_nonhae_tst))]
y_nonhae_tst = dat_nonhae_tst[,match('HAE', names(dat_nonhae_tst))]

x_nonhae_tst <- x_nonhae_tst[, match(names(x_hae_tst), names(x_nonhae_tst))]
x_nonhae_trn <- x_nonhae_trn[, match(names(x_hae_trn), names(x_nonhae_trn))]
x_tst = rbind(x_hae_tst, x_nonhae_tst)
y_tst = c(y_hae_tst, y_nonhae_tst)
dat_tst = data.frame(y_tst, x_tst)
names(dat_tst)[1]='HAE'

x_trn = rbind(x_hae_trn, x_nonhae_trn)
y_trn = c(y_hae_trn, y_nonhae_trn)
dat_trn = data.frame(y_trn, x_trn)
names(dat_trn)[1]='HAE'

# lasso_rf_iters = 200

tst_label = y_tst
tst_prob_logit = rep(0, length(y_tst))
tst_prob_wgtlasso = rep(0, length(y_tst))
tst_prob_lasso = rep(0, length(y_tst))
tst_prob_rf = rep(0, length(y_tst))

Sys.time()->start

# Logistic regression
# tst_prob_logit = predict(trn_logit_fit, dat_tst[,-match('HAE', names(dat_tst))], type="response")

# Bagging LASSO, Bagging Random Forest
for (i in 1:lasso_rf_iters){
    # 	tst_prob_lasso = tst_prob_lasso + predict(trn_undbag_lasso_fit[[i]], as.matrix(x_tst), s="lambda.min", type="response")/lasso_rf_iters
    tst_prob_rf = tst_prob_rf + predict(trn_undbag_rf_fit[[i]], dat_tst[, -match('HAE', names(dat_tst))], type = "prob")[,2]/lasso_rf_iters
}

print(Sys.time()-start)
# Time difference of 12.26336 mins

save(tst_label, tst_prob_rf, file=paste(plots_path, "tst_rf_prob.RData", sep=""))















