#########################################################################
### Purpose: compare in-sample/out-of-sample performance (Rx+Dx models)
#########################################################################

# Clear work space
rm(list=ls())

# R work directory
wk_dir = "C:\\HAE\\R\\"
# Data read and save path
data_path = "C:\\HAE\\Data\\"
# Plot save path
plots_path = "C:\\HAE\\Plots\\"

#########################################################################
### Source functions
#########################################################################

# Setup R work directory
setwd(wk_dir)
# Load R packages
source("loadpackage.R")
# Auxiliary functions
source("auxfunctions.R")

#########################################################################
### Data manupulation
#########################################################################

# Load training/testing data: dat_hae_trn, dat_hae_tst, dat_nonhae_trn, dat_nonhae_tst
load(paste(data_path, "dat_hae_trn_tst_split_nov26.RData", sep=""))

x_hae_trn = dat_hae_trn[,-(1:2)]
y_hae_trn = dat_hae_trn[,2]
x_hae_tst = dat_hae_tst[,-(1:2)]
y_hae_tst = dat_hae_tst[,2]

x_nonhae_trn = dat_nonhae_trn[,-(1:2)]
y_nonhae_trn = dat_nonhae_trn[,2]
x_nonhae_tst = dat_nonhae_tst[,-(1:2)]
y_nonhae_tst = dat_nonhae_tst[,2]

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
	flds_out_nonhae[[m]] = which(dat_nonhae_trn$PATIENT_ID %in% dat_hae_trn$PATIENT_ID[flds_out_hae[[m]]])
}
idx_nonhae_out = data.frame(id=1:n_nonhae_trn, flds_out=0)
for (m in 1:kfold_out){idx_nonhae_out$flds_out[flds_out_nonhae[[m]]]=m}

# random forest formula
rf.formula=as.simple.formula(names(dat_trn)[which(names(dat_trn)!='HAE')], 'HAE')
lasso_rf_iters = 200

Sys.time()->start

cv_label_val = list()
cv_prob_logit_val = list()

cv_label_subtrn = list()
cv_prob_logit_subtrn = list()

for (m in 1:kfold_out){

	print(paste(m,"/",kfold_out,sep=""))
	start1 <- Sys.time()

	idx_hae_val = which(idx_hae_out$flds_out==m)
	dat_hae_val = dat_hae_trn[idx_hae_val,]
	dat_hae_subtrn = dat_hae_trn[-idx_hae_val,]

	idx_nonhae_val = which(idx_nonhae_out$flds_out==m)
	dat_nonhae_val = dat_nonhae_trn[idx_nonhae_val,]
	dat_nonhae_subtrn = dat_nonhae_trn[-idx_nonhae_val,]

	x_hae_subtrn = dat_hae_subtrn[,-(1:2)]
	y_hae_subtrn = dat_hae_subtrn$HAE
	x_nonhae_subtrn = dat_nonhae_subtrn[,-(1:2)]
	y_nonhae_subtrn = dat_nonhae_subtrn$HAE

	x_subtrn = rbind(x_hae_subtrn, x_nonhae_subtrn)
	row.names(x_subtrn) <- NULL
	y_subtrn = c(y_hae_subtrn, y_nonhae_subtrn)
	dat_subtrn = data.frame(as.numeric(y_subtrn), x_subtrn)
	names(dat_subtrn)[1]='HAE'

	x_hae_val = dat_hae_val[,-(1:2)]
	y_hae_val = dat_hae_val$HAE
	x_nonhae_val = dat_nonhae_val[,-(1:2)]
	y_nonhae_val = dat_nonhae_val$HAE

	x_val = rbind(x_hae_val, x_nonhae_val)
	row.names(x_val) <- NULL
	y_val = c(y_hae_val, y_nonhae_val)
	dat_val = data.frame(as.numeric(y_val), x_val)
	names(dat_val)[1]='HAE'

        # (2) Logistic regression
        logit_fit = glm(HAE~., family=binomial(), data=dat_subtrn)

        prob_logit_val = rep(0, length(y_val))
	prob_logit_val = predict(logit_fit, dat_val[,-1], type="response")

        prob_logit_subtrn = rep(0, length(y_subtrn))
	prob_logit_subtrn = predict(logit_fit, dat_subtrn[,-1], type="response")


        # Save results for each fold
        cv_label_val[[m]] = y_val
	cv_prob_logit_val[[m]] = prob_logit_val

        cv_label_subtrn[[m]] = y_subtrn
	cv_prob_logit_subtrn[[m]] = prob_logit_subtrn

	print(Sys.time()-start1)

}

print(Sys.time()-start)


save(cv_label_val, cv_prob_logit_val, cv_label_subtrn, cv_prob_logit_subtrn, file=paste(data_path, "cv_logit_prob_val_subtrn.RData", sep=""))


#########################################################################
### Model performance on validation and subtraining sets
#########################################################################

kfold_out = 5
sens_levels = seq(0.5, 0.05, by = -0.05)

# load data: cv_label_val, cv_prob_logit_val, cv_label_subtrn, cv_prob_logit_subtrn
load(paste(data_path, "cv_logit_prob_val_subtrn.RData", sep=""))

pred_cv_logit_val =list()
perf_roc_cv_logit_val = list()
auc_cv_logit_val = rep(0,kfold_out)
perf_pr_cv_logit_val = list()
aupr_cv_logit_val = rep(0,kfold_out)
ppv_cv_logit_val = array(0, dim=c(kfold_out, length(sens_levels)))

pred_cv_logit_subtrn =list()
perf_roc_cv_logit_subtrn = list()
auc_cv_logit_subtrn = rep(0,kfold_out)
perf_pr_cv_logit_subtrn = list()
aupr_cv_logit_subtrn = rep(0,kfold_out)
ppv_cv_logit_subtrn = array(0, dim=c(kfold_out, length(sens_levels)))


cv_auc_aupr_ppv_logit = list()

for (m in 1:kfold_out) {

	# Logistc regression
	# Validation set
        pred_cv_logit_val[[m]] <- prediction(cv_prob_logit_val[[m]], cv_label_val[[m]])
	perf_roc_cv_logit_val[[m]] <- performance(pred_cv_logit_val[[m]] ,"tpr","fpr")
	auc_cv_logit_val[m] <- performance(pred_cv_logit_val[[m]],"auc")@y.values[[1]]
	perf_pr_cv_logit_val[[m]] <- performance(pred_cv_logit_val[[m]] , "ppv", "sens")
	aupr_cv_logit_val[m] <- aupr_trapz(perf_pr_cv_logit_val[[m]]@x.values[[1]], perf_pr_cv_logit_val[[m]]@y.values[[1]])
	ppv_cv_logit_val[m,] <- ppv_sens_v(perf_pr_cv_logit_val[[m]]@x.values[[1]], perf_pr_cv_logit_val[[m]]@y.values[[1]], sens_levels)

	# Sub-training set
        pred_cv_logit_subtrn[[m]] <- prediction(cv_prob_logit_subtrn[[m]], cv_label_subtrn[[m]])
	perf_roc_cv_logit_subtrn[[m]] <- performance(pred_cv_logit_subtrn[[m]] ,"tpr","fpr")
	auc_cv_logit_subtrn[m] <- performance(pred_cv_logit_subtrn[[m]],"auc")@y.values[[1]]
	perf_pr_cv_logit_subtrn[[m]] <- performance(pred_cv_logit_subtrn[[m]] , "ppv", "sens")
	aupr_cv_logit_subtrn[m] <- aupr_trapz(perf_pr_cv_logit_subtrn[[m]]@x.values[[1]], perf_pr_cv_logit_subtrn[[m]]@y.values[[1]])
	ppv_cv_logit_subtrn[m,] <- ppv_sens_v(perf_pr_cv_logit_subtrn[[m]]@x.values[[1]], perf_pr_cv_logit_subtrn[[m]]@y.values[[1]], sens_levels)


	cv_auc_aupr_ppv_logit[[m]] = data.frame(
		Fold = rep(m, (2+length(sens_levels))),
		Metrics = c('AUC', 'AUPR', paste('PPV (Sensitivity=', 100*sens_levels, '%)', sep="")),
		Logit_in_sample = c(auc_cv_logit_subtrn[m], aupr_cv_logit_subtrn[m], ppv_cv_logit_subtrn[m,]),
		Logit_out_of_sample = c(auc_cv_logit_val[m], aupr_cv_logit_val[m], ppv_cv_logit_val[m,])
	)
	cv_auc_aupr_ppv_logit[[m]]$overfitting = (cv_auc_aupr_ppv_logit[[m]]$Logit_in_sample - cv_auc_aupr_ppv_logit[[m]]$Logit_out_of_sample)/cv_auc_aupr_ppv_logit[[m]]$Logit_out_of_sample

}

for (m in 1:kfold_out) {
	if (m==1) {cv_auc_aupr_ppv_logit_inoutsamp_rxdx <- cv_auc_aupr_ppv_logit[[m]]} else {
		cv_auc_aupr_ppv_logit_inoutsamp_rxdx <- rbind(cv_auc_aupr_ppv_logit_inoutsamp_rxdx, cv_auc_aupr_ppv_logit[[m]])
		}
}

write.csv(cv_auc_aupr_ppv_logit_inoutsamp_rxdx, file=paste(data_path,"cv_auc_aupr_ppv_logit_inoutsamp_rxdx.csv", sep="") ,row.names=FALSE)

