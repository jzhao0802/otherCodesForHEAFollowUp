#########################################################################
### Purpose:
#########################################################################

# Clear work space
# Clear work space
rm(list=ls())
lasso_rf_iters = 200
# R work directory
wk_dir = "F:\\Jie\\Shire_follow_up\\02_Code\\HAE_R_codes_Dec15\\"
# Data read and save path
data_path = "F:\\Jie\\Shire_follow_up\\03_Results\\for_old200K&1233\\"
# Plot save path
plots_path1 = "F:\\Jie\\Shire_follow_up\\03_Results\\for_old200K&1233\\"
plots_path <- paste0(plots_path1, 'iters=', lasso_rf_iters, '\\')

#########################################################################
### Source functions + load data
#########################################################################

# Setup R work directory
setwd(wk_dir)
# Load R packages
source("loadpackage.R")
# Auxiliary functions
source("auxfunctions.R")

load(paste(plots_path1, "dat_hae_trn_tst_split_Mar31.RData", sep=""))

#########################################################################
### Model performances (average over 5-fold validation sets)
#########################################################################

kfold_out = 5
# lasso_rf_iters = 3
sens_levels = seq(0.5, 0.05, by = -0.05)

# Read in weighted LASSO CV results
# cv_prob_wgtlasso = list()
# cv_label_wgtlasso = list()
# for (m in 1:kfold_out) {
# 	dd <- read.csv(paste(data_path, "pred_score_val_iter_", m, ".csv", sep=""), header=T)
#  	cv_prob_wgtlasso[[m]] = dd$best_pred_val
# 	cv_label_wgtlasso[[m]] = as.numeric(dd$val_y)
# }
# 
# # Read in weighted SVM CV results
# cv_prob_wgtsvm = list()
# cv_label_wgtsvm = list()
# for (m in 1:kfold_out) {
# 	dd <- read.csv(paste(data_path, "Deliver_on_1210_Hui\\Biased_SVM\\CV_Model\\val_pred_score_iter_", m, ".csv", sep=""), header=T)
#  	cv_prob_wgtsvm[[m]] = dd$prob
# 	cv_label_wgtsvm[[m]] = as.numeric(dd$true_label)
# }
# 

# pred_cv_wgtlasso =list()
# perf_roc_cv_wgtlasso = list()
# auc_cv_wgtlasso = rep(0,kfold_out)
# perf_pr_cv_wgtlasso = list()
# aupr_cv_wgtlasso = rep(0,kfold_out)
# 
# pred_cv_wgtsvm =list()
# perf_roc_cv_wgtsvm = list()
# auc_cv_wgtsvm = rep(0,kfold_out)
# perf_pr_cv_wgtsvm = list()
# aupr_cv_wgtsvm = rep(0,kfold_out)
# 
# pred_cv_lasso =list()
# perf_roc_cv_lasso = list()
# auc_cv_lasso = rep(0,kfold_out)
# perf_pr_cv_lasso = list()
# aupr_cv_lasso = rep(0,kfold_out)

pred_cv_rf =list()
perf_roc_cv_rf = list()
auc_cv_rf = rep(0,kfold_out)
perf_pr_cv_rf = list()
aupr_cv_rf = rep(0,kfold_out)

# pred_cv_logit =list()
# perf_roc_cv_logit = list()
# auc_cv_logit = rep(0,kfold_out)
# perf_pr_cv_logit = list()
# aupr_cv_logit = rep(0,kfold_out)

# ppv_cv_logit = array(0, dim=c(kfold_out, length(sens_levels)))
# ppv_cv_wgtlasso = array(0, dim=c(kfold_out, length(sens_levels)))
# ppv_cv_wgtsvm = array(0, dim=c(kfold_out, length(sens_levels)))
# ppv_cv_lasso = array(0, dim=c(kfold_out, length(sens_levels)))
ppv_cv_rf = array(0, dim=c(kfold_out, length(sens_levels)))
# Load: cv_label, cv_prob_lasso, cv_prob_rf, cv_prob_logit
load(paste(plots_path, "cv_rf_prob_Mar30.RData", sep=""))

for (m in 1:kfold_out) {

	# Weighted LASSO
#         pred_cv_wgtlasso[[m]] <- prediction(cv_prob_wgtlasso[[m]], cv_label_wgtlasso[[m]])
# 	perf_roc_cv_wgtlasso[[m]] <- performance(pred_cv_wgtlasso[[m]] ,"tpr","fpr")
# 	auc_cv_wgtlasso[m] <- performance(pred_cv_wgtlasso[[m]],"auc")@y.values[[1]]
# 	perf_pr_cv_wgtlasso[[m]] <- performance(pred_cv_wgtlasso[[m]] , "ppv", "sens")
# 	aupr_cv_wgtlasso[m] <- aupr_trapz(perf_pr_cv_wgtlasso[[m]]@x.values[[1]], perf_pr_cv_wgtlasso[[m]]@y.values[[1]])
# 	ppv_cv_wgtlasso[m,] = ppv_sens_v(perf_pr_cv_wgtlasso[[m]]@x.values[[1]], perf_pr_cv_wgtlasso[[m]]@y.values[[1]], sens_levels)

	# Weighted SVM
#         pred_cv_wgtsvm[[m]] <- prediction(cv_prob_wgtsvm[[m]], cv_label_wgtsvm[[m]])
# 	perf_roc_cv_wgtsvm[[m]] <- performance(pred_cv_wgtsvm[[m]] ,"tpr","fpr")
# 	auc_cv_wgtsvm[m] <- performance(pred_cv_wgtsvm[[m]],"auc")@y.values[[1]]
# 	perf_pr_cv_wgtsvm[[m]] <- performance(pred_cv_wgtsvm[[m]] , "ppv", "sens")
# 	aupr_cv_wgtsvm[m] <- aupr_trapz(perf_pr_cv_wgtsvm[[m]]@x.values[[1]], perf_pr_cv_wgtsvm[[m]]@y.values[[1]])
# 	ppv_cv_wgtsvm[m,] = ppv_sens_v(perf_pr_cv_wgtsvm[[m]]@x.values[[1]], perf_pr_cv_wgtsvm[[m]]@y.values[[1]], sens_levels)

	# LASSO
#         pred_cv_lasso[[m]] <- prediction(cv_prob_lasso[[m]], cv_label[[m]])
# 	perf_roc_cv_lasso[[m]] <- performance(pred_cv_lasso[[m]] ,"tpr","fpr")
# 	auc_cv_lasso[m] <- performance(pred_cv_lasso[[m]],"auc")@y.values[[1]]
# 	perf_pr_cv_lasso[[m]] <- performance(pred_cv_lasso[[m]] , "ppv", "sens")
# 	aupr_cv_lasso[m] <- aupr_trapz(perf_pr_cv_lasso[[m]]@x.values[[1]], perf_pr_cv_lasso[[m]]@y.values[[1]])
# 	ppv_cv_lasso[m,] = ppv_sens_v(perf_pr_cv_lasso[[m]]@x.values[[1]], perf_pr_cv_lasso[[m]]@y.values[[1]], sens_levels)
# 
	# Random Forest
        pred_cv_rf[[m]] <- prediction(cv_prob_rf[[m]], cv_label[[m]])
	perf_roc_cv_rf[[m]] <- performance(pred_cv_rf[[m]] ,"tpr","fpr")
	auc_cv_rf[m] <- performance(pred_cv_rf[[m]],"auc")@y.values[[1]]
	perf_pr_cv_rf[[m]] <- performance(pred_cv_rf[[m]] , "ppv", "sens")
	aupr_cv_rf[m] <- aupr_trapz(perf_pr_cv_rf[[m]]@x.values[[1]], perf_pr_cv_rf[[m]]@y.values[[1]])
	ppv_cv_rf[m,] = ppv_sens_v(perf_pr_cv_rf[[m]]@x.values[[1]], perf_pr_cv_rf[[m]]@y.values[[1]], sens_levels)

# 	# Logistc regression
#         pred_cv_logit[[m]] <- prediction(cv_prob_logit[[m]], cv_label[[m]])
# 	perf_roc_cv_logit[[m]] <- performance(pred_cv_logit[[m]] ,"tpr","fpr")
# 	auc_cv_logit[m] <- performance(pred_cv_logit[[m]],"auc")@y.values[[1]]
# 	perf_pr_cv_logit[[m]] <- performance(pred_cv_logit[[m]] , "ppv", "sens")
# 	aupr_cv_logit[m] <- aupr_trapz(perf_pr_cv_logit[[m]]@x.values[[1]], perf_pr_cv_logit[[m]]@y.values[[1]])
# 	ppv_cv_logit[m,] = ppv_sens_v(perf_pr_cv_logit[[m]]@x.values[[1]], perf_pr_cv_logit[[m]]@y.values[[1]], sens_levels)

}


# Output ROC curve (average over 5 folds)

# pd_cv_wgtlasso <- prediction(cv_prob_wgtlasso, cv_label_wgtlasso)
# pf_roc_cv_wgtlasso <- performance(pd_cv_wgtlasso,"tpr","fpr")
# 
# pd_cv_wgtsvm <- prediction(cv_prob_wgtsvm, cv_label_wgtsvm)
# pf_roc_cv_wgtsvm <- performance(pd_cv_wgtsvm,"tpr","fpr")
# 
# 
# pd_cv_lasso <- prediction(cv_prob_lasso, cv_label)
# pf_roc_cv_lasso <- performance(pd_cv_lasso,"tpr","fpr")

pd_cv_rf <- prediction(cv_prob_rf, cv_label)
pf_roc_cv_rf <- performance(pd_cv_rf,"tpr","fpr")

# pd_cv_logit <- prediction(cv_prob_logit, cv_label)
# pf_roc_cv_logit <- performance(pd_cv_logit,"tpr","fpr")
# 
# # ## Plot ROC curve
# png(filename=paste(plots_path, "cv_roc_rxdx.png", sep=""))
# plot(pf_roc_cv_logit, avg="vertical", col="black", lwd=2, main='Rx/Dx model: ROC curve (5-fold CV)')
# plot(pf_roc_cv_wgtlasso, avg="vertical", col="blue", add=TRUE, lwd=2)
# plot(pf_roc_cv_lasso, avg="vertical", col="green", add=TRUE, lwd=2)
# plot(pf_roc_cv_rf, avg="vertical", col="red", add=TRUE, lwd=2)
# plot(pf_roc_cv_wgtsvm, avg="vertical", col="purple", add=TRUE, lwd=2)
# abline(a=0,b=1,lty=2)
# legend(x="bottomright",legend=c("Logit", "Weighted LASSO", "Bagging LASSO", "Bagging RF", "Weighted SVM"), lty=c(1,1,1,1,1), lwd=c(2,2,2,2,2), col=c('black','blue','green','red','purple'))
# dev.off()

# Output PR curve (average over 5 folds)
# pf_pr_cv_wgtlasso <- performance(pd_cv_wgtlasso,"ppv", "sens")
# pf_pr_cv_wgtsvm <- performance(pd_cv_wgtsvm,"ppv", "sens")
# pf_pr_cv_lasso <- performance(pd_cv_lasso,"ppv", "sens")
pf_pr_cv_rf <- performance(pd_cv_rf,"ppv", "sens")
# pf_pr_cv_logit <- performance(pd_cv_logit,"ppv", "sens")

# ## Plot PR curve
# png(filename=paste(plots_path, "cv_pr_rxdx.png", sep=""))
# plot(pf_pr_cv_logit, avg="vertical", col="black", lwd=2, main='Rx/Dx model: PR curve (5-fold CV)')
# plot(pf_pr_cv_wgtlasso, avg="vertical", col="blue", add=TRUE, lwd=2)
# plot(pf_pr_cv_lasso, avg="vertical", col="green", add=TRUE, lwd=2)
# plot(pf_pr_cv_rf, avg="vertical", col="red", add=TRUE, lwd=2)
# plot(pf_pr_cv_wgtsvm, avg="vertical", col="purple", add=TRUE, lwd=2)
# legend(x="topright",legend=c("Logit", "Weighted LASSO", "Bagging LASSO", "Bagging RF", "Weighted SVM"), lty=c(1,1,1,1,1), lwd=c(2,2,2,2,2), col=c('black','blue','green','red','purple'))
# dev.off()

## Plot ROC and PR curve
pdf(paste(plots_path, "cv_roc_pr_RF.pdf", sep=""))
par(mfrow=c(2,1))
par(pty='m')
par(cex.lab=1.2, cex.axis=0.9)

plot(pf_roc_cv_rf, avg="vertical", col="red", lwd=2)
legend(x="topright",legend=c('ROC CURVE'), lty=c(1), lwd=c(2), col=c('red'))

plot(pf_pr_cv_rf, avg="vertical", col="blue", lwd=2)
legend(x="topright",legend=c("PR CURVE"), lty=c(1), lwd=c(2), col=c('blue'))

dev.off()

# PPV at different sensitivity levels
# ppv_cv_logit_mean = apply(ppv_cv_logit, 2, mean)
# ppv_cv_wgtlasso_mean = apply(ppv_cv_wgtlasso, 2, mean)
# ppv_cv_wgtsvm_mean = apply(ppv_cv_wgtsvm, 2, mean)
# ppv_cv_lasso_mean = apply(ppv_cv_lasso, 2, mean)
ppv_cv_rf_mean = apply(ppv_cv_rf, 2, mean)

cv_auc_aupr_ppv = data.frame(
	Metrics = c('AUC', 'AUPR', paste('PPV (Sensitivity=', 100*sens_levels, '%)', sep="")),
# 	Logit = c(mean(auc_cv_logit), mean(aupr_cv_logit), ppv_cv_logit_mean),
# 	Weighted_LASSO = c(mean(auc_cv_wgtlasso), mean(aupr_cv_wgtlasso), ppv_cv_wgtlasso_mean),
# 	Bagging_LASSO = c(mean(auc_cv_lasso), mean(aupr_cv_lasso), ppv_cv_lasso_mean),
	Bagging_RandomForest = c(mean(auc_cv_rf), mean(aupr_cv_rf), ppv_cv_rf_mean)
# 		Weighted_SVM = c(mean(auc_cv_wgtsvm), mean(aupr_cv_wgtsvm), ppv_cv_wgtsvm_mean)
)

write.csv(cv_auc_aupr_ppv, file=paste(plots_path,"cv_auc_aupr_ppv_rxdx.csv", sep="") ,row.names=FALSE)

#########################################################################
### (B.4) Model performances (on testing data)
#########################################################################

#########################################################################
### (B.4.1) CV-training cut-off probabilities for each sensitivity level
#########################################################################

# Load: cv_label, cv_prob_lasso, cv_prob_rf, cv_prob_logit
load(paste(plots_path, "cv_rf_prob_Mar30.RData", sep=""))

kfold_out = 5
# lasso_rf_iters = 200
sens_levels = seq(0.5, 0.05, by = -0.05)


# From Hui Jin's CV results using Weighted LASSO and weighted SVM
# cv_prob_wgtlasso = list()
# cv_label_wgtlasso = list()
# for (m in 1:kfold_out) {
# 	dd <- read.csv(paste(data_path, "pred_score_val_iter_", m, ".csv", sep=""), header=T)
#  	cv_prob_wgtlasso[[m]] = dd$best_pred_val
# 	cv_label_wgtlasso[[m]] = as.numeric(dd$val_y)
# }
# cv_prob_wgtsvm = list()
# cv_label_wgtsvm = list()
# for (m in 1:kfold_out) {
# 	dd <- read.csv(paste(data_path, "Deliver_on_1210_Hui\\Biased_SVM\\CV_Model\\val_pred_score_iter_", m, ".csv", sep=""), header=T)
#  	cv_prob_wgtsvm[[m]] = dd$prob
# 	cv_label_wgtsvm[[m]] = as.numeric(dd$true_label)
# }


# prob_cut_sens_logit = array(0, dim=c(kfold_out, length(sens_levels)))
# prob_cut_sens_wgtlasso = array(0, dim=c(kfold_out, length(sens_levels)))
# prob_cut_sens_wgtsvm = array(0, dim=c(kfold_out, length(sens_levels)))
# prob_cut_sens_lasso = array(0, dim=c(kfold_out, length(sens_levels)))
prob_cut_sens_rf = array(0, dim=c(kfold_out, length(sens_levels)))

for (m in 1:kfold_out) {

# 	# Logistic Regression
#         prob_cut_sens_logit[m,] <- prob_cut_sens_v(prediction(cv_prob_logit[[m]], cv_label[[m]]), sens_levels)
# 
# 	# Weighted LASSO
#         prob_cut_sens_wgtlasso[m,] <- prob_cut_sens_v(prediction(cv_prob_wgtlasso[[m]], cv_label[[m]]), sens_levels)
# 
# 	# Weighted LASSO
#         prob_cut_sens_wgtsvm[m,] <- prob_cut_sens_v(prediction(cv_prob_wgtsvm[[m]], cv_label[[m]]), sens_levels)
# 
# 	# Bagging LASSO
#         prob_cut_sens_lasso[m,] <- prob_cut_sens_v(prediction(cv_prob_lasso[[m]], cv_label[[m]]), sens_levels)

	# Bagging Random Forest
        prob_cut_sens_rf[m,] <- prob_cut_sens_v(prediction(cv_prob_rf[[m]], cv_label[[m]]), sens_levels)


}

# prob_cut_sens_logit_cvmean = apply(prob_cut_sens_logit, 2, mean)
# prob_cut_sens_wgtlasso_cvmean = apply(prob_cut_sens_wgtlasso, 2, mean)
# prob_cut_sens_wgtsvm_cvmean = apply(prob_cut_sens_wgtsvm, 2, mean)
# prob_cut_sens_lasso_cvmean = apply(prob_cut_sens_lasso, 2, mean)
prob_cut_sens_rf_cvmean = apply(prob_cut_sens_rf, 2, mean)

# save(sens_levels, prob_cut_sens_logit_cvmean, prob_cut_sens_wgtlasso_cvmean, prob_cut_sens_wgtsvm_cvmean, prob_cut_sens_lasso_cvmean, prob_cut_sens_rf_cvmean, file=paste(plots_path, "cv_prob_cut_sens_rxdx.RData", sep=""))
save(sens_levels, prob_cut_sens_rf_cvmean, file=paste(plots_path, "cv_prob_cut_sens_rxdx.RData", sep=""))


#########################################################################
### (B.4.2) Apply all-training model to testing data
#########################################################################


###################################################################################################
### (B.4.3) Model performance on testing data
### (1) AUC, AUPR, PPV at different sensitivity levels
### (2) For each sensitivity level, produce confusion matrix with CV probability cutoff values
### (3) Plot ROC, PR curves
###################################################################################################

# Load: sens_levels, prob_cut_sens_logit_cvmean, prob_cut_sens_wgtlasso_cvmean, prob_cut_sens_lasso_cvmean, prob_cut_sens_rf_cvmean
load(paste(plots_path, "cv_prob_cut_sens_rxdx.RData", sep=""))
# Load: tst_label, tst_prob_logit, tst_prob_lasso, tst_prob_rf
load(paste(plots_path, "tst_rf_prob.RData", sep=""))

sens_levels = seq(0.5, 0.05, by = -0.05)

# # From Hui Jin's testing results using Weighted LASSO
# dd <- read.csv(paste(data_path, "pred_score_on_test.csv", sep=""), header=T)
# tst_prob_wgtlasso = dd$best_pred_ts
# 
# # From Hui Jin's testing results using Weighted SVM
# dd <- read.csv(paste(data_path, "Deliver_on_1210_Hui\\Biased_SVM\\Full_Model\\ts_pred_score.csv", sep=""), header=T)
# tst_prob_wgtsvm = dd$prob

tst_label_char = as.character(tst_label)

conf_logit = list()
conf_wgtlasso = list()
conf_wgtsvm = list()
conf_lasso = list()
conf_rf = list()

# Logistc regression
pred_tst_logit = prediction(tst_prob_logit, tst_label)
perf_roc_tst_logit = performance(pred_tst_logit ,"tpr","fpr")
auc_tst_logit = performance(pred_tst_logit,"auc")@y.values[[1]]
perf_pr_tst_logit = performance(pred_tst_logit , "ppv", "sens")
aupr_tst_logit = aupr_trapz(perf_pr_tst_logit@x.values[[1]], perf_pr_tst_logit@y.values[[1]])
ppv_tst_logit = ppv_sens_v(perf_pr_tst_logit@x.values[[1]], perf_pr_tst_logit@y.values[[1]], sens_levels)

for (i in 1:length(sens_levels)){
	tst_pred_class_logit = ifelse(tst_prob_logit >= prob_cut_sens_logit_cvmean[i], "1", "0")
	conf_logit[[i]] = confusionM(tst_pred_class_logit, tst_label_char, pos_char="1", neg_char="0")
}

# Weighted LASSO (require tst_prob_wgtlasso from "Hui Jin")
pred_tst_wgtlasso = prediction(tst_prob_wgtlasso, tst_label)
perf_roc_tst_wgtlasso = performance(pred_tst_wgtlasso ,"tpr","fpr")
auc_tst_wgtlasso = performance(pred_tst_wgtlasso,"auc")@y.values[[1]]
perf_pr_tst_wgtlasso = performance(pred_tst_wgtlasso , "ppv", "sens")
aupr_tst_wgtlasso = aupr_trapz(perf_pr_tst_wgtlasso@x.values[[1]], perf_pr_tst_wgtlasso@y.values[[1]])
ppv_tst_wgtlasso = ppv_sens_v(perf_pr_tst_wgtlasso@x.values[[1]], perf_pr_tst_wgtlasso@y.values[[1]], sens_levels)

for (i in 1:length(sens_levels)){
	tst_pred_class_wgtlasso = ifelse(tst_prob_wgtlasso >= prob_cut_sens_wgtlasso_cvmean[i], "1", "0")
	conf_wgtlasso[[i]] = confusionM(tst_pred_class_wgtlasso, tst_label_char, pos_char="1", neg_char="0")
}

# Weighted SVM (require tst_prob_wgtsvm from "Hui Jin")
pred_tst_wgtsvm = prediction(tst_prob_wgtsvm, tst_label)
perf_roc_tst_wgtsvm = performance(pred_tst_wgtsvm ,"tpr","fpr")
auc_tst_wgtsvm = performance(pred_tst_wgtsvm,"auc")@y.values[[1]]
perf_pr_tst_wgtsvm = performance(pred_tst_wgtsvm , "ppv", "sens")
aupr_tst_wgtsvm = aupr_trapz(perf_pr_tst_wgtsvm@x.values[[1]], perf_pr_tst_wgtsvm@y.values[[1]])
ppv_tst_wgtsvm = ppv_sens_v(perf_pr_tst_wgtsvm@x.values[[1]], perf_pr_tst_wgtsvm@y.values[[1]], sens_levels)

for (i in 1:length(sens_levels)){
	tst_pred_class_wgtsvm = ifelse(tst_prob_wgtsvm >= prob_cut_sens_wgtsvm_cvmean[i], "1", "0")
	conf_wgtsvm[[i]] = confusionM(tst_pred_class_wgtsvm, tst_label_char, pos_char="1", neg_char="0")
}


# LASSO
pred_tst_lasso = prediction(tst_prob_lasso, tst_label)
perf_roc_tst_lasso = performance(pred_tst_lasso ,"tpr","fpr")
auc_tst_lasso = performance(pred_tst_lasso,"auc")@y.values[[1]]
perf_pr_tst_lasso = performance(pred_tst_lasso , "ppv", "sens")
aupr_tst_lasso = aupr_trapz(perf_pr_tst_lasso@x.values[[1]], perf_pr_tst_lasso@y.values[[1]])
ppv_tst_lasso = ppv_sens_v(perf_pr_tst_lasso@x.values[[1]], perf_pr_tst_lasso@y.values[[1]], sens_levels)

for (i in 1:length(sens_levels)){
	tst_pred_class_lasso = ifelse(tst_prob_lasso >= prob_cut_sens_lasso_cvmean[i], "1", "0")
	conf_lasso[[i]] = confusionM(tst_pred_class_lasso, tst_label_char, pos_char="1", neg_char="0")
}

# Random Forest
pred_tst_rf = prediction(tst_prob_rf, tst_label)
perf_roc_tst_rf = performance(pred_tst_rf ,"tpr","fpr")
auc_tst_rf = performance(pred_tst_rf,"auc")@y.values[[1]]
perf_pr_tst_rf = performance(pred_tst_rf , "ppv", "sens")
aupr_tst_rf = aupr_trapz(perf_pr_tst_rf@x.values[[1]], perf_pr_tst_rf@y.values[[1]])
ppv_tst_rf = ppv_sens_v(perf_pr_tst_rf@x.values[[1]], perf_pr_tst_rf@y.values[[1]], sens_levels)

for (i in 1:length(sens_levels)){
	tst_pred_class_rf = ifelse(tst_prob_rf >= prob_cut_sens_rf_cvmean[i], "1", "0")
	conf_rf[[i]] = confusionM(tst_pred_class_rf, tst_label_char, pos_char="1", neg_char="0")
}

## (1) AUC, AUPR, PPV at different sensitivity levels
tst_auc_aupr_ppv = data.frame(
	Metrics = c('AUC', 'AUPR', paste('PPV (Sensitivity=', 100*sens_levels, '%)', sep="")),
# 	Logit = c(auc_tst_logit, aupr_tst_logit, ppv_tst_logit),
# 	Weighted_LASSO = c(auc_tst_wgtlasso, aupr_tst_wgtlasso, ppv_tst_wgtlasso),
# 	Bagging_LASSO = c(auc_tst_lasso, aupr_tst_lasso, ppv_tst_lasso),
	Bagging_RandomForest = c(auc_tst_rf, aupr_tst_rf, ppv_tst_rf)
# 	Weighted_SVM = c(auc_tst_wgtsvm, aupr_tst_wgtsvm, ppv_tst_wgtsvm)
)

write.csv(tst_auc_aupr_ppv, file=paste(plots_path,"tst_auc_aupr_ppv_rxdx.csv", sep="") ,row.names=FALSE)


## (2) For each sensitivity level, produce confusion matrix with CV probability cutoff values
tst_cutoff_sens_ppv = data.frame()
for (i in 1:length(sens_levels)){
# 		Logit = c(prob_cut_sens_logit_cvmean[i], conf_logit[[i]]$sensitivity, conf_logit[[i]]$ppv)
# 		Weighted_LASSO = c(prob_cut_sens_wgtlasso_cvmean[i], conf_wgtlasso[[i]]$sensitivity, conf_wgtlasso[[i]]$ppv)
# 		Bagging_LASSO = c(prob_cut_sens_lasso_cvmean[i], conf_lasso[[i]]$sensitivity, conf_lasso[[i]]$ppv)
		Bagging_RandomForest = c(prob_cut_sens_rf_cvmean[i], conf_rf[[i]]$sensitivity, conf_rf[[i]]$ppv)
# 		Weighted_SVM = c(expit(prob_cut_sens_wgtsvm_cvmean[i]), conf_wgtsvm[[i]]$sensitivity, conf_wgtsvm[[i]]$ppv)
		if(i==1) {
			tst_cutoff_sens_ppv = data.frame(
				Metrics = c(paste('Sens_Level_', sens_levels[i]*100, "%", sep=""), 'Probability cut-off', 'Sensitivity', 'PPV'),
# 				Logit = c(0, Logit),
# 				Weighted_LASSO = c(0, Weighted_LASSO),
# 				Bagging_LASSO = c(0, Bagging_LASSO),
				Bagging_RandomForest = c(0, Bagging_RandomForest)
# 				Weighted_SVM = c(0, Weighted_SVM)
				)
			} else{
				tst_cutoff_sens_ppv = rbind(tst_cutoff_sens_ppv,
					data.frame(
					Metrics = c(paste('Sens_Level_', sens_levels[i]*100, "%", sep=""), 'Probability cut-off', 'Sensitivity', 'PPV'),
# 					Logit = c(0, Logit),
# 					Weighted_LASSO = c(0, Weighted_LASSO),
# 					Bagging_LASSO = c(0, Bagging_LASSO),
					Bagging_RandomForest = c(0, Bagging_RandomForest)
# 				        Weighted_SVM = c(0, Weighted_SVM)
					))
				}
}

write.csv(tst_cutoff_sens_ppv, file=paste(plots_path,"tst_cutoff_sens_ppv_rxdx.csv", sep="") ,row.names=FALSE)


## (3) ROC curve, PR curve

## Plot ROC curve
# png(filename=paste(plots_path, "tst_roc_rxdx.png", sep=""))
# plot(perf_roc_tst_logit, col="black", lwd=2, main='Rx/Dx model: ROC curve (testing)')
# plot(perf_roc_tst_wgtlasso, col="blue", add=TRUE, lwd=2)
# plot(perf_roc_tst_lasso, col="green", add=TRUE, lwd=2)
# plot(perf_roc_tst_rf, col="red", add=TRUE, lwd=2)
# plot(perf_roc_tst_wgtsvm, col="purple", add=TRUE, lwd=2)
# abline(a=0,b=1,lty=2)
# legend(x="bottomright",legend=c("Logit", "Weighted LASSO", "Bagging LASSO", "Bagging RF", "Weighted SVM"), lty=c(1,1,1,1,1), lwd=c(2,2,2,2,2), col=c('black','blue','green','red','purple'))

# dev.off()


## Plot PR curve
# png(filename=paste(plots_path, "tst_pr_rxdx.png", sep=""))
# plot(perf_pr_tst_logit, col="black", lwd=2, main='Rx/Dx model: PR curve (testing)')
# plot(perf_pr_tst_wgtlasso, col="blue", add=TRUE, lwd=2)
# plot(perf_pr_tst_lasso, col="green", add=TRUE, lwd=2)
# plot(perf_pr_tst_rf, col="red", add=TRUE, lwd=2)
# plot(perf_pr_tst_wgtsvm, col="purple", add=TRUE, lwd=2)
# legend(x="topright",legend=c("Logit", "Weighted LASSO", "Bagging LASSO", "Bagging RF", "Weighted SVM"), lty=c(1,1,1,1,1), lwd=c(2,2,2,2,2), col=c('black','blue','green','red','purple'))
# dev.off()
# 
pdf(paste(plots_path, "tst_roc_pr_RF.pdf", sep=""))
par(mfrow=c(2,1))
par(pty='m')
par(cex.lab=1.2, cex.axis=0.9)

plot(perf_roc_tst_rf, col="red", lwd=2)
legend(x="topright",legend=c('ROC CURVE'), lty=c(1), lwd=c(2), col=c('red'))

plot(perf_pr_tst_rf, col="blue", lwd=2)
legend(x="topright",legend=c("PR CURVE"), lty=c(1), lwd=c(2), col=c('blue'))

dev.off()



########################################################
# Variable importance from bagging LASSO and RF
########################################################

load(paste(plots_path, "trn_rf_fit_Mar31.RData", sep=""))

kfold_out = 5
# lasso_rf_iters = 20
p = dim(dat_hae_trn)[2]-2

# var_imp_lasso_array = array(0,dim=c(lasso_rf_iters,p))
var_imp_rf_array = array(0,dim=c(lasso_rf_iters,p))

for (i in 1:lasso_rf_iters){

	# LASSO var selection
# 	a=coef(trn_undbag_lasso_fit[[i]], s = "lambda.min")
# 	var_imp_lasso_array[i,(summary(a)$i[-1]-1)]=1

	# RF variable importance
	var_imp_rf_array[i,] = trn_undbag_rf_fit[[i]]$importance
	# if (i==1) {row_names= row.names(a)[-1]}
	
}
colnames(var_imp_rf_array) <- rownames(trn_undbag_rf_fit[[1]]$importance)

# var_imp_lasso = apply(var_imp_lasso_array, 2, mean)
var_imp_rf = apply(var_imp_rf_array, 2, mean)
var_imp_rf <- var_imp_rf[order(var_imp_rf, decreasing = T)]

# dat_imp_lasso_rf_tmp1 = data.frame(var_name=row_names, var_imp_lasso=var_imp_lasso, var_imp_rf=var_imp_rf)

write.csv(var_imp_rf, paste0(plots_path, 'feature_importance_score_rf.csv'))
# variable descriptions
var_desc_all <- read.xlsx(paste(data_path, "var_desc_all.xlsx", sep=""), 1, header=T)

load(paste(data_path, "dat_hae_1111_rf_nov26.RData", sep=""))
load(paste(data_path, "dat_nonhae_1111_rf.RData", sep=""))
dat_hae = dat_hae_1111_rf_nov26
dat_nonhae = dat_nonhae_1111_rf
names(dat_nonhae)[1]='PATIENT_ID'
dat = rbind(dat_hae, dat_nonhae)
x = model.matrix(HAE ~ .,dat[,-1])[,-1]
y = dat$HAE
dat = data.frame(PATIENT_ID=dat[,1], HAE=y, x)
idx_hae = 1:dim(dat_hae_1111_rxonly)[1]
dat_hae = dat[idx_hae,]
dat_nonhae = dat[-idx_hae,]

# variable means of HAE vs. Non-HAE
var_mean = data.frame(var_mean_hae = apply(dat_hae[,-(1:2)],2, function(x)mean(x, na.rm=TRUE)), var_mean_nonhae = apply(dat_nonhae[,-(1:2)],2, function(x)mean(x, na.rm=TRUE)))
var_mean = data.frame(Var=row.names(var_mean), var_mean)
row.names(var_mean)=NULL

dat_imp_lasso_rf_tmp2 = sqldf("select b.*, a.var_imp_lasso, a.var_imp_rf from dat_imp_lasso_rf_tmp1 a inner join var_desc_all b on a.var_name=b.Var")
dat_imp_lasso_rf_tmp3 = sqldf("select a.*, b.var_mean_hae, b.var_mean_nonhae from dat_imp_lasso_rf_tmp2 a inner join var_mean b on a.Var=b.Var")
dat_imp_lasso_rf = sqldf("select Var, Var_desc, var_mean_hae, var_mean_nonhae, var_imp_lasso, var_imp_rf from dat_imp_lasso_rf_tmp3")
write.csv(dat_imp_lasso_rf, file=paste(data_path,"trn_dat_imp_lasso_rf_rxdx.csv", sep="") ,row.names=FALSE)

dat_imp_lasso_tmp1 = dat_imp_lasso_rf[,-6]
o = rev(order(dat_imp_lasso_tmp1$var_imp_lasso))
dat_imp_lasso = dat_imp_lasso_tmp1[o,]
write.csv(dat_imp_lasso, file=paste(data_path,"trn_dat_imp_bagginglasso_rxdx.csv", sep="") ,row.names=FALSE)

dat_imp_rf_tmp1 = dat_imp_lasso_rf[,-5]
o = rev(order(dat_imp_rf_tmp1$var_imp_rf))
dat_imp_rf = dat_imp_rf_tmp1[o,]
write.csv(dat_imp_rf, file=paste(data_path,"trn_dat_imp_baggingrf_rxdx.csv", sep="") ,row.names=FALSE)



















