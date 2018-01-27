#########################################################################
### Purpose: logistic regression coefficients
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
load(paste(data_path, "trn_lasso_rf_logit_fit_rx.RData", sep=""))
logit_fit_rx =  data.frame(var=names(trn_logit_fit$coef), coef=trn_logit_fit$coef)
row.names(logit_fit_rx)= NULL
write.csv(logit_fit_rx, file=paste(data_path,"logit_fit_rx.csv", sep="") ,row.names=FALSE)

# Load training/testing data: dat_hae_trn, dat_hae_tst, dat_nonhae_trn, dat_nonhae_tst
load(paste(data_path, "trn_lasso_rf_logit_fit_dx.RData", sep=""))
logit_fit_dx =  data.frame(var=names(trn_logit_fit$coef), coef=trn_logit_fit$coef)
row.names(logit_fit_dx)= NULL
write.csv(logit_fit_dx, file=paste(data_path,"logit_fit_dx.csv", sep="") ,row.names=FALSE)

# Load training/testing data: dat_hae_trn, dat_hae_tst, dat_nonhae_trn, dat_nonhae_tst
load(paste(data_path, "trn_lasso_rf_logit_fit_nov26.RData", sep=""))
logit_fit_rxdx =  data.frame(var=names(trn_logit_fit$coef), coef=trn_logit_fit$coef)
row.names(logit_fit_rxdx)= NULL
write.csv(logit_fit_rxdx, file=paste(data_path,"logit_fit_rxdx.csv", sep="") ,row.names=FALSE)



















