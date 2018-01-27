# 15.2.1 for all the Lasso experiments we did before, of all the 
# covariates involved in the Lasso modeling, summarize which covariates 
# were removed from the models and which were left.  

dir <- "F:\\Jie\\Shire_follow_up\\03_Results\\"
outdir <- "F:\\Jie\\Shire_follow_up\\03_Results\\summary\\covariates_retained_lasso\\"
covar <- 'for80+'
model <- 'Lasso_auc'
n.simu <- 5

get_covar_left_lasso <- function(dir, covar, model, n.simu){
    dataDir <- paste0(dir, covar, '\\')
    modelDir <- paste0(dir, covar, '\\', model, '\\wt=0.005_norm=TRUE\\')
    load(paste0(dataDir, 'trVlNormSim', simu, '_cv1.RData'))
    coef_org <- setdiff(names(cv_trTs$cv_tr), 'HAE')
    retain_n <- rep(0, length(coef_org))
    for(simu in 1:n.simu){
        if(grepl('similarity', model, ignore.case = T)){
            coef_left <- read.csv(paste0(modelDir, 'F_model_coefficient_sim', simu,'.csv'), stringsAsFactors = F)$variable
            
        }else{
            coef_left <- read.csv(paste0(modelDir, 'model_coefficient_sim', simu,'.csv'), stringsAsFactors = F)$variable
            
        }
        coef <- list(coef_org=coef_org, coef_left=coef_left)
        retain_n <- retain_n+ ifelse(is.na(match(coef_org, coef_left)), 0, 1)
    }
    retain_n <- data.frame(covar=coef_org, retain_n=retain_n)
    write.csv(retain_n, paste0(outdir, 'covariates_retained_', covar, '_', model, '.csv'))
    return('finished!\n')
}

get_covar_left_lasso(dir
                     , covar='for80+'
                     , model="Lasso_aupr0.01_0.1"
                     , n.simu)
get_covar_left_lasso(dir
                     , covar='for80+'
                     , model="Lasso"
                     , n.simu)
get_covar_left_lasso(dir
                     , covar='for200+'
                     , model="Lasso"
                     , n.simu)
get_covar_left_lasso(dir
                     , covar='for200+'
                     , model="Lasso_auc"
                     , n.simu)
get_covar_left_lasso(dir
                     , covar='for200+a'
                     , model="Lasso"
                     , n.simu)
get_covar_left_lasso(dir
                     , covar='for200+a'
                     , model="Lasso_auc"
                     , n.simu)
get_covar_left_lasso(dir
                     , covar='for200+a'
                     , model="Lasso_aupr0.01_0.1"
                     , n.simu)
get_covar_left_lasso(dir
                     , covar='for200+a'
                     , model="Similarity_aupr0.01_0.1"
                     , n.simu)

