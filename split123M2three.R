library(dplyr)
library(snowfall)
#step1: get the patientid of 1087 cleaned HAE
dataDir <- "F:\\Jie\\Shire_follow_up\\01_data"
resultDir <- "F:\\Jie\\Shire_follow_up\\01_Data"
load(paste0(resultDir, '\\HAE1087_ptid.RData'))#HAE1087_ptid
id_HAE <- HAE1087_ptid
HAE9692_ptid <- read.csv(paste0(resultDir, '\\patient_id_9692.csv'))[, 1]
id_LHAE <- HAE9692_ptid
file_nm <- list.files(paste0(dataDir, '\\Splited_data_patid'))
# #a <- grep('\\w+a$', file_nm, value=T, perl=T)
# pt_match <- read.table(paste0(dataDir, '\\patient_match\\', f), sep=',')
# sep_1 <- function(i){
#     
# }
#read in the variable name file
vars <- read.table(paste0(resultDir, '\\var.csv'), header = F, stringsAsFactors = F)

vars <- gsub('(\\w+)(,$)', '\\1', vars[, 1], perl=T) #241

sep_123M <- function(i, HAE9692_ptid){
    resultDir1 <- paste0(resultDir, '\\sep_data_123M')
    if(!dir.exists(resultDir1)) dir.create(resultDir1, showWarnings = T, recursive = TRUE)
    
    dt <- read.table(paste0(dataDir, '\\Splited_data_patid\\', file_nm[i]), sep=',')
    names(dt) <- vars
    id <- dt$patient_id
    id_HAE_sp <- id[id %in% id_HAE]
    id_LHAE_sp <- id[id %in% id_LHAE]
    id_rest <- id[!id %in% id_LHAE]
    HAE_sp <- dt[id %in% id_HAE_sp,]
    LHAE_sp <- dt[id %in% id_LHAE_sp,]
    rest_sp <- dt[id %in% id_rest,]
    rm(dt)
    gc()
    save(HAE_sp, file=paste0(resultDir1, '\\HAE', i, '.RData'))
    save(LHAE_sp, file=paste0(resultDir1, '\\LHAE', i, '.RData'))
    save(rest_sp, file=paste0(resultDir1, '\\rest', i, '.RData'))
    
    #temp <- list(HAE_sp, LHAE_sp, rest_sp)
    # return(temp)
}


sfInit(parallel=TRUE, cpus=10, type='SOCK')
#sfSource("F:\\Jie\\Shire\\03_code\\subFunc_v3.R")
# sfSource("F:\\Jie\\Shire_follow_up\\02_Code\\Similarity\\funs_similarity.R")

# cat(file=traceFile, append=TRUE, 'n.simu simulations parallel sfExport running!\n')
sfExport('id_HAE', 'id_LHAE', 'vars', 'resultDir', 'dataDir', 'file_nm')
# sfExport('createCurve_v2', 'par_cv', 'par_lasso_simu')
# sfClusterEval(library("glmnet"))
# sfClusterEval(library("ROCR"))
# sfClusterEval(library("plyr"))
# sfClusterEval(library("dplyr"))
# sfClusterEval(library("snowfall"))
# sfClusterEval(library("caTools"))

temp <- sfClusterApplyLB(1:100, sep_123M)
#save(pred_ts_allSim, file=paste0(modelDir, '//pred_allSim.RData'))
sfStop()


#get the rbinded HAE, LHAE, rest
