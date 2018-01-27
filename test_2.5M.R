
library(xlsx)
library(ROCR)
library(plyr)
library(caret)
library(dplyr)
library(glmnet)
library(snow)
library(snowfall)
library(caTools)



dataDir <- "F:\\Dong\\share\\Rep_Samp"
resultDir <- "F:\\Jie\\Shire_follow_up\\03_Results\\"
n.split <- 51
n.simu <- 5
#cleaning and feature selection for 2.5million test data
#parallel on 51 splited files (around 50k rows in one file)
library(caret)
library(dplyr)
library(plyr)
library(snowfall)

BucketAge <- function(age)
{
    # thresholds are 30, 50 and 70
    
    bucket <- rep("", length(age))
    bucket[age <= 29] = "AGE<=29"
    bucket[(age >= 30) & (age <= 49)] = "30<=AGE<=49"
    bucket[(age >= 50) & (age <= 69)] = "50<=AGE<=69"
    bucket[age >= 70] = "AGE>=70"
    
    return (bucket)
}

# uses the non-zero values of the positive/HAE patients part
CapPercentile2 <- function(vec, Hae, percentile)
{
    if ((percentile < 0) | (percentile > 100))
        stop("Error! percentile must be between 0 and 100 (inclusive). ")
    positivePatPart <- vec[Hae==1]
    nonzeroPart <- positivePatPart[positivePatPart > 0]
    if (length(nonzeroPart) == 0)
        return (vec)
    threshold <- quantile(nonzeroPart, percentile/100, type=3)
    #vec[vec > threshold] <- threshold
    #   print(paste("In CapPercentile threshold:", threshold))
    #   print(paste("In CapPercentile max(vec):", max(vec)))
    return (threshold)
}


getCapValue <- function(resultDir){
    result <- read.csv(paste0(resultDir, 'CleanDataB4Spliting.csv'))
    temp <- result %>%
    {
    #       vars2Cap <- read.csv("vars2cap.csv", header=F, 
    #                             sep=",", colClasses="character")
    vars2Cap <- colnames(.)[grepl("_AFREQ", colnames(.))]
    #       print(paste("num of vars2Cap:", length(vars2Cap)))
    #       print("colnames(.):")
    #       print(colnames(.))
    
    dataLastStep <- .
    if (bTestMode)
        dataB4Capping <<- .
    #       apply(select(., one_of(vars2Cap[,1])), 
    #             2, PrintMaxAndPercentile, percentile=99)
    
#     #compute 99% percentile
#     dataMutated <- 
#         mutate_each(dataLastStep, funs(CapPercentile(., Hae=dataLastStep$HAE, percentile=99)), 
#                     one_of(vars2Cap))
    # print(paste("after mutate max(ER_AFREQ):", max(dataMutated$ER_AFREQ)))
    # print("after mutation")
    #       apply(select(dataMutated, one_of(vars2Cap[,1])), 
    #             2, PrintMaxAndPercentile, percentile=99)
#     dataMutated
    #get the capped value for each "_AFREQ" variables
    sapply(dataLastStep[, vars2Cap], function(x)CapPercentile2(x, .$HAE, percentile = 99))
    }
    const_vars <- names(temp)[unlist(lapply(temp, length))>1] #[1] "DIAG_52_AFREQ" "PRC_66_AFREQ"
    temp[match(const_vars, names(temp))] <- 0
    temp1 <- unlist(temp)
    return(temp1)
}
caps <- getCapValue(resultDir)
clean_2d5M <- function(f, dataDir, resultDir, bTestMode, caps){
    load(paste0(dataDir, '\\adj_ppv_samp_', f,'.RData')) #adj_ppv_samp
    adj_ppv_samp$GENDER <- ifelse(adj_ppv_samp$GENDERM==1, 'M', ifelse(adj_ppv_samp$GENDERU==1, "U", "F"))
    binonlyInits <- c("PRC_60", "PRC_64", "DIAG_32")
    data <- adj_ppv_samp %>%
        select(-matches('REGION|GENDERU|GENDERM|PATIENT_ID'))%>%
        #TEST     
        {
            if (bTestMode)
            {
                if (any(grepl("REGION", colnames(.))))
                    stop("Test Failed! Variables with 'REGION' in posOrgData not all removed.")
                .
            } else
                .
        } %>%
        #ENDTEST
        filter(GENDER!='U') %>%
        {
            vec <- rep(0, nrow(.))
            vec[.$GENDER=="M"] = 1
            mutate(., GENDER=vec)
        } %>%
        mutate(HAE=0) %>%
        #TEST
        {
            if (bTestMode)
            {
                # numerical? 
                if (!is.numeric(.$GENDER))
                    stop("Test Failed! GENDER in posOrgData is not converted to numeric.")
                # 2 levels? 
                lvls <- levels(factor(.$GENDER))
                if (length(lvls) != 2)
                    stop("Test Failed! There are more than two different values after converting GENDER to numeric in posOrgData.")
                # 0 and 1? 
                if ((lvls[1] != 0) | (lvls[2] != 1))
                    stop("Test Failed! After converting GENDER to numeric, the levels are not 0 and 1 in posOrgData.")
                .
            } else
                .
        } %>%
    #ENDTEST
        mutate(LOOKBACK_DAYS=lookback_days) %>%
        select(-lookback_days) %>%
    # removing missing gender
#     {
#     print("Removing GENDER=='U'..");cat("\n")
#     .
#     } %>%
#         filter(GENDER != "U") %>%
#         #TEST     
#         {
#             if (bTestMode)
#             {
#                 if (any(.$GENDER == "U"))
#                     stop("Test Failed! Observations with GENDER 'U' are not completely removed. ")
#                 .
#             } else
#                 .
#         } %>% #[1] 49997   240
#         #ENDTEST
    
    
    #     # remove negative records whose look-back durations don't match any positive
    #     mutate(lookback_matched=HAE) %>%
    # !!!!!!!!!
    # the HAE_PATIENT_ID may need to be computed
    # !!!!!!!!!
        {
            print("Removing negative records whose look-back durations don't match any positive\n")
            cat("\n")
            .
        } %>%
        {
            dataLastStep <- .
            load(paste(resultDir, "PosCleanDataB4Matching.RData", sep="")) #pos
            
            posLookbacks <- pos %>%
                select(LOOKBACK_DAYS)
            daysOneSide <- 29
            posLookbacks_Expanded <- 
                data_frame(LOOKBACK_DAYS=rep(0, (daysOneSide*2+1)*nrow(posLookbacks)))
            idx <- 1
            for (iPos in 1:nrow(posLookbacks))
            {
                vecThisPat <- 
                    (posLookbacks$LOOKBACK_DAYS[iPos]-29):
                    (posLookbacks$LOOKBACK_DAYS[iPos]+29)
                posLookbacks_Expanded$LOOKBACK_DAYS[idx:(idx+2*daysOneSide)] <-
                    vecThisPat
                idx <- idx + daysOneSide*2+1
            }
            posLookbacks_Unique <- unique(posLookbacks_Expanded)
            #       print("posLookbacks_Unique")
            #       print(posLookbacks_Unique)
            
            
            negLookbacks <- 
                filter(dataLastStep, HAE==0) %>%
                select(LOOKBACK_DAYS) %>%
                mutate(negID=(1:sum(dataLastStep$HAE==0)))
            #       print("negLookbacks")
            #       print(negLookbacks)
            
            negIDsLookbackMatched <- 
                inner_join(posLookbacks_Unique, negLookbacks, by="LOOKBACK_DAYS") %>%
                select(negID)
            #       print("negIDsLookbackMatched")
            #       print(negIDsLookbackMatched)
            #       print("dim(negIDsLookbackMatched)")
            #       print(dim(negIDsLookbackMatched))
            #       print(paste("nNegs removed not matching pos: ", 
            #                   sum(dataLastStep$HAE==0)-nrow(negIDsLookbackMatched)))
            
            #       filter(dataLastStep, 
            #              (HAE==1) | (PATIENT_ID %in% negIDsLookbackMatched$PATIENT_ID))
            bind_rows(filter(., HAE==1), 
                      filter(., HAE==0)[negIDsLookbackMatched$negID, ])
            
        } %>%
    
    
        # remove age <= 12
        {
            print("Removing AGE<=12..");cat("\n")
            .
        } %>%
        filter(AGE > 12) %>%
        #TEST     
        {
            if (bTestMode)
            {
                if (any(.$AGE <= 12))
                    stop("Test Failed! Observations with AGE <= 12 are not completely removed. ")
                .
            } else
                .
        } %>% #[1] 42692   240
        #ENDTEST
        # remove all-zero patients
        {
            dataLastStep <- .
            sumCol <- apply(select(., matches("_FLAG")), 1, sum)
            nonAllZeroPats <- 
                mutate(., allzeros=(sumCol == 0)) %>%
                filter(allzeros==F) %>%
                select(-allzeros)
            print(paste("Number of allzero patients:", nrow(dataLastStep)-nrow(nonAllZeroPats)))
            nonAllZeroPats
        } %>% #636 #[1] 42056   240
        # binary-only variables: keep only FLAG
        {
            print("Removing FREQ and AFREQ for binary-only variables..");cat("\n")
            .
        } %>%
        {
            vars2Drop_BinOnly <- 
                c(paste(binonlyInits, "FREQ", sep="_"), 
                  paste(binonlyInits, "AFREQ", sep="_"))
            select(., -one_of(vars2Drop_BinOnly))
        } %>%
        #TEST     
        {
            if (bTestMode)
            {
                IDs_AFREQAndOther <- 
                    (!grepl("_FREQ", colnames(.))) &
                    (!grepl("FLAG", colnames(.)))
                vars_AFREQAndOther <<- .[, IDs_AFREQAndOther]
                
                if ((any(paste(binonlyInits, "FREQ", sep="_") %in% colnames(.))) |
                    (any(paste(binonlyInits, "AFREQ", sep="_") %in% colnames(.))))
                    stop("Test Failed! FREQ and AFREQ variables are not completely removed for binary-only variables. ")
                .
            } else
                .
        } %>%
        #ENDTEST
        
        # other variables: remove FLAG and FREQ
        {
            print("Removing FLAG and FREQ for other variables..");cat("\n")
            .
        } %>%
        {
            dataLastStep <- .
            
            varsKept_BinOnly <- paste(binonlyInits, "FLAG", sep="_")
            nonBinVars <- 
                select(., -one_of(varsKept_BinOnly)) %>%
                select(., -matches("_FLAG")) %>%
                select(., -matches("_FREQ")) 
            
            select(dataLastStep, one_of(varsKept_BinOnly)) %>%
                bind_cols(nonBinVars)
        } %>%
        #TEST     
        {
            if (bTestMode)
            {
                # no FREQ and FLAG (except the 3 bin-only)
                if (any(grepl("_FREQ", colnames(.))))
                    stop("Test Failed! FREQ variables are not completely removed.")
                nonBinOnlyVars <- colnames(.)
                nonBinOnlyVars <- nonBinOnlyVars[!(nonBinOnlyVars %in% paste(binonlyInits, "FLAG", sep="_"))]
                if (any(grepl("FLAG", nonBinOnlyVars)))
                    stop("Test Failed! Non-binary-only variables still have FLAG.")
                
                # the original AFREQs and other variables from the last step are intact
                if (!identical(vars_AFREQAndOther, .[, !(colnames(.) %in% paste(binonlyInits, "FLAG", sep="_"))]))
                    stop("Test Failed! AFREQ variables or oter variables without FLAG/FREQ/AFREQ are changed accidentally.")
                
                .
            } else
                .
        } %>%
        #ENDTEST
    # age in buckets
    {
    print("Putting AGE into buckets..");cat("\n")
    .
    } %>%
        mutate(., ageBucket=BucketAge(AGE)) %>%
        {
            dataLastStep <- .
            categories <- levels(factor(dataLastStep$ageBucket))
            for (iLevel in 1:length(categories))
            {
                print(paste("level", iLevel, ":", categories[iLevel]))
                dataLastStep[, categories[iLevel]] <- 
                    as.numeric((dataLastStep$ageBucket == categories[iLevel]))
            }
            select(dataLastStep, -one_of(c("ageBucket", "AGE")))
        } %>%
        {
            cat("\n")
            .
        } %>%
        #TEST     
        {
            if (bTestMode)
            {
                if (any(colnames(.) == "AGE"))
                    stop("Test Failed! The original 'AGE' variable still exists after it's broken into buckets.")
                
                # every age bucket is binary and coded with 0 and 1
                ageVars <- colnames(.)[grepl("AGE", colnames(.))]
                for (iVar in 1:length(ageVars))
                {
                    lvls <- levels(factor(.[[ageVars[iVar]]]))
                    if (length(lvls) != 2)
                        stop(paste("Test Failed! The AGE bucket ", 
                                   ageVars[iVar], 
                                   " doesn't have 2 levels. "))
                    if ((lvls[1] != 0) | (lvls[2] != 1))
                        stop(paste("Test Failed! The 2 levels of the AGE bucket ", 
                                   ageVars[iVar], 
                                   " has incorrect values. "))
                }
                
                .
            } else
                .
        } 
    
    
    
#     # remove constant columns
#     {
#     dataLastStep <- .
#     levels <- sapply(., function(x)length(table(x)))
#     temp2 <- .[, levels>1]
#     print(paste("Number of constant variables:", ncol(dataLastStep)-ncol(temp2)))
#     temp2
#     } %>%
#         
#         #how about the collinear variables?
#         #     i.	Collinear variable removal (for example, whether a variable needs to be removed is determined 
#         #           using only the training + validation data, then the test data need to remove the corresponding variables);
#         
#     {
#         print("Removing collinear variables..");cat("\n")
#         .
#     } %>%
#     {
#         file_corr <- file(paste(resultDir, "colinear_vars.txt", sep=""), "w")
#         threshold <- 0.8
#         if (bTestMode)
#             collThreshold <<- threshold
#         writeLines(paste("Correlation threshold:", threshold), file_corr)
#         dataLastStep <- data.matrix(.)
#         # cor matrix
#         corrMat <- cor(dataLastStep, dataLastStep)
#         corrMat <- corrMat - diag(ncol(corrMat))
#         # remove one variable of the correlated pair
#         vars2Remove <- NULL
#         for (iVar in 1:(ncol(dataLastStep)-1))
#         {
#             thisVar <- colnames(dataLastStep)[iVar]
#             if (thisVar %in% vars2Remove)
#                 next
#             for (jVar in (iVar+1):ncol(dataLastStep))
#             {
#                 if (colnames(dataLastStep)[jVar] %in% vars2Remove)
#                     next
#                 if (abs(corrMat[iVar, jVar]) >= threshold)
#                 {
#                     vars2Remove <- c(vars2Remove, colnames(dataLastStep)[jVar])
#                     writeLines(paste(colnames(dataLastStep)[iVar], 
#                                      "is correlated with", 
#                                      colnames(dataLastStep)[jVar], 
#                                      ":", corrMat[iVar, jVar]), file_corr)
#                 }
#                 
#             }
#         }
#         
#         writeLines("vars2Remove due to collinearity:", file_corr)
#         writeLines(paste(vars2Remove,collapse=","), file_corr)
#         
#         close(file_corr)
#         if (!is.null(vars2Remove))
#             select(., -one_of(vars2Remove))
#         else
#             .
#     } %>%
#         #TEST     
#     {
#         if (bTestMode)
#         {
#             dataLastStep <- data.matrix(.)
#             corrMat <- cor(dataLastStep, dataLastStep)
#             corrMat <- corrMat - diag(ncol(corrMat))
#             if (max(abs(corrMat)) >= collThreshold)
#                 stop("Test Failed! Colliear variables are not completely removed.")
#             .
#         } else
#             .
#     } %>%
#         #ENDTEST
        
        # cap variables with extreme values selected by Nadea
        #     ii.	Cap extreme values;
        
#     {
#         print("Capping extreme values..");cat("\n")
#         .
#     } %>%
#     {
#         #       vars2Cap <- read.csv("vars2cap.csv", header=F, 
#         #                             sep=",", colClasses="character")
#         vars2Cap <- colnames(.)[grepl("_AFREQ", colnames(.))]
#         #       print(paste("num of vars2Cap:", length(vars2Cap)))
#         #       print("colnames(.):")
#         #       print(colnames(.))
#         
#         dataLastStep <- .
#         if (bTestMode)
#             dataB4Capping <<- .
#         #       apply(select(., one_of(vars2Cap[,1])), 
#         #             2, PrintMaxAndPercentile, percentile=99)
#         
#         # compute 99% percentile
#         dataMutated <- 
#             mutate_each(dataLastStep, funs(CapPercentile3(., Hae=dataLastStep$HAE, percentile=99)), 
#                         one_of(vars2Cap))
#         # print(paste("after mutate max(ER_AFREQ):", max(dataMutated$ER_AFREQ)))
#         # print("after mutation")
#         #       apply(select(dataMutated, one_of(vars2Cap[,1])), 
#         #             2, PrintMaxAndPercentile, percentile=99)
#         dataMutated
#     } %>%
#         #TEST     
#     {
#         if (bTestMode)
#         {
#             afreqVars <- colnames(dataB4Capping)[grepl("_AFREQ", colnames(dataB4Capping))]
#             for (iVar in 1:length(afreqVars))
#             {
#                 positivePart <- dataB4Capping[[afreqVars[iVar]]][dataB4Capping$HAE==1]
#                 nonzeroPart <- positivePart[positivePart > 0]
#                 if (length(nonzeroPart) == 0)
#                     next
#                 threshold <- quantile(nonzeroPart, 99/100, type=3)
#                 currentVar <- afreqVars[iVar]
#                 if (any(.[[afreqVars[iVar]]] > threshold))
#                     stop(paste("Test Failed! Variable", afreqVars[iVar], "is not capped properly."))
#             }
#             if (F)
#                 stop("Test Failed! The final rule of extreme value capping is not determined yet. ")
#             .
#         } else
#             .
#     }
#     
#     
    rm(adj_ppv_samp)
  #  data$HAE <- 0
        save(data, file=paste0(resultDir, 'test2d5M_split', f,'.RData'))
}


#parallel on n.folds cv
sfInit(parallel=TRUE, cpus=40, type='SOCK')
#sfSource("F:\\Jie\\Shire\\03_code\\subFunc_v3.R")
#sfSource("F:\\Jie\\Shire_follow_up\\02_Code\\clean_split_featureSelection_lasso.R")

#sfExport( 'crit', 'traceFile', 'lambda_seq', 'modelDir', 'dataDir', 'norm')
sfExport('clean_2d5M', 'BucketAge')
sfClusterEval(library("glmnet"))
sfClusterEval(library("ROCR"))
sfClusterEval(library("plyr"))
sfClusterEval(library("dplyr"))
#cat(file=traceFile, append=TRUE, 'simulation:', i, ' parallele start running!\n')

ms_fromCV <- sfClusterApplyLB(1:n.split, clean_2d5M, dataDir, resultDir, bTestMode=F)
sfStop()

#prepare the normalized data for each split using metrics from training + validation

#fetch the normalizing metrics before
metrics_allSim <- ldply(lapply(1:5, function(sim){
    metrics <- read.csv(paste0(resultDir, 'norm_matrics_sim', sim, '.csv'))
}
), rbind)

metrics_avg <- aggregate(.~X, metrics_allSim, mean)
dim(metrics_avg)

#normalize the 2.5Million data
norm_2d5m <- function(f, resultDir)
{
    load(paste0(resultDir, 'test2d5M_split', f,'.RData')) #data
    names(data)[83: 86] <- c("X30..AGE..49" ,  "X50..AGE..69",   "AGE..29"  ,     "AGE..70" )
    temp <- lapply(names(data), function(v){
        var <- as.numeric(as.matrix(data[, v]))
        min_range <-  metrics_avg[match(v, metrics_avg$X),]
        if(all(is.na(min_range))){
            stop("cannot find the variables:", v, " in metrics_avg.csv\n")
        }
        var2 <- (var-as.numeric(min_range[2]))/as.numeric(min_range[3])
        return(var2)
    })
    var_list <- names(data)
    rm(data)
    data2 <- as.data.frame(t(ldply(temp, quickdf)))
    names(data2) <- var_list
    save(data2, file=paste0(resultDir, 'test2d5M_norm_split', f,'.RData'))
    return(data2)
    
}


#parallel on n.folds cv
sfInit(parallel=TRUE, cpus=40, type='SOCK')
#sfSource("F:\\Jie\\Shire\\03_code\\subFunc_v3.R")
#sfSource("F:\\Jie\\Shire_follow_up\\02_Code\\clean_split_featureSelection_lasso.R")

sfExport( 'metrics_avg')
sfExport('norm_2d5m')
sfClusterEval(library("glmnet"))
sfClusterEval(library("ROCR"))
sfClusterEval(library("plyr"))
sfClusterEval(library("dplyr"))
#cat(file=traceFile, append=TRUE, 'simulation:', i, ' parallele start running!\n')

result <- sfClusterApplyLB(1:n.split, norm_2d5m, resultDir)
sfStop()


#generate the total model for 2.5M (averaging and extend to a sequence)
#normed data


#non-normed data

#apply the optimum model to the 2.5M (splited into 51 files)
pred_2d5m_par <- function(f, resultDir, sim_sel, norm, wt)
{
    t0 <- proc.time()
    #save(lambda_seq, optN, total_model, file=paste0(modelDir,"//total_model_simu", i, ".RData"))
    #for non-normed data/normed data
    modelDir <- paste0(resultDir, 'Lasso//wt=', wt, '_norm=', norm)
    #save(data2, file=paste0(resultDir, 'test2d5M_norm_split', f,'.RData'))
    load(paste0(resultDir, 'test2d5M_norm_split', f,'.RData'))
    data_matrix <- model.matrix(HAE~.
                                             , data=data2)[,-1]     # removes intercept term
    rm(data2)
    load(paste0(modelDir,"//total_model_simu", sim_sel, ".RData"))
    pred<- predict(total_model, data_matrix, type="response")[,optN]
    save(pred, file=paste0(modelDir, "//pred_2d5m_split", f, '.RData'))
    rm(data_matrix)
    cat('time used:', (proc.time()-t0)[3]/60, ' min!\n')
    return(pred)

}


#parallel on n.folds cv
sfInit(parallel=TRUE, cpus=20, type='SOCK')
#sfSource("F:\\Jie\\Shire\\03_code\\subFunc_v3.R")
#sfSource("F:\\Jie\\Shire_follow_up\\02_Code\\clean_split_featureSelection_lasso.R")

#sfExport( 'metrics_avg')
sfExport('pred_2d5m_par')
sfClusterEval(library("glmnet"))
sfClusterEval(library("ROCR"))
sfClusterEval(library("plyr"))
sfClusterEval(library("dplyr"))
#cat(file=traceFile, append=TRUE, 'simulation:', i, ' parallele start running!\n')

result_nn <- sfClusterApplyLB(1:n.split, pred_2d5m_par, resultDir, 1, F, 1/200)
result_nm <- sfClusterApplyLB(1:n.split, pred_2d5m_par, resultDir, 1, T, 1/200)

sfStop()

fetch_pred_1kPos <- function(resultDir, norm, wt, sim_sel){
    temp <- lapply(1:n.simu, function(i){
        if(norm==T){
            load(paste0(resultDir, 'splitedDataB4ModelTsNormSim', i, '.RData')) 
            ts <- as.data.frame(ts_nm)
            rm(ts_nm)
        }else{
            load(paste0(resultDir, 'splitedDataB4ModelTsSim', i, '.RData'))
        }
        resp <- ts$HAE
        return(resp)
    })  
    resp <- unlist(temp)
    
    modelDir <- paste0(resultDir, 'Lasso//wt=', wt, '_norm=', norm)
    load(paste0(modelDir, '//pred_allSim.RData')) #pred_ts_allSim
    pred <- pred_ts_allSim[[sim_sel]]
    
    pred_pos <- pred[resp==1] #1087
}

msOnTest_sep_v2 <- function(pred, response, recall_tar){
    #pred <- apply(pred, 1, mean, na.rm=T)
    predobj <- prediction(pred, response)
    #add plot
    perf <- performance(predobj, 'ppv', 'sens') # added by jie for recall-precision plot.
    recall <- perf@x.values[[1]]
    precision <- perf@y.values[[1]]
    auc <- performance(predobj, 'auc')@y.values[[1]]
    rec_prec <- data.frame(recall=recall, precision=precision)
    rec_prec_omitMiss <- rec_prec[complete.cases(rec_prec),]
    aupr <- trapz(rec_prec_omitMiss$recall, rec_prec_omitMiss$precision)
    bucket <- cut(recall, breaks=seq(0, 1, 0.01), include.lowest=T,right=F)
    rec_prec_byBucket <- aggregate(rec_prec, by=list(bucket), function(i)mean(i, na.rm=T))
    #write.csv(rec_prec_byBucket, paste('Curve_dong.csv', sep=''), 
    #         row.names=F, quote=T)
    #plot
    #pdf(file=paste('recall-precision curve on test.pdf', sep=''))
    #plot(recall, precision, type='l', main=paste('recall-precision curve'))
    #plot(perf)
    #dev.off()
    
    ##in simulation
    temp4 <- unlist(lapply(recall_tar, function(X){
        #idx <- sample(rep(which(abs(rec_prec[, 1]-X)==min(abs(rec_prec[, 1]-X), na.rm=T)), 2), 1)
        idx=which(abs(rec_prec[, 1]-X)==min(abs(rec_prec[, 1]-X), na.rm=T))[1]
        prec_sel <- rec_prec[idx, 2]
        return(prec_sel)
    }))    
    
    ##end
    ms <- c(auc, aupr, temp4)
    names(ms) <- c('auc',"aupr", paste("PPV(recall=", recall_tar,')', sep=''))
    
    return(list(ms=ms, curve=rec_prec_byBucket, rec_prec=rec_prec))
    
}


summarize_perf_2d5m <- function(resultDir, norm, wt, sim_sel, target_recall)
{
    modelDir <- paste0(resultDir, 'Lasso//wt=', wt, '_norm=', norm)
    load(paste0(modelDir, '//pred_allSim.RData')) #pred_ts_allSim
    pred_pos <- fetch_pred_1kPos(resultDir, norm, wt, sim_sel)
    temp <- lapply(1:n.split, function(f){
        load(paste0(modelDir, "//pred_2d5m_split", f, '.RData')) #pred
        return(pred)
    })
    pred_neg <- unlist(temp) #[1] 1865578
    pred <- c(pred_pos, pred_neg)
    response <- c(rep(1, length(pred_pos)), rep(0, length(pred_neg)))
    temp1 <- msOnTest_sep_v2(pred, response, target_recall)
    #plot
    sensitivity <- temp1$rec_prec[, 1]
    PPV <- temp1$rec_prec[, 2]
    pdf(paste0(modelDir, '\\recall-precision curve on 2.5Mil norm=', norm, '.pdf'))
    plot(sensitivity, PPV, type='l', main=paste('recall-precision curve(PR CURVE)'))
    #plot(perf)
    dev.off()
    return(temp1)
}



target_recall <- seq(0.5, 0.05, -0.05)
perf_nm <- summarize_perf_2d5m(resultDir, norm=T, wt=1/200, sim_sel=1, target_recall=target_recall)
perf_nn <- summarize_perf_2d5m(resultDir, norm=F, wt=1/200, sim_sel=1, target_recall=target_recall)

perf_2d5m <- as.data.frame(cbind(perf_nn$ms, perf_nm$ms))
names(perf_2d5m) <- c("without norm", 'with norm')
write.csv(perf_2d5m, paste0(resultDir, 'Lasso\\performance_on2.5Mil.csv'), row.names=T)


