createCurve_v2 <- function(resp, pred, recall_tar, crit_nm){
    predobj <- prediction(pred, resp)
    #add plot
    perf <- performance(predobj, 'prec', 'rec') # added by jie for recall-precision plot.
    recall <- perf@x.values[[1]]
    precision <- perf@y.values[[1]]
    auc <- performance(predobj, 'auc')@y.values[[1]]
    
    rePrec <- cbind(recall, precision)
    rec_prec <- data.frame(recall=recall, precision=precision)
    rec_prec_omitMiss <- rec_prec[complete.cases(rec_prec),]
    aupr <- trapz(rec_prec_omitMiss$recall, rec_prec_omitMiss$precision)
    bucket <- cut(recall, breaks=seq(0, 1, 0.005), include.lowest=T,right=F)
    rec_prec_byBucket <- aggregate(rePrec, by=list(bucket), function(i)mean(i, na.rm=T))
    
    ##in simulation
    #recall<- c(0.05, 0.1, 0.25, 0.5)
    
    temp4 <- unlist(lapply(recall_tar, function(X){
        idx <- which(abs(rePrec[, 1]-X)==min(abs(rePrec[, 1]-X), na.rm=T))[1]
        rec_sel <- rePrec[idx, 1]
        prec_sel <- rePrec[idx, 2]
        temp <- c(rec_sel, prec_sel)
        names(temp) <- NULL
        return(temp)
    }))  
    temp5 <- unlist(lapply(recall_tar, function(X){
        idx <- which(abs(rePrec[, 1]-X)==min(abs(rePrec[, 1]-X), na.rm=T))[1]
        prec_sel <- rePrec[idx, 2]
        return(prec_sel)
    }))
    ppv_avg <- mean(temp5, na.rm=T)
    
    resp_pred <- ifelse(pred>=0, 1, 0)
    con_tb <- table(resp_pred, resp)
    if(nrow(con_tb)==1){
        ifp0  <- 0
    }else{
        ifp0 <- con_tb[2,1]
        
    }
    itp <- sum(resp)*temp4[1]
    ifp <- (1-temp4[2])/temp4[2]*itp
    
    #if(cutoff_forDR==0){
    #hui added
    
    #   resp_pred <- ifelse(pred>=0, 1, 0)
    #  con_tb <- table(resp_pred, resp)
    # ifp <- con_tb[2,1]
    ##end 
    
    #}else{
    #   itp <- sum(resp)*temp4[1]
    #  ifp <- (1-temp4[2])/temp4[2]*itp
    #}
    #return(c(temp4[2], ifp, ifp0))
    if(crit_nm=="ppv_0.05recall"){
        return(temp4[2])
        
    }else if(crit_nm=="auc"){
        return(auc)
    }else if(crit_nm=="aupr0.01_0.1"){
        return(ppv_avg)
    }else{
        stop('wrong criteria name!\n')
    }
    
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


split_cv4F <- function(dataDir='F:\\Jie\\Shire_follow_up\\03_Results\\for200+a\\'
                     , n.simu=5
                     , n.folds=5
                     , norm=T
                     , wt=1/200
                     , crit_nm='auc')
{   
    
    dataDir <- paste0(dataDir, 'Similarity_', crit_nm, '//wt=', wt, '_norm=', norm)
    if(!dir.exists(dataDir)) dir.create(dataDir, showWarnings = T, recursive = TRUE)
    

    for(i in 1:n.simu){
        # save(tr_matrix_sel, tr_resp_sel, ts_matrix, response_ts, file=paste0(modelDir, '//dataFor_FModel_sim', i, '.RData'))
        
        load(paste0(dataDir, '//dataFor_FModel_sim', i, '.RData'))
        rm(ts_matrix)
        for(j in 1:n.folds){
            set.seed(20)
            foldid <- nrow(tr_matrix_sel)
            foldid[tr_resp_sel==1] <- sample(rep(1:n.folds, length=length(which(tr_resp_sel==1))))
            foldid[tr_resp_sel==0] <- sample(rep(1:n.folds, length=length(which(tr_resp_sel==0))))
            table(tr_resp_sel, foldid)
            cv_ts <- tr_matrix_sel[foldid==j, ]
            cv_tr <- tr_matrix_sel[foldid!=j, ]
            cv_tr_resp <- tr_resp_sel[foldid!=j]
            cv_ts_resp <- tr_resp_sel[foldid==j]
            cv_trTs <- list(cv_tr=cv_tr, cv_ts=cv_ts, cv_tr_resp=cv_tr_resp, cv_ts_resp=cv_ts_resp)
            save(cv_trTs, file=paste0(dataDir, '\\trVlTs4FModelSim', i, '_cv', j, '.RData'))   
            
            
        }
    }
    
    
    cat("\nsplited cv data saved successfully!\n")
    
}



par_cv <- function(j, i, wt)
{
    #load cv data
    #cat(file=traceFile, append=T, '1\n')
    
    if(norm==T){
        
        load(paste0(dataDir, 'trVlNormSim', i, '_cv', j, '.RData'))   
    }else{
        load(paste0(dataDir, 'trVlSim', i, '_cv', j, '.RData'))    
        
    }
    #cv_trTs$cv_tr
    #cv_trTs$cv_ts
    cv_data <- bind_rows(cv_trTs$cv_tr, cv_trTs$cv_ts)
    #cv_data <- cv_data[, -match("LOOKBACK_DAYS", colnames(cv_data))] #remove lookback_days
    cv_matrix <- model.matrix(HAE~., data=cv_data)
    rm(cv_data)
    #cat(file=traceFile, append=T, '2\n')
    
    cv_tr_matrix <- cv_matrix[1:nrow(cv_trTs$cv_tr), ]
    cv_test_matrix <- cv_matrix[-(1:nrow(cv_trTs$cv_tr)), ]
    resp_cvTr <- cv_trTs$cv_tr$HAE
    resp_cvTs <- cv_trTs$cv_ts$HAE
    #cat(file=traceFile, append=T, '3\n')
    
    start_cv <- proc.time()
    cat(file=traceFile, append=TRUE, 'sim-', i, ' fold-', j, ' start!\n')
    fit_lasso<- glmnet(cv_tr_matrix, resp_cvTr
                       ,lambda=lambda_seq
                       , family="binomial"
                       , alpha=1
                       ,weights = ifelse(resp_cvTr==1, 1, wt)
                       ,standardize=F)
    #cat(file=traceFile, append=T, '4\n')
    
    test_pred<- predict(fit_lasso, cv_test_matrix, type="response")
    rm(cv_matrix, cv_tr_matrix, cv_test_matrix)
    cat(file=traceFile, append=T, 'sim-', i, ' fold-', j, ' lasso and pred and rm object end!\n')
    test_pred_avg<- apply(test_pred, 2, function(x){createCurve_v2(resp_cvTs , x, crit, crit_nm=crit_nm)})
    test_pred_avg<- c(test_pred_avg , rep(NA , length(lambda_seq) - length(test_pred_avg))) # some small lambda may not be reached
    optN_cvi <- which(test_pred_avg==max(test_pred_avg, na.rm = T))
    if(length(optN_cvi>1)) optN_cvi <- sample(optN_cvi, 1)
    optMs_cvi <- max(test_pred_avg, na.rm = T)
    optLmd_cvi <- lambda_seq[optN_cvi]
    cat('sim-', i, ' fold-', j, 'optN_cvi:', optN_cvi, 'optLmd_cvi', optLmd_cvi, 'maximum ms-', optMs_cvi, ' \n')
    cat(file=traceFile, append=TRUE, 'sim-', i, ' fold-', j, 'optN_cvi:', optN_cvi, 'optLmd_cvi', optLmd_cvi,optLmd_cvi, 'maximum ms-', optMs_cvi, ' \n')
    cat('sim-', i, ' fold-', j, ' end!\n')
    cat(file=traceFile, append=TRUE, 'sim-', i, ' fold-', j, ' time:', (proc.time()-start_cv)[3]/60, 'min!\n')
    cat('sim-', i, ' fold-', j, ' time:', (proc.time()-start_cv)[3]/60, 'min!\n')
    cat(file=traceFile, append=TRUE, 'sim-', i, ' fold-', j, ' end time:', date(), '!\n\n')
    cat('sim-', i, ' fold-', j, ' end time:', date(), '!\n\n')
    
    return(test_pred_avg)
    
    
}

par_lasso_simu <- function(i, norm, Btest)
{
    #load trVl data
    if(norm==T){
        load(paste0(dataDir, 'splitedDataB4ModelTrVlNormSim', i, '.RData')) 
        load(paste0(dataDir, 'splitedDataB4ModelTsNormSim', i, '.RData')) 
        trVl <- as.data.frame(trVl_nm)
        ts <- as.data.frame(ts_nm)
        rm(trVl_nm, ts_nm)
    }else{
        load(paste0(dataDir, 'splitedDataB4ModelTrVlSim', i, '.RData'))
        load(paste0(dataDir, 'splitedDataB4ModelTsSim', i, '.RData'))
        
    }
    # Calculating initial lambda and the lambda sequence
    trTs <- bind_rows(trVl, ts)
    #trTs <- trTs[, -match("LOOKBACK_DAYS", colnames(trTs))] #remove lookback_days
    trTs_matrix<- model.matrix(HAE~.
                               , data=trTs)[, -1]     # removes intercept term
    tr_matrix <- trTs_matrix[1:nrow(trVl), ]
    ts_matrix <- trTs_matrix[-(1:nrow(trVl)), ]
    response_tr <- trVl$HAE
    response_ts <- ts$HAE
    save(response_ts, file=paste0(modelDir, '//resp_ts_sim', i, '.RData'))
    
    initial_lambda<-glmnet(x=tr_matrix
                           , y=response_tr
                           , family="binomial"
                           , alpha=1
                           , standardize=F
                           , weights=ifelse(response_tr==1, 1, wt)
    )$lambda  # calculating the initial lambda
    lambda_seq<- c(initial_lambda[-length(initial_lambda)] 
                   , seq(initial_lambda[length(initial_lambda)] , 0 , length=100)
    ) # get a length=100 descending sequence from initial lambda to 0
    if(Btest==T){
        lambda_seq <- lambda_seq[1:5]
    }
    
    rm(trTs_matrix, trVl, trTs)
    
    #n.folds cross validation
    cat(file=traceFile, append=T, 'cv on simulation:', i, ' start!\n')
    
    #parallel on n.folds cv
    sfInit(parallel=TRUE, cpus=n.folds, type='SOCK')
    #sfSource("F:\\Jie\\Shire\\03_code\\subFunc_v3.R")
    sfSource("F:\\Jie\\Shire_follow_up\\02_Code\\clean_split_featureSelection_lasso.R")
    cat(file=traceFile, append=TRUE, 'simulation:', i, ' sfExport running!\n')
    sfExport( 'crit', 'traceFile', 'lambda_seq', 'modelDir', 'dataDir', 'norm', "crit_nm")
    sfExport('createCurve_v2', 'par_cv')
    sfClusterEval(library("glmnet"))
    sfClusterEval(library("ROCR"))
    sfClusterEval(library("plyr"))
    sfClusterEval(library("dplyr"))
    sfClusterEval(library("caTools"))
    
    cat(file=traceFile, append=TRUE, 'simulation:', i, ' parallele start running!\n')
    
    ms_fromCV <- sfClusterApplyLB(1:n.folds, par_cv, i, wt)
    sfStop()
    ms_fromCV_df <- ldply(ms_fromCV, quickdf)
    
    ms_wtj <- apply(ms_fromCV_df, 2, mean,na.rm=T)
    optN <- which(ms_wtj==max(ms_wtj, na.rm=T))
    if(length(optN)>1){
        optN <- sample(optN, 1)
    }
    optLmd <- lambda_seq[optN]
    maxMs <- ms_wtj[optN]
    optMs_1wt <-  c(idx=optN, ms=maxMs)
    
    cat(file=traceFile, append=TRUE, "simulation-", i,' optimum lambda idx-', optN, ' optimum lambda-', optLmd, ' maximum(corresponding) ms-', maxMs, ' !\n')
    cat("simulation-", i,' optimum lambda idx-', optN, ' optimum lambda-', optLmd, ' maximum(corresponding) ms-', maxMs, ' !\n')
    #get the optimum model on training+validation and save the model
    total_model<- glmnet(x=tr_matrix
                         , y=response_tr
                         , lambda=lambda_seq
                         , family="binomial"
                         , alpha=1
                         , standardize=F
                         , weights=ifelse(response_tr==1, 1, wt)
    )
    save(lambda_seq, optN, total_model, file=paste0(modelDir,"//S_total_model_simu", i, ".RData"))
    cat(file=traceFile, append = T, 'apply optimum model to the whole training+validation data and save the total model end!\n')
    cat('apply optimum model to the whole training+validation data end!\n')
    
    model_coef<- total_model$beta[, as.vector(optN)]
    odds_ratio<- exp(model_coef)[model_coef != 0]
    non_zero_var<- names(model_coef)[as.vector(model_coef != 0)]
    re_model_var<- c('HAE', non_zero_var)
    #model<- data.frame(coefficient=model_coef[model_coef!=0], odds_ratio, p_value=p_value[match(non_zero_var, names(p_value))])
    model_output <- data.frame(variable=non_zero_var, 
                               coefficient=as.vector(model_coef)[as.vector(model_coef) != 0], 
                               odds_ratio=odds_ratio)
    write.csv(model_output, file = paste0(modelDir, '//S_model_coefficient_sim', i, '.csv'))
    
    #apply the optimum weight and lambda to the training+validation data
    pred_trVl <- predict(total_model, tr_matrix, type='response')[, optN]
    save(pred_trVl, file=paste0(modelDir, '//S_pred_trVl_sim', i, '.RData'))
    #performance on test data
    
    
    #apply the optimum weight and lambda to the test data
    pred_ts<- predict(total_model, ts_matrix, type="response")[,optN]
    save(pred_ts, file=paste0(modelDir, '//S_pred_ts_sim', i, '.RData'))
    #select FP & P for training+validation
    idx <- which((pred_trVl>=0 & response_tr==0) | response_tr==1)
    tr_matrix_sel <- tr_matrix[idx, ]
    tr_resp_sel <- response_tr[idx]
    save(tr_matrix_sel, tr_resp_sel, ts_matrix, response_ts, file=paste0(modelDir, '//dataFor_FModel_sim', i, '.RData'))
    
    cat(file=traceFile, append=TRUE, 'simulation ', i, ': test_pred end!\n') #added by Jie
    return(pred_ts)    
    
}




par_cv_4F <- function(j, i, wt)
{
    #load cv data
    #cat(file=traceFile, append=T, '1\n')
    # cv_trTs <- list(cv_tr=cv_tr, cv_ts=cv_ts, cv_tr_resp=cv_tr_resp, cv_ts_resp=cv_ts_resp)
    load(paste0(modelDir, '\\trVlTs4FModelSim', i, '_cv', j, '.RData'))
    
    #cv_trTs$cv_tr
    #cv_trTs$cv_ts
    cv_tr_matrix <- cv_trTs$cv_tr
    cv_test_matrix <- cv_trTs$cv_ts
    resp_cvTr <- cv_trTs$cv_tr_resp
    resp_cvTs <- cv_trTs$cv_ts_resp
    #cat(file=traceFile, append=T, '3\n')
    
    start_cv <- proc.time()
    cat(file=traceFile, append=TRUE, 'sim-', i, ' fold-', j, ' start!\n')
    fit_lasso<- glmnet(cv_tr_matrix, resp_cvTr
                       ,lambda=lambda_seq
                       , family="binomial"
                       , alpha=1
                       ,weights = ifelse(resp_cvTr==1, 1, wt)
                       ,standardize=F)
    #cat(file=traceFile, append=T, '4\n')
    
    test_pred<- predict(fit_lasso, cv_test_matrix, type="response")
    rm(cv_matrix, cv_tr_matrix, cv_test_matrix)
    cat(file=traceFile, append=T, 'sim-', i, ' fold-', j, ' lasso and pred and rm object end!\n')
    test_pred_avg<- apply(test_pred, 2, function(x){createCurve_v2(resp_cvTs , x, crit, crit_nm=crit_nm)})
    test_pred_avg<- c(test_pred_avg , rep(NA , length(lambda_seq) - length(test_pred_avg))) # some small lambda may not be reached
    optN_cvi <- which(test_pred_avg==max(test_pred_avg, na.rm = T))
    if(length(optN_cvi>1)) optN_cvi <- sample(optN_cvi, 1)
    optMs_cvi <- max(test_pred_avg, na.rm = T)
    optLmd_cvi <- lambda_seq[optN_cvi]
    cat('sim-', i, ' fold-', j, 'optN_cvi:', optN_cvi, 'optLmd_cvi', optLmd_cvi, 'maximum ms-', optMs_cvi, ' \n')
    cat(file=traceFile, append=TRUE, 'sim-', i, ' fold-', j, 'optN_cvi:', optN_cvi, 'optLmd_cvi', optLmd_cvi,optLmd_cvi, 'maximum ms-', optMs_cvi, ' \n')
    cat('sim-', i, ' fold-', j, ' end!\n')
    cat(file=traceFile, append=TRUE, 'sim-', i, ' fold-', j, ' time:', (proc.time()-start_cv)[3]/60, 'min!\n')
    cat('sim-', i, ' fold-', j, ' time:', (proc.time()-start_cv)[3]/60, 'min!\n')
    cat(file=traceFile, append=TRUE, 'sim-', i, ' fold-', j, ' end time:', date(), '!\n\n')
    cat('sim-', i, ' fold-', j, ' end time:', date(), '!\n\n')
    
    return(test_pred_avg)
    
    
}

par_lasso_FModel_simu <- function(i, norm, Btest)
{
    #load selected trVl data and ts data
    # save(tr_matrix_sel, tr_resp_sel, ts_matrix, response_ts, file=paste0(modelDir, '//dataFor_FModel_sim', i, '.RData'))
    load(paste0(modelDir, '//dataFor_FModel_sim', i, '.RData'))
    # Calculating initial lambda and the lambda sequence

    initial_lambda<-glmnet(x=tr_matrix_sel
                           , y=tr_resp_sel
                           , family="binomial"
                           , alpha=1
                           , standardize=F
                           , weights=ifelse(tr_resp_sel==1, 1, wt)
    )$lambda  # calculating the initial lambda
    lambda_seq<- c(initial_lambda[-length(initial_lambda)] 
                   , seq(initial_lambda[length(initial_lambda)] , 0 , length=100)
    ) # get a length=100 descending sequence from initial lambda to 0
    if(Btest==T){
        lambda_seq <- lambda_seq[1:5]
    }
    
#     rm(trTs_matrix, trVl, trTs)
    
    #n.folds cross validation
    cat(file=traceFile, append=T, 'cv on simulation:', i, ' start!\n')
    
    #parallel on n.folds cv
    sfInit(parallel=TRUE, cpus=n.folds, type='SOCK')
    #sfSource("F:\\Jie\\Shire\\03_code\\subFunc_v3.R")
    sfSource("F:\\Jie\\Shire_follow_up\\02_Code\\Similarity\\funs_similarity.R")
    cat(file=traceFile, append=TRUE, 'simulation:', i, ' sfExport running!\n')
    sfExport( 'crit', 'traceFile', 'lambda_seq', 'modelDir', 'dataDir', 'norm', "crit_nm")
    sfExport('createCurve_v2', 'par_cv_4F')
    sfClusterEval(library("glmnet"))
    sfClusterEval(library("ROCR"))
    sfClusterEval(library("plyr"))
    sfClusterEval(library("dplyr"))
    sfClusterEval(library("caTools"))
    
    cat(file=traceFile, append=TRUE, 'simulation:', i, ' parallele start running!\n')
    
    ms_fromCV <- sfClusterApplyLB(1:n.folds, par_cv_4F, i, wt)
    sfStop()
    ms_fromCV_df <- ldply(ms_fromCV, quickdf)
    
    ms_wtj <- apply(ms_fromCV_df, 2, mean,na.rm=T)
    optN <- which(ms_wtj==max(ms_wtj, na.rm=T))
    if(length(optN)>1){
        optN <- sample(optN, 1)
    }
    optLmd <- lambda_seq[optN]
    maxMs <- ms_wtj[optN]
    optMs_1wt <-  c(idx=optN, ms=maxMs)
    
    cat(file=traceFile, append=TRUE, "simulation-", i,' optimum lambda idx-', optN, ' optimum lambda-', optLmd, ' maximum(corresponding) ms-', maxMs, ' !\n')
    cat("simulation-", i,' optimum lambda idx-', optN, ' optimum lambda-', optLmd, ' maximum(corresponding) ms-', maxMs, ' !\n')
    #get the optimum model on training+validation and save the model
    # save(tr_matrix_sel, tr_resp_sel, ts_matrix, response_ts, file=paste0(modelDir, '//dataFor_FModel_sim', i, '.RData'))
    
    total_model<- glmnet(x=tr_matrix_sel
                         , y=tr_resp_sel
                         , lambda=lambda_seq
                         , family="binomial"
                         , alpha=1
                         , standardize=F
                         , weights=ifelse(tr_resp_sel==1, 1, wt)
    )
    save(lambda_seq, optN, total_model, file=paste0(modelDir,"//F_total_model_simu", i, ".RData"))
    cat(file=traceFile, append = T, 'apply optimum model to the whole training+validation data and save the total model end!\n')
    cat('apply optimum model to the whole training+validation data end!\n')
    
    model_coef<- total_model$beta[, as.vector(optN)]
    odds_ratio<- exp(model_coef)[model_coef != 0]
    non_zero_var<- names(model_coef)[as.vector(model_coef != 0)]
    re_model_var<- c('HAE', non_zero_var)
    #model<- data.frame(coefficient=model_coef[model_coef!=0], odds_ratio, p_value=p_value[match(non_zero_var, names(p_value))])
    model_output <- data.frame(variable=non_zero_var, 
                               coefficient=as.vector(model_coef)[as.vector(model_coef) != 0], 
                               odds_ratio=odds_ratio)
    write.csv(model_output, file = paste0(modelDir, '//F_model_coefficient_sim', i, '.csv'))
    
    #performance on test data
    
    
    #apply the optimum weight and lambda to the test data
    pred_ts<- predict(total_model, ts_matrix, type="response")[,optN]
    cat(file=traceFile, append=TRUE, 'simulation ', i, ': test_pred end!\n') #added by Jie

    resp_pred <- data.frame(resp=response_ts, pred=pred_ts)
    save(resp_pred, file=paste0(modelDir, '//F_resp_pred_ts_sim', i, '.RData'))
    return(resp_pred)    
    
}

S_reSelectData4F <- function(resultDir, dataDir, norm, n.simu, n.folds, wt, crit, Btest, crit_nm, threshold){
    dataDir <- "F:\\Jie\\Shire_follow_up\\03_Results\\for200+a\\"
    modelDir <- paste0(resultDir, 'Similarity_', crit_nm, '//wt=', wt, '_norm=', norm)
    temp <- lapply(1:n.simu, function(i){
        load(paste0(dataDir, 'splitedDataB4ModelTrVlNormSim', i, '.RData')) 
        load(paste0(dataDir, 'splitedDataB4ModelTsNormSim', i, '.RData')) 
        trVl <- as.data.frame(trVl_nm)
        response_tr <- trVl$HAE
        
        #save(pred_trVl, file=paste0(modelDir, '//S_pred_trVl_sim', i, '.RData'))
        #save(response_ts, file=paste0(modelDir, '//resp_ts_sim', i, '.RData'))
        load(paste0(modelDir, '//S_pred_trVl_sim', i, '.RData'))
        load(paste0(modelDir, '//resp_ts_sim', i, '.RData'))
        #save(tr_matrix_sel, tr_resp_sel, ts_matrix, response_ts, file=paste0(modelDir, '//dataFor_FModel_sim', i, '.RData'))
        load(paste0(modelDir, '//dataFor_FModel_sim', i, '.RData'))
        tr_matrix_sel_old <- tr_matrix_sel
        tr_resp_sel_old <- tr_resp_sel
        rm(tr_matrix_sel, tr_resp_sel)
        #select FP & P for training+validation
        idx <- which((pred_trVl>=threshold & tr_resp_sel_old==0) | tr_resp_sel_old==1)
        tr_matrix_sel <- tr_matrix_sel_old[idx, ]
        tr_resp_sel <- tr_resp_sel_old[idx]
        save(tr_matrix_sel, tr_resp_sel, ts_matrix, response_ts, file=paste0(modelDir, '//dataFor_FModel_sim', i, '.RData'))
        return(length(tr_resp_sel)/length(response_ts))
    })
    
}


#run the main function for cv lasso on simulations

run_lasso_S <- function(resultDir, dataDir, norm, n.folds, wt, crit, Btest, crit_nm){
    modelDir <- paste0(resultDir, 'Similarity_', crit_nm, '//wt=', wt, '_norm=', norm)
    if(!dir.exists(modelDir)) dir.create(modelDir, showWarnings = T, recursive = TRUE)
    traceFile <- paste0(modelDir, '//S_traceFile.csv')
    
    #n.sim parallele running start!
    cat(file=traceFile, append=T, 'parallele on n.simu simulation start!\n')
    
    sfInit(parallel=TRUE, cpus=n.simu, type='SOCK')
    #sfSource("F:\\Jie\\Shire\\03_code\\subFunc_v3.R")
    sfSource("F:\\Jie\\Shire_follow_up\\02_Code\\Similarity\\funs_similarity.R")
    
    cat(file=traceFile, append=TRUE, 'n.simu simulations parallel sfExport running!\n')
    sfExport( 'crit', 'traceFile', 'dataDir', 'modelDir', 'n.folds', "wt", "crit_nm")
    sfExport('createCurve_v2', 'par_cv', 'par_lasso_simu')
    sfClusterEval(library("glmnet"))
    sfClusterEval(library("ROCR"))
    sfClusterEval(library("plyr"))
    sfClusterEval(library("dplyr"))
    sfClusterEval(library("snowfall"))
    sfClusterEval(library("caTools"))
    
    temp <- sfClusterApplyLB(1:n.simu, par_lasso_simu, norm, Btest)
    #save(pred_ts_allSim, file=paste0(modelDir, '//pred_allSim.RData'))
    sfStop()
#     cat(unlist(lapply(pred_ts_allSim, length)), '\n')
    
}

run_lasso_F <- function(resultDir, dataDir, norm, n.folds, wt, crit, Btest, crit_nm){
    modelDir <- paste0(resultDir, 'Similarity_', crit_nm, '//wt=', wt, '_norm=', norm)
    if(!dir.exists(modelDir)) dir.create(modelDir, showWarnings = T, recursive = TRUE)
    traceFile <- paste0(modelDir, '//F_traceFile.csv')
    
    #n.sim parallele running start!
    cat(file=traceFile, append=T, 'parallele on n.simu simulation start!\n')
    
    sfInit(parallel=TRUE, cpus=n.simu, type='SOCK')
    #sfSource("F:\\Jie\\Shire\\03_code\\subFunc_v3.R")
    sfSource("F:\\Jie\\Shire_follow_up\\02_Code\\Similarity\\funs_similarity.R")
    
    cat(file=traceFile, append=TRUE, 'n.simu simulations parallel sfExport running!\n')
    sfExport( 'crit', 'traceFile', 'dataDir', 'modelDir', 'n.folds', "wt", "crit_nm")
    sfExport('createCurve_v2', 'par_cv_4F')
    sfClusterEval(library("glmnet"))
    sfClusterEval(library("ROCR"))
    sfClusterEval(library("plyr"))
    sfClusterEval(library("dplyr"))
    sfClusterEval(library("snowfall"))
    sfClusterEval(library("caTools"))
    
    temp <- sfClusterApplyLB(1:n.simu, par_lasso_FModel_simu, norm, Btest)
    #save(pred_ts_allSim, file=paste0(modelDir, '//pred_allSim.RData'))
    sfStop()
    #     cat(unlist(lapply(pred_ts_allSim, length)), '\n')
    
}


#summarize the performance on test
summarize_perf_200K <- function(resultDir, n.simu, target_recall, wt, norm, crit_nm){
    modelDir <- paste0(resultDir, 'Similarity_', crit_nm, '//wt=', wt, '_norm=', norm)
    #     save(pred_ts, file=paste0(modelDir, '//S_pred_ts_sim', i, '.RData'))
#     save(resp_pred, file=paste0(modelDir, '//F_resp_pred_ts_sim', i, '.RData'))
   
    perf_list <- lapply(1:n.simu, function(i){
        load(paste0(modelDir, '//S_pred_ts_sim', i, '.RData'))
        load(paste0(modelDir, '//F_resp_pred_ts_sim', i, '.RData'))
        pred <- cbind(resp_pred[, 2], pred_ts)
        pred_f <- ifelse(apply(pred, 1, function(x)sum(x[-1]))==2, 1, 0)
        
        temp1 <- msOnTest_sep_v2(pred_f, resp_pred[, 1], target_recall)
        return(temp1)
    })
    
    #summarize the ppv at target recall values for 
    #all the n.simu simulations(get the averaged value as the final metrics)
    ms_on200K_allSimu <- ldply(lapply(perf_list, function(x)x$ms), quickdf)
    ms_on200K_avg <- apply(ms_on200K_allSimu, 2, mean)
    
    #plot the performance on the all simulations
    rec_prec <- ldply(lapply(perf_list, function(x)x$rec_prec), rbind)
    rec_prec_omitMiss <- rec_prec[complete.cases(rec_prec),]
    #aupr <- trapz(rec_prec_omitMiss$recall, rec_prec_omitMiss$precision)
    #bucket <- cut(recall, breaks=seq(0, 1, 0.01), include.lowest=T,right=F)
    rec_prec_byBucket <- aggregate(rec_prec_omitMiss, by=list(rec_prec_omitMiss$recall), function(i)mean(i, na.rm=T))[, -1]
    sensitivity <- rec_prec_byBucket[, 1]
    PPV <- rec_prec_byBucket[, 2]
    pdf(file=paste(modelDir, '//recall-precision curve on 200kTS.pdf', sep=''))
    plot(sensitivity, PPV, type='l', main=paste('recall-precision curve(PR Curve)'))
    #plot(perf)
    dev.off()
    return(ms_on200K_avg)
    
}