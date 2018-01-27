msOnTest_sep_v2 <- function(pred, response, recall_tar, plots_path){
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



msOnTest_sep_v3 <- function(pred, response, recall_tar, plots_path, simu){
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
    plot(recall, precision, type='l', main=paste0('recall-precision curve simulation', simu))
    #plot(perf)
#     dev.off()
    
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


# new 200K dataset 972 wighout removing region==U

split_simu <- function(simu, nonhaeFile, outDir){
    dat_hae_1111_rf_nov26 <- 
        read.csv("F:\\Jie\\Shire_follow_up\\01_data\\dat_hae_1111_rf_nov26_flag.csv"
                 , header=T, sep=",", check.names=F)
    if(nonhaeFile=='new_200K'){
        dat_nonhae <- read.table(paste0("F:\\Jie\\Shire_follow_up\\01_data\\newdata_200K_3M\\"
                                        , "nonhae_200K", ".csv")
                                 , sep=','
                                 , stringsAsFactors = F
                                 , head=T
        )
    }else if(nonhaeFile=='old_200K'){
        load(paste("F:\\Jie\\Shire_follow_up\\01_data", "\\dat_nonhae_1111_rf.RData", sep=""))
        dat_nonhae <- dat_nonhae_1111_rf
        rm(dat_nonhae_1111_rf)
        gc()
    }
    load("F:\\Jie\\Shire_follow_up\\01_data\\HAE1087_ptid.RData")
    
    #     hae <- dat_hae_1111_rf_nov26 %>% filter(FLAG_==0) %>% select(-c(REGION, FLAG_)) %>% filter(GENDER !='U') %>% 
    #     {
    #         .$GENDERM=ifelse(.$GENDER=='M', 1, 0)
    #         .
    #     } %>% select(-GENDER) %>% filter(AGE>12) %>%
    #     {
    #         dataLastStep <- .
    #         sumCol <- apply(select(., matches("_FLAG")), 1, sum)
    #         nonAllZeroPats <- 
    #             mutate(., allzeros=(sumCol < 3)) %>%
    #             filter(allzeros==F) %>%
    #             select(-allzeros)
    #         print(paste("Number of event < 3 patients:", nrow(dataLastStep)-nrow(nonAllZeroPats)))
    #         nonAllZeroPats
    #     } 
    hae <- dat_hae_1111_rf_nov26 %>% filter(FLAG_==0) %>% select(-c(REGION, FLAG_)) %>% filter(GENDER !='U')
    hae$GENDERM <- ifelse(hae$GENDER=='M', 1, 0)
    hae$GENDER <- NULL
    hae <- hae[hae$PATIENT_ID %in% HAE1087_ptid, ]
    
    nonhae <- dat_nonhae %>% mutate(LOOKBACK_DAYS=lookback_days) %>% select(-lookback_days)
    if(nonhaeFile=='old_200K'){
        nonhae <- nonhae %>% select(-REGION) %>% mutate(hae_patient_id=HAE_PATIENT)  %>% select(-HAE_PATIENT) 
        nonhae$GENDERM <- ifelse(nonhae$GENDER=='M', 1, 0)
        nonhae$GENDER <- NULL
        nonhae$hae_patient_id <- as.numeric(nonhae$hae_patient_id)
    }
    # sample parts of nonhae according to the times which is the repeated times of nonhae to hae
    #     times <- 3
    #     nonhae_sample <- nonhae %>% 
    #         group_by(hae_patient_id) %>%
    #         sample_n(times)
    #     nonhae <- as.data.frame(nonhae_sample)
    
    
    set.seed(20)
    tr_idx <- createFolds(hae$PATIENT_ID, 5, returnTrain=T)[[simu]]
    dat_hae_trn <- hae[tr_idx, ]
    dat_hae_tst <- hae[-tr_idx, ]
    
    dat_nonhae_trn <- nonhae[nonhae$hae_patient_id %in% dat_hae_trn$PATIENT_ID,]
    dat_nonhae_tst <- nonhae[nonhae$hae_patient_id %in% dat_hae_tst$PATIENT_ID,]
    dat_nonhae_trn$HAE <- 0
    dat_nonhae_tst$HAE <- 0
    if(!dir.exists(outDir)) dir.create(outDir, showWarnings = T, recursive = TRUE)
    
    save(dat_hae_trn, dat_hae_tst, dat_nonhae_trn , dat_nonhae_tst
         , file=paste(outDir, "dat_hae_trn_tst_split_simu", simu, ".RData", sep=""))
    
}

split_simu2 <- function(simu, nonhaeFile, outDir){
    dat_hae_1111_rf_nov26 <- 
        read.csv("F:\\Jie\\Shire_follow_up\\01_data\\dat_hae_1111_rf_nov26_flag.csv"
                 , header=T, sep=",", check.names=F)
    
        load(paste("F:\\Jie\\Shire_follow_up\\01_data", "\\dat_nonhae_1111_rf.RData", sep=""))
        dat_nonhae <- dat_nonhae_1111_rf
        rm(dat_nonhae_1111_rf)
        gc()

    hae <- dat_hae_1111_rf_nov26 %>% filter(FLAG_==0) %>% select(-c( FLAG_)) 
hae <- as.data.frame(model.matrix(~., hae)[, -1])
    nonhae <- dat_nonhae %>% mutate(LOOKBACK_DAYS=lookback_days) %>% select(-lookback_days)
        nonhae <- nonhae %>% mutate(hae_patient_id=HAE_PATIENT)  %>% select(-HAE_PATIENT) 
        nonhae$hae_patient_id <- as.numeric(nonhae$hae_patient_id)
        nonhae <- as.data.frame(model.matrix(~., nonhae)[, -1])
    # sample parts of nonhae according to the times which is the repeated times of nonhae to hae
    #     times <- 3
    #     nonhae_sample <- nonhae %>% 
    #         group_by(hae_patient_id) %>%
    #         sample_n(times)
    #     nonhae <- as.data.frame(nonhae_sample)
    
    
    set.seed(20)
    tr_idx <- createFolds(hae$PATIENT_ID, 5, returnTrain=T)[[simu]]
    dat_hae_trn <- hae[tr_idx, ]
    dat_hae_tst <- hae[-tr_idx, ]
    
    dat_nonhae_trn <- nonhae[nonhae$hae_patient_id %in% dat_hae_trn$PATIENT_ID,]
    dat_nonhae_tst <- nonhae[nonhae$hae_patient_id %in% dat_hae_tst$PATIENT_ID,]
    dat_nonhae_trn$HAE <- 0
    dat_nonhae_tst$HAE <- 0
    if(!dir.exists(outDir)) dir.create(outDir, showWarnings = T, recursive = TRUE)
    
    save(dat_hae_trn, dat_hae_tst, dat_nonhae_trn , dat_nonhae_tst
         , file=paste(outDir, "dat_hae_trn_tst_split_simu", simu, ".RData", sep=""))
    
}

split_simu3 <- function(simu, haeFile, nonhaeFile, dataDir, outDir){
    dat_hae_1111_rf_nov26 <- 
        read.csv(paste0(dataDir, "dat_hae_1111_rf_nov26_flag.csv")
                 , header=T, sep=",", check.names=F)
    
    
    
    hae <- dat_hae_1111_rf_nov26 %>% filter(FLAG_==0) %>% select(-c( FLAG_, REGION)) 
    # hae <- as.data.frame(model.matrix(~., hae)[, -1])
    hae$GENDERM <- ifelse(hae$GENDER=='M', 1, 0)
    hae$GENDER <- NULL
    if(haeFile==973){
        HAE973_ptid <- readRDS(paste0(dataDir, 'HAE973_ptid.RData'))
        hae <- hae[hae$PATIENT_ID %in% HAE973_ptid, ]
        
    }else if(haeFile==1087){
        load(pate0(dataDir, 'HAE1087_ptid.RData'))
        hae <- hae[hae$PATIENT_ID %in% HAE1087_ptid, ]
    }
    
    # nonhae
    if(nonhaeFile=='for_old200K'){
        load(paste(dataDir, "dat_nonhae_1111_rf.RData", sep=""))
        
        nonhae <- dat_nonhae_1111_rf %>% mutate(LOOKBACK_DAYS=lookback_days) %>% select(-c(lookback_days, REGION))
        nonhae <- nonhae %>% mutate(hae_patient_id=HAE_PATIENT)  %>% select(-HAE_PATIENT) 
        nonhae$hae_patient_id <- as.numeric(nonhae$hae_patient_id)
        # nonhae <- as.data.frame(model.matrix(~., nonhae)[, -1])
        nonhae$GENDERM <- ifelse(nonhae$GENDER=='M', 1, 0)
        nonhae$GENDER <- NULL
    }else if(nonhaeFile=='for_new200K'){
        dat_nonhae <- read.table(paste0(dataDir, "newdata_200K_3M\\"
                                        , "nonhae_200K_v2", ".csv")
                                 , sep=',' 
                                 , stringsAsFactors = F
                                 , head=T
        )
        nonhae <- dat_nonhae %>% mutate(LOOKBACK_DAYS=lookback_days) %>% select(-c(lookback_days))
    }else if(nonhaeFile=='for_new300K'){
        dat_nonhae <- read.table(paste0(dataDir, "newdata_200K_3M\\"
                                        , "nonhae_300K", ".csv")
                                 , sep=',' 
                                 , stringsAsFactors = F
                                 , head=T
        )
        dat_nonhae <- dat_nonhae %>% mutate(LOOKBACK_DAYS=lookback_days) %>% select(-c(lookback_days))
        
    }else{
        stop('the wrong nonhae input!\n')
    }
    
    # sample parts of nonhae according to the times which is the repeated times of nonhae to hae
    #     times <- 3
    #     nonhae_sample <- nonhae %>% 
    #         group_by(hae_patient_id) %>%
    #         sample_n(times)
    #     nonhae <- as.data.frame(nonhae_sample)
    
    if(nonhaeFile=='for_new300K'){
        set.seed(20)
        tr_idx <- createFolds(hae$PATIENT_ID, 5, returnTrain=T)[[simu]]
        dat_hae_trn <- hae[tr_idx, ]
        dat_hae_tst <- hae[-tr_idx, ]
        
        tr_idx_nonhae <- createFolds(dat_nonhae$patient_id, 5, returnTrain=T)[[simu]]
        dat_nonhae_trn <- dat_nonhae[tr_idx_nonhae, ]
        dat_nonhae_tst <- dat_nonhae[-tr_idx_nonhae, ]
        
    }else{
        set.seed(20)
        tr_idx <- createFolds(hae$PATIENT_ID, 5, returnTrain=T)[[simu]]
        dat_hae_trn <- hae[tr_idx, ]
        dat_hae_tst <- hae[-tr_idx, ]
        
        dat_nonhae_trn <- nonhae[nonhae$hae_patient_id %in% dat_hae_trn$PATIENT_ID,]
        dat_nonhae_tst <- nonhae[nonhae$hae_patient_id %in% dat_hae_tst$PATIENT_ID,]
    }
    
    dat_nonhae_trn$HAE <- 0
    dat_nonhae_tst$HAE <- 0
    outDir <- paste0(outDir, nonhaeFile, '&', haeFile, '\\')
    if(!dir.exists(outDir)) dir.create(outDir, showWarnings = T, recursive = TRUE)
    
    save(dat_hae_trn, dat_hae_tst, dat_nonhae_trn , dat_nonhae_tst
         , file=paste(outDir, "dat_hae_trn_tst_split_simu", simu, ".RData", sep=""))
    
}

run_split <- function(n.simu, haeFile, nonhaeFile, dataDir, outDir, split_fun){
    for (i in 1:n.simu){
        eval(parse(text=paste0('split_simu', split_fun, '(simu=', i, ', haeFile=haeFile, nonhaeFile=nonhaeFile, dataDir=dataDir, outDir=outDir)')))
    }
}



# run the bagging forest model
run_bagging_rf_par <- function(simu, plots_path1, lasso_rf_iters)
{
    plots_path <- paste0(plots_path1, 'iters=', lasso_rf_iters, '\\simu', simu, '\\')
    if(!dir.exists(plots_path)) dir.create(plots_path, showWarnings = T, recursive = TRUE)
    
    load(paste(plots_path1, "dat_hae_trn_tst_split_simu", simu, ".RData", sep=""))
    
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
    #      rf.formula=as.simple.formula(names(dat_trn)[which(names(dat_trn)!='HAE')], 'HAE')
    rf.formula=NA
    
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
        save(ans_lasso_rf, file=paste0(plots_path, 'ans_lasso_rf.RData'))
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
    load(paste(plots_path1, "dat_hae_trn_tst_split_simu", simu, ".RData", sep=""))
    
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
    
}


run_bagging_rf_par_forTrainigFit <- function(simu, plots_path1, lasso_rf_iters)
{
    plots_path <- paste0(plots_path1, 'iters=', lasso_rf_iters, '\\simu', simu, '\\')
    if(!dir.exists(plots_path)) dir.create(plots_path, showWarnings = T, recursive = TRUE)
    
    load(paste(plots_path1, "dat_hae_trn_tst_split_simu", simu, ".RData", sep=""))
    
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
    ### Model training (all training data)
    #########################################################################
    
    Sys.time()->start
    
    # (1) Underbagging LASSO / Random Forest
    trn_ans_lasso_rf = undbag_lasso_rf(rf_formula=rf.formula, dat_pos=dat_hae_trn, dat_neg=dat_nonhae_trn
                                       , iters=lasso_rf_iters, mtry=25, ntree=300
                                       , simu=simu, plots_path = plots_path)
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
    load(paste(plots_path1, "dat_hae_trn_tst_split_simu", simu, ".RData", sep=""))
    
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
    dat_tst <- dat_tst[, c('HAE', setdiff(names(dat_tst), 'HAE'))]
    write.table(dat_tst, paste0(plots_path, 'dat_tst.csv'), row.names = F, sep=',')
    for (i in 1:lasso_rf_iters){
        # 	tst_prob_lasso = tst_prob_lasso + predict(trn_undbag_lasso_fit[[i]], as.matrix(x_tst), s="lambda.min", type="response")/lasso_rf_iters
        tst_prob_rf = tst_prob_rf + predict(trn_undbag_rf_fit[[i]], dat_tst[, -match('HAE', names(dat_tst))], type = "prob")[,2]/lasso_rf_iters
    }
    
    print(Sys.time()-start)
    # Time difference of 12.26336 mins
    
    save(tst_label, tst_prob_rf, file=paste(plots_path, "tst_rf_prob.RData", sep=""))
    
}


run_bagging_rf <- function(n.simu, wk_dir, outDir, lasso_rf_iters)
{
    trace_path <- paste0(outDir, 'iters=', lasso_rf_iters, '\\')
    if(!dir.exists(trace_path)) dir.create(trace_path, showWarnings = T, recursive = TRUE)
    
    traceFile <- paste0(trace_path, 'traceFile.csv')
    cat(file=traceFile, append=T, 'parallele on n.simu simulation start!\n')
    #     wk_dir = "D:\\jzhao\\Shire_followup\\02_Code\\HAE_R_codes_Dec15\\"
    
    sfInit(parallel=TRUE, cpus=n.simu, type='SOCK')
    #sfSource("F:\\Jie\\Shire\\03_code\\subFunc_v3.R")
    
    cat(file=traceFile, append=TRUE, 'n.simu simulations parallel sfExport running!\n')
    sfExport(  'outDir', 'wk_dir')
    sfExport('undbag_lasso_rf'
    )
    
    sfSource(paste0(wk_dir, "loadpackage.R"))
    # Auxiliary functions
    sfSource(paste0(wk_dir, "auxfunctions.R"))
    # 
    sfSource(paste0(wk_dir, "funs_baggingRF.R"))
    
    sfClusterEval(library(ggplot2))
    sfClusterEval(library(ROCR))
    sfClusterEval(library(PRROC))
    # sfClusterEval(library(FSelector))
    sfClusterEval(library(randomForest))
    sfClusterEval(library(caret))
    sfClusterEval(library(e1071))
    sfClusterEval(library(reshape2))
    sfClusterEval(library(sqldf))
    sfClusterEval(library(glmnet))
    sfClusterEval(library(caTools))
    # sfClusterEval(library(gbm))
    # sfClusterEval(library(xlsx))
    sfClusterEval(library(dplyr))    
    temp <- sfClusterApplyLB(1:n.simu, run_bagging_rf_par_forTrainigFit
                             , outDir
                             , lasso_rf_iters 
    )
    #save(pred_ts_allSim, file=paste0(modelDir, '//pred_allSim.RData'))
    sfStop()
    #     cat(unlist(lapply(pred_ts_allSim, length)), '\n')
}



get_perf_allSimu <- function(outdir, iters, n.simu, recall_tar){
    
    temp <- lapply(1:n.simu, function(i){
        load(paste0(outdir, "dat_hae_trn_tst_split_simu", i, ".RData"))
        resp <- c(dat_hae_tst$HAE, dat_nonhae_tst$HAE)
        saveRDS(resp, file = paste0(outdir, "resp_simu", i, ".RData"))
        load(paste0(outdir, "iters=", iters, '\\simu', i, '\\tst_rf_prob.RData'))
        resp_pred <- data.frame(resp=resp, pred=tst_prob_rf)
        return(resp_pred)
    })
    
    resp_pred <- ldply(temp, rbind)
    saveRDS(resp_pred, paste0(outdir, 'iters=', iters, '\\resp_pred.RData'))
    temp1 <- msOnTest_sep_v2(resp_pred[, 2], resp_pred[, 1], recall_tar)
    perf <- temp1$ms
    write.csv(perf, paste0(outdir, 'iters=', iters, '\\performance_onAllSimu.csv'))
    return(perf)
}


get_perf_allSimu_forPRcurve <- function(outdir, iters, n.simu, recall_tar){
    
    plots_path <- paste0(outdir, 'iters=', iters, '\\')
    
    # saveRDS(resp_pred, paste0(outdir, 'iters=', iters, '\\resp_pred.RData'))
    resp_pred <- readRDS(paste0(plots_path, 'resp_pred.RData'))
    temp1 <- msOnTest_sep_v2(resp_pred[, 2], resp_pred[, 1], recall_tar, plots_path = plots_path)
    perf <- temp1$ms
    recPrec <- temp1$rec_prec
    write.csv(recPrec, paste0(plots_path, "recall_precision_on200K.csv"))
    write.csv(temp1$curve, paste0(plots_path, "recall_precision_byBucket_on200K.csv"), 
              row.names=F, quote=T)
        pdf(file=paste(plots_path, 'recall-precision curve on test 200K.pdf', sep=''))
        recall=recPrec[, 1]
        precision=recPrec[, 2]
        plot(x=recall, y=precision, type='l', main='recall-precision curve(5 simulations)')
        dev.off()
    # write.csv(perf, paste0(outdir, 'iters=', iters, '\\performance_onAllSimu.csv'))
    return(perf)
}


get_perf_3M_par_forPRcurve <- function(simu, outDir, lasso_rf_iters, recall_tar, fileNm_3M, path_3M){
    
    plots_path <- paste0(outDir, 'iters=', lasso_rf_iters, '\\simu', simu, '\\')
    # if(!dir.exists(plots_path)){dir.create(plots_path, showWarnings = T, recursive = T, model='0777')}
#     x_3M <- read.table(paste0(path_3M, fileNm_3M, ".csv")
#                        , sep=','
#                        , stringsAsFactors = F
#                        , head=T
#     )
#     if('lookback_days' %in% names(x_3M)){
#         x_3M <- x_3M %>% mutate(LOOKBACK_DAYS=lookback_days) %>% select(-patient_id) %>% select(-lookback_days)
#         
#     }
#     
    # save( tst_prob_rf_hae, tst_prob_rf, file=paste(plots_path, "tst_rf_prob_haeTs&3M.RData", sep=""))
    load(paste(plots_path, "tst_rf_prob_haeTs&3M.RData", sep=""))
    resp <- c(rep(1, length(tst_prob_rf_hae)), rep(0, length(tst_prob_rf)))
    
    pred <- c(tst_prob_rf_hae, tst_prob_rf)
    
    perf_result <- msOnTest_sep_v3(pred, resp, recall_tar=seq(0.5, 0.05, -0.05), plots_path, simu)
    recPrec <- perf_result$rec_prec
    write.csv(recPrec, paste0(plots_path, "recall_precision_sim", simu, '.csv'))
    write.csv(perf_result$curve, paste0(plots_path, "recall_precision_byBucket_sim", simu, '.csv'), 
              row.names=F, quote=T)
    
#     write.csv(perf_result$ms, paste0(plots_path, 'perf_on3M.csv'))
    result <- c(simu=simu, perf_result$ms)
    return(result)
}
run_perf_3M_forPRcurve <- function(outDir, wk_dir, lasso_rf_iters, n.simu, recall_tar, fileNm_3M, path_3M){
     trace_path <- paste0(outDir, 'iters=', lasso_rf_iters, '\\')
    if(!dir.exists(trace_path)) dir.create(trace_path, showWarnings = T, recursive = TRUE)
#     plot
    pdf(file=paste(trace_path, 'recall-precision curve on test 3M.pdf', sep=''))
    par(mfrow=c(3,2))
    par(pty='m')
    par(cex.lab=1.2, cex.axis=0.9)
    
    for(simu in 1:n.simu){
        temp=get_perf_3M_par_forPRcurve(simu
                                   , outDir
                                   , lasso_rf_iters
                                   , recall_tar
                                   , fileNm_3M
                                   , path_3M
                                   )
#         return('finished\n')
    }
    
    dev.off()
}



get_perf_3M_par <- function(simu, outDir, lasso_rf_iters, recall_tar, fileNm_3M, path_3M){
    
    plots_path <- paste0(outDir, 'iters=', lasso_rf_iters, '\\simu', simu, '\\')
    # if(!dir.exists(plots_path)){dir.create(plots_path, showWarnings = T, recursive = T, model='0777')}
    x_3M <- read.table(paste0(path_3M, fileNm_3M, ".csv")
                       , sep=','
                       , stringsAsFactors = F
                       , head=T
    )
    if('lookback_days' %in% names(x_3M)){
        x_3M <- x_3M %>% mutate(LOOKBACK_DAYS=lookback_days) %>% select(-patient_id) %>% select(-lookback_days)
        
    }
    
    load(paste0(outDir, 'dat_hae_trn_tst_split_simu', simu, '.RData'))
    rm(dat_hae_trn, dat_nonhae_trn, dat_nonhae_tst)
    gc()
    
    tst_prob_rf = rep(0, nrow(x_3M))
    tst_prob_rf_hae = rep(0, nrow(dat_hae_tst)) #218
    
    Sys.time()->start
    
    # Logistic regression
    # tst_prob_logit = predict(trn_logit_fit, dat_tst[,-match('HAE', names(dat_tst))], type="response")
    
    # Bagging LASSO, Bagging Random Forest
    load(paste0(plots_path, 'trn_rf_fit_Mar31.RData'))
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
    
    perf_result <- msOnTest_sep_v2(pred, resp, recall_tar=seq(0.5, 0.05, -0.05))
    write.csv(perf_result$ms, paste0(plots_path, 'perf_on3M.csv'))
    result <- c(simu=simu, perf_result$ms)
    return(result)
}

run_perf_3M <- function(outDir, wk_dir, lasso_rf_iters, n.simu, recall_tar, fileNm_3M, path_3M){
    trace_path <- paste0(outDir, 'iters=', lasso_rf_iters, '\\')
    # if(!dir.exists(trace_path)) dir.create(trace_path, showWarnings = T, recursive = TRUE)
    
    traceFile <- paste0(trace_path, 'traceFile_pred3M.csv')
    cat(file=traceFile, append=T, 'parallele on n.simu simulation start!\n')
    #     wk_dir = "D:\\jzhao\\Shire_followup\\02_Code\\HAE_R_codes_Dec15\\"
    
    sfInit(parallel=TRUE, cpus=n.simu, type='SOCK')
    #sfSource("F:\\Jie\\Shire\\03_code\\subFunc_v3.R")
    
    cat(file=traceFile, append=TRUE, 'n.simu simulations parallel sfExport running!\n')
    sfExport(  'outDir', 'wk_dir')
    sfExport('get_perf_3M_par', 'msOnTest_sep_v2'
    )
    
    sfSource(paste0(wk_dir, "loadpackage.R"))
    # Auxiliary functions
    sfSource(paste0(wk_dir, "auxfunctions.R"))
    # 
    sfSource(paste0(wk_dir, "funs_baggingRF.R"))
    
    sfClusterEval(library(ggplot2))
    sfClusterEval(library(ROCR))
    sfClusterEval(library(PRROC))
    # sfClusterEval(library(FSelector))
    sfClusterEval(library(randomForest))
    sfClusterEval(library(caret))
    sfClusterEval(library(e1071))
    sfClusterEval(library(reshape2))
    sfClusterEval(library(sqldf))
    sfClusterEval(library(glmnet))
    sfClusterEval(library(caTools))
    # sfClusterEval(library(gbm))
    # sfClusterEval(library(xlsx))
    sfClusterEval(library(dplyr))    
    temp <- sfClusterApplyLB(1:n.simu, get_perf_3M_par
                             , outDir
                             , lasso_rf_iters 
                             , recall_tar
                             , fileNm_3M
                             , path_3M
    )
    #save(pred_ts_allSim, file=paste0(modelDir, '//pred_allSim.RData'))
    sfStop()
    
    ms_allSimu <- ldply(temp, quickdf)
    
    return(ms_allSimu)
    
}


get_perf_2.5M_par <- function(simu, outDir, lasso_rf_iters, recall_tar, fileNm_2.5M, path_2.5M){
    
    plots_path <- paste0(outDir, 'iters=', lasso_rf_iters, '\\simu', simu, '\\')
    # if(!dir.exists(plots_path)){dir.create(plots_path, showWarnings = T, recursive = T, model='0777')}
    #     x_3M <- read.table(paste0(path_2.5M, fileNm_2.5M, ".csv")
    #                        , sep=','
    #                        , stringsAsFactors = F
    #                        , head=T
    #     )
    
    x_3M <- lapply(1:51, function(i) {
        load(paste0(path_2.5M, fileNm_2.5M, '_', i, '.RData'))
        return(adj_ppv_samp)
    })
    x_3M <- ldply(x_3M, rbind)
    
    load(paste0(plots_path, 'trn_rf_fit_Mar31.RData'))
    vars_rf <- rownames(trn_undbag_rf_fit[[1]]$importance)
    if('LOOKBACK_DAYS' %in% vars_rf & 'lookback_days' %in% names(x_3M)){
        x_3M <- x_3M %>% mutate(LOOKBACK_DAYS=lookback_days) %>% select(-PATIENT_ID) %>% select(-lookback_days)
        
    }
    load(paste0(outDir, 'dat_hae_trn_tst_split_simu', simu, '.RData'))
    rm(dat_hae_trn, dat_nonhae_trn, dat_nonhae_tst)
    gc()
    
    tst_prob_rf = rep(0, nrow(x_3M))
    tst_prob_rf_hae = rep(0, nrow(dat_hae_tst)) #218
    
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
    
    save( tst_prob_rf_hae, tst_prob_rf, file=paste(plots_path, "tst_rf_prob_haeTs&2.5M.RData", sep=""))
    
    resp <- c(rep(1, length(tst_prob_rf_hae)), rep(0, length(tst_prob_rf)))
    
    pred <- c(tst_prob_rf_hae, tst_prob_rf)
    
    perf_result <- msOnTest_sep_v2(pred, resp, recall_tar=seq(0.5, 0.05, -0.05))
    write.csv(perf_result$ms, paste0(plots_path, 'perf_on2.5M.csv'))
    result <- c(simu=simu, perf_result$ms)
    return(result)
}

run_perf_2.5M <- function(outDir, wk_dir, lasso_rf_iters, n.simu, recall_tar, fileNm_2.5M, path_2.5M){
    trace_path <- paste0(outDir, 'iters=', lasso_rf_iters, '\\')
    # if(!dir.exists(trace_path)) dir.create(trace_path, showWarnings = T, recursive = TRUE)
    
    traceFile <- paste0(trace_path, 'traceFile_pred2.5M.csv')
    cat(file=traceFile, append=T, 'parallele on n.simu simulation start!\n')
    #     wk_dir = "D:\\jzhao\\Shire_followup\\02_Code\\HAE_R_codes_Dec15\\"
    
    sfInit(parallel=TRUE, cpus=n.simu, type='SOCK')
    #sfSource("F:\\Jie\\Shire\\03_code\\subFunc_v3.R")
    
    cat(file=traceFile, append=TRUE, 'n.simu simulations parallel sfExport running!\n')
    sfExport(  'outDir', 'wk_dir')
    sfExport('get_perf_2.5M_par', 'msOnTest_sep_v2'
    )
    
    sfSource(paste0(wk_dir, "loadpackage.R"))
    # Auxiliary functions
    sfSource(paste0(wk_dir, "auxfunctions.R"))
    # 
    sfSource(paste0(wk_dir, "funs_baggingRF.R"))
    
    sfClusterEval(library(ggplot2))
    sfClusterEval(library(ROCR))
    sfClusterEval(library(PRROC))
    # sfClusterEval(library(FSelector))
    sfClusterEval(library(randomForest))
    sfClusterEval(library(caret))
    sfClusterEval(library(e1071))
    sfClusterEval(library(reshape2))
    sfClusterEval(library(sqldf))
    sfClusterEval(library(glmnet))
    sfClusterEval(library(caTools))
    # sfClusterEval(library(gbm))
    # sfClusterEval(library(xlsx))
    sfClusterEval(library(dplyr))    
    temp <- sfClusterApplyLB(1:n.simu, get_perf_2.5M_par
                             , outDir
                             , lasso_rf_iters 
                             , recall_tar
                             , fileNm_2.5M
                             , path_2.5M
    )
    #save(pred_ts_allSim, file=paste0(modelDir, '//pred_allSim.RData'))
    sfStop()
    
    ms_allSimu <- ldply(temp, quickdf)
    
    return(ms_allSimu)
    
}
