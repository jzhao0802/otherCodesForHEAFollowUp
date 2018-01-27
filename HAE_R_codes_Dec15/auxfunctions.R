# trim data by variable to avoid outliers
trim_dat <- function(indata, invar, pct){
	n=dim(indata)[1]
	p=length(invar)
	idx=rep(1,n)
	for(i in 1:p){
		lwbd=quantile(indata[,invar[i]], pct[i])
		upbd=quantile(indata[,invar[i]], 1-pct[i])
		idx= idx*(indata[,invar[i]]>=lwbd)
		idx= idx*(indata[,invar[i]]<=upbd)
        }
	return(indata[which(idx==1),])
}

#split the origianl data into training and test
split <- function(hae, nonhae, n.simu)
{
    
    set.seed(20)
    simuId_hae <- createFolds(which(hae$HAE==1), n.simu) #position of row index
    simuId_nonhae <- createFolds(which(data$HAE==0), n.simu)
    dataSplited <- lapply(1:n.simu, function(i){
        haeSimu_i <- which(data$HAE==1)[simuId_hae[[i]]]
        nonhaeSimu_i <- which(data$HAE==0)[simuId_nonhae[[i]]]
        haeSimu_i_trVl <- which(data$HAE==1)[-simuId_hae[[i]]]
        nonhaeSimu_i_trVl <- which(data$HAE==0)[-simuId_nonhae[[i]]]
        
        simu_i_test <- bind_rows(data[haeSimu_i, ], data[nonhaeSimu_i, ])
        simu_i_trainValid <- bind_rows(data[haeSimu_i_trVl, ], data[nonhaeSimu_i_trVl, ])
        return(list(tr_vl=simu_i_trainValid, ts=simu_i_test))
    })
    save(dataSplited, file=paste0(arglist$resultDir, 'splitedDataB4clean2.RData'))
    cat("\nsplited data saved successfully!\n")
    
}

# find optimal cutoff point from ROC curve, output fp,tp,fn,fn
opt.cut = function(perf, pred){

        x=perf@x.values[[1]]
        y=perf@y.values[[1]]
        p=pred

        d = (x - 0)^2 + (y-1)^2
        ind = which(d == min(d))[1]
        v=list(
        cutoff = as.numeric(p@cutoffs[[1]][ind]),
        fp= p@fp[[1]][ind],
        tp= p@tp[[1]][ind],
        tn= p@tn[[1]][ind],
        fn= p@fn[[1]][ind]
        )
        return(v)
}

confus <- function(opt.cut){

	fp=opt.cut$fp
	tp=opt.cut$tp
	tn=opt.cut$tn
	fn=opt.cut$fn

	v=list(sensitivity=tp/(tp+fn),
	       specificity=tn/(fp+tn),
	       ppv=tp/(tp+fp),
	       npv=tn/(fn+tn),
	       acc=(tn+tp)/(tn+tp+fp+fn)
	       )
	return(v)
}


# PPV at Sensitivity=x
# v_sen: vector of sensitivity
# v_ppv: vector of PPV
ppv_sens <- function(v_sen, v_ppv, x){
	idx = min(which(abs(v_sen-x)==min(abs(v_sen-x)))) # look for sensitivity closest to value x
	r = v_ppv[idx]
	return(r)
}

ppv_sens_v <- function(v_sen, v_ppv, v_x){
	m=length(v_x)
	r=rep(0,m)
	for(i in 1:m){
		r[i] = ppv_sens(v_sen, v_ppv, v_x[i])
	}
	return(r)
}

# Calculate PR curve AUC
aupr_trapz <- function(v_sens, v_ppv){
	a = data.frame(x=v_sens, y=v_ppv)
	a = a[complete.cases(a),] # remove any NA
	return(trapz(a$x, a$y))
}


# Random Underbagging LASSO / Random Forest
# dat_pos/dat_neg: PATIENT_ID, HAE and predictors
undbag_lasso_rf <- function(rf_formula, dat_pos, dat_neg, iters, mtry, ntree, simu, plots_path){
    if(sum(grepl('patient_id', names(dat_neg), ignore.case = T))>1){
        flds=createFolds(dat_neg[, grepl('hae_patient_id', names(dat_neg), ignore.case = T)], iters)
    }else{
        flds=createFolds(dat_neg[, grepl('patient_id', names(dat_neg), ignore.case = T)], iters) #for new 300K dataset
        
    }
    
    x_pos = dat_pos[,-match(c('PATIENT_ID', 'HAE'), names(dat_pos))]
    y_pos = dat_pos$HAE
    x_neg = dat_neg[,-match(grep('patient_id|hae', names(dat_neg), valu=T, perl=T, ignore.case = T)
                            , names(dat_neg))]
    x_neg <- x_neg[, names(x_pos)]
    
    y_neg = dat_neg$HAE
    
    undbag_lasso_fit=list()
    undbag_rf_fit=list()
    
    #Sys.time()->start
    for (i in 1:iters){###
        
        #print(paste(i,"/",iters,sep=""))
        
        x_samp <- rbind(x_pos, x_neg[flds[[i]],])
        row.names(x_samp) <- NULL
        y_samp <- c(y_pos, y_neg[flds[[i]]])
        xy_samp <- data.frame(as.numeric(y_samp), x_samp)
        names(xy_samp)[1]="HAE"
        write.csv(xy_samp, paste0(plots_path, 'model_data_iter', i, '.csv'))
        # LASSO
        # 		undbag_lasso_fit[[i]] <- cv.glmnet(as.matrix(x_samp), y_samp, family='binomial', alpha=1) # LASSO CV
        
        # Random Forest
        rf_samp <- xy_samp
        rf_samp$HAE = as.factor(rf_samp$HAE)
        # 	        rf_samp <- rf_samp[1:2000,]
        # 		undbag_rf_fit[[i]] <- randomForest(formula=rf_formula, data=rf_samp, ntree=ntree, mtry=mtry, replace=T)
        undbag_rf_fit[[i]] <- randomForest(x=as.matrix(x_samp), y=as.factor(y_samp), ntree=ntree, mtry=mtry, replace=T)
        
    }
    #print(Sys.time()-start)
    #each one takes ~44 sec, so total  takes iters x 44 sec.
    
    # 	ans = list(undbag_lasso_fit=undbag_lasso_fit, undbag_rf_fit=undbag_rf_fit)
    ans = list(undbag_rf_fit=undbag_rf_fit)
    return(ans)
    
}


# Bagging logistic regression
# dat_pos/dat_neg: PATIENT_ID, HAE and predictors
bagging_logit <- function(dat_pos, dat_neg, iters){

	flds=createFolds(dat_neg$PATIENT_ID, iters)

	x_pos = dat_pos[,-(1:2)]
	y_pos = dat_pos$HAE
	x_neg = dat_neg[,-(1:2)]
	y_neg = dat_neg$HAE

	bagging_logit_fit=list()

	#Sys.time()->start
	for (i in 1:iters){###

		print(paste(i,"/",iters,sep=""))

		x_samp <- rbind(x_pos, x_neg[flds[[i]],])
		row.names(x_samp) <- NULL
		y_samp <- c(y_pos, y_neg[flds[[i]]])
		xy_samp <- data.frame(as.numeric(y_samp), x_samp)
		names(xy_samp)[1]="HAE"

		bagging_logit_fit[[i]] = glm(HAE~., family=binomial(), data=xy_samp)

	}
	#print(Sys.time()-start)

	return(bagging_logit_fit)

}

# find desire sensitivity cut off
prob_cut_sens <- function(pred, ppv_val){

        perf = performance(pred, "ppv", "sens")
        x = perf@x.values[[1]]
        y = perf@y.values[[1]]
        ind = max(which(abs(ppv_val-x)==min(abs(ppv_val-x))))
        cutoff = as.numeric(pred@cutoffs[[1]][ind])

        return(cutoff)
}

prob_cut_sens_v <- function(pred, v_ppv_val){
	m=length(v_ppv_val)
	r=rep(0,m)
	for(i in 1:m){
		r[i] = prob_cut_sens(pred, v_ppv_val[i])
	}
	return(r)
}


# produce ppv, npv, sensitivity, specificity, accuracy
confusionM <- function(pred_class, label, pos_char, neg_char){

		xtab = table(pred_class, label)
		a = confusionMatrix(xtab, positive = pos_char)
		accuracy = as.numeric(a$overall["Accuracy"])
		ppv = as.numeric(a$byClass["Pos Pred Value"])
		sensitivity = as.numeric(a$byClass["Sensitivity"])
		npv = as.numeric(a$byClass["Neg Pred Value"])
		specificity = as.numeric(a$byClass["Specificity"])

		confusion_table = a$table

		tp = confusion_table[pos_char, pos_char]
		fp = confusion_table[pos_char, neg_char]
		tn = confusion_table[neg_char, neg_char]
		fn = confusion_table[neg_char, pos_char]

		v = list(confusion_table = confusion_table,
		         tp = tp, fp = fp, tn = tn, fn = fn,
			 accuracy = accuracy, ppv = ppv, sensitivity = sensitivity, npv = npv, specificity = specificity)
                return(v)
}

expit <- function(x){
	return(exp(x)/(1+exp(x)))
}





