library(dplyr)

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
CapPercentile <- function(vec, Hae, percentile)
{
    if ((percentile < 0) | (percentile > 100))
        stop("Error! percentile must be between 0 and 100 (inclusive). ")
    positivePatPart <- vec[Hae==1]
    nonzeroPart <- positivePatPart[positivePatPart > 0]
    if (length(nonzeroPart) == 0)
        return (vec)
    threshold <- quantile(nonzeroPart, percentile/100, type=3)
    vec[vec > threshold] <- threshold
    #   print(paste("In CapPercentile threshold:", threshold))
    #   print(paste("In CapPercentile max(vec):", max(vec)))
    return (vec)
}

PrintMaxAndPercentile <- function(vec, percentile)
{
    p <- quantile(vec, percentile/100, type=3)
    m <- max(vec)
    print("head(vec):")
    print(head(vec))
    print(paste(names(vec), ", Max:", m, ", 99 percentile:", p))
}


#step1:  b.	All other steps in my cleaning and feature selection code need to be performed
#outside the evaluation / simulation loop (i.e., on the whole ~1K positives + ~200K negatives).


arglist <- list(bTestMode=T
                , dataDir='F:\\Lichao\\work\\Projects\\HAE\\data\\from Dong\\'
                , resultDir= "F:\\Jie\\Shire_follow_up\\03_Results\\for200+\\"
)

clean_pts <- function(bTestMode, dataDir, resultDir)
{
    print("Loading and preparing data..");cat("\n")
    dat_hae_1111_rf_nov26 <- 
        read.csv("F:/Lichao/work/Projects/HAE/data/Zhenxing/dat_hae_1111_rf_nov26_flag.csv"
                 , header=T, sep=",", check.names=F)
    load(paste(dataDir, "dat_nonhae_1111_rf.RData", sep=""))
    # !!!!!!!!!
    # the following needs to be changed, as at the beginning 
    # both positives and negatives have PATIENT_ID
    # but the HAE_PATIENT_ID needs to be computed for every 
    # remaining negative patient in the code a few lines below
    # !!!!!!!!!
    posOrgData <- 
        tbl_df(dat_hae_1111_rf_nov26) %>% #1233
        filter(FLAG_==0) %>%
        select(-FLAG_) %>%
        filter(GENDER!='U' & REGION!='U') %>%
#         select(-matches("REGION")) %>%
        select(-PATIENT_ID) %>%
        #TEST     
#         {
#             if (bTestMode)
#             {
#                 if (any(grepl("REGION", colnames(.))))
#                     stop("Test Failed! Variables with 'REGION' in posOrgData not all removed.")
#                 .
#             } else
#                 .
#         } %>%
        #ENDTEST
        {
            vec <- rep(0, nrow(.))
            vec[.$GENDER=="M"] = 1
            mutate(., GENDER=vec)
        } %>%
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
        }
    #ENDTEST
    
    negOrgData <- 
        tbl_df(dat_nonhae_1111_rf) %>%
#         select(-matches("REGION")) %>%
        select(-HAE_PATIENT) %>%
#         #TEST     
#         {
#             if (bTestMode)
#             {
#                 if (any(grepl("REGION", colnames(.))))
#                     stop("Test Failed! Variables with 'REGION' in negOrgData not all removed.")
#                 .
#             } else
#                 .
#         } %>%
        #ENDTEST
        filter(GENDER!='U' & REGION!='U') %>%
        {
            numGender <- rep(0, nrow(.)) # convert to numerics
            numGender[.$GENDER == "M"] <- 1
            mutate(., GENDER=numGender)
        } %>%
        mutate(HAE=0) %>%
        #TEST     
        {
            if (bTestMode)
            {
                if (!any("HAE" %in% colnames(.)))
                    stop("Test Failed! HAE is not created in negOrgData yet.")
                if (any(.$HAE != 0))
                    stop("Test Failed! HAE should be all zeros for negOrgData.")
                #         if (!is.character(.$PATIENT_ID))
                #           stop("Test Failed! PATIENT_ID is not converted to character in negOrgData.")
                .
            } else
                .
        } %>%
        #ENDTEST
        mutate(LOOKBACK_DAYS=lookback_days) %>%
        select(-lookback_days)
    
    #
    ## the actual cleaning
    
    data <- 
        bind_rows(posOrgData, negOrgData) %>%
        #TEST     
        {
            print("bind_rows finished")
            if (bTestMode)
            {
                if (nrow(.) != (nrow(posOrgData) + nrow(negOrgData)))
                    stop("Test Failed! bind_rows result incorrect.")
                .
            } else
                .
        } %>%
        #ENDTEST
        
        # removing missing gender
        {
            print("Removing GENDER=='U'..");cat("\n")
            .
        } %>%
        filter(GENDER != "U") %>%
        #TEST     
        {
            if (bTestMode)
            {
                if (any(.$GENDER == "U"))
                    stop("Test Failed! Observations with GENDER 'U' are not completely removed. ")
                .
            } else
                .
        } %>%
        #ENDTEST
        
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
        } %>%
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
        } %>%
        {
            print("Saving the positive result before unmatched removal ..")
            #             write.table(. , sep=",", 
            #                         file=paste(resultDir, "CleanDataB4Matching.csv", sep=""), 
            #                         row.names=F)
            all <- .
            pos <- all %>% filter(HAE==1)
            save(pos, file=paste(resultDir, "PosCleanDataB4Matching.RData", sep=""))
            .
        } %>%
        
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
        #data2 <- data %>%
        {
            dataLastStep <- .
            posLookbacks <- 
                filter(dataLastStep, HAE==1) %>%
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
        #     {
        #       print("dim")
        #       print(dim(.))
        #       .
        #     } %>%
        #TEST 
        
        {
            if (bTestMode)
            {
                # randomly sample 100 negatives and make sure all these match positive
                dataLastStep <- .
                if (any(is.na(.$LOOKBACK_DAYS)))
                    stop("Test Failed! There shouldn't be any NAs in the matched data. ")
                negIDs <- 1:sum(.$HAE==0)
                if (length(negIDs) >= 100)
                    negIDs <- sample(negIDs)[1:100]
                poses <- .$LOOKBACK_DAYS[.$HAE == 1]
                negs <- .$LOOKBACK_DAYS[.$HAE == 0]
                for (iNeg in 1:length(negIDs))
                {
                    daysNeg <- negs[negIDs[iNeg]]
                    nPoses <- sum(.$HAE == 1)
                    bMatched <- F
                    for (iPos in 1:nPoses)
                    {
                        daysPos <- poses[iPos]
                        if ((daysNeg > (daysPos-30)) & (daysNeg < (daysPos+30)))
                        {
                            bMatched <- T
                            break
                        }
                    }
                    if (!bMatched)
                        stop("Test Failed! There's still at least one patient's lookback period not matching any positive patient.")
                }
                
                .
            } else
                .
        }  #216348    242
        #ENDTEST
        result <- data %>% 
        {
            temp <- model.matrix(~REGION, data=data
                                 , contrasts.arg = list(REGION=contrasts(as.factor(data$REGION), contrasts = F)))[, -1]
            bind_cols(as.data.frame(temp), .) 
        }%>%
            select(-REGION) #216348    245
            
        print("Saving the result..")
    write.table(result, sep=",", 
                file=paste(resultDir, "CleanDataB4Spliting.csv", sep=""), 
                row.names=F)
    
    if (bTestMode)
        print("Finished with all tests passed.")
    else
        print("Finished without testing")
    
}

result <- clean_pts(bTestMode, dataDir, resultDir) #[1] 216392    242

#step2: split 5 simulation
arglist <- list(dataDir='F:\\Jie\\Shire_follow_up\\03_Results\\for200+\\'
                , resultDir="F:\\Jie\\Shire_follow_up\\03_Results\\for200+\\" 
                , n.simu=5
)
library(caret)
split <- function(arglist)
{
    data <- read.csv(paste0(arglist$dataDir, "CleanDataB4Spliting.csv"))
    set.seed(20)
    simuId_hae <- createFolds(which(data$HAE==1), n.simu) #position of row index
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
split(arglist=arglist)



#step3: for each simulation on training+validation do   a.	Cleanings / feature selection to perform inside the evaluation / simulation loop (i.e., the initial processing need to be performed on the training + validation data, then for the test data they need to be processed according to the conclusion from the training + validation data): 
# uses the non-zero values of the positive/HAE patients part
CapPercentile <- function(vec, Hae, percentile)
{
    if ((percentile < 0) | (percentile > 100))
        stop("Error! percentile must be between 0 and 100 (inclusive). ")
    positivePatPart <- vec[Hae==1]
    nonzeroPart <- positivePatPart[positivePatPart > 0]
    if (length(nonzeroPart) == 0)
        return (vec)
    threshold <- quantile(nonzeroPart, percentile/100, type=3)
    vec[vec > threshold] <- threshold
    #   print(paste("In CapPercentile threshold:", threshold))
    #   print(paste("In CapPercentile max(vec):", max(vec)))
    return (vec)
}

CapPercentile2 <- function(vec, Hae, percentile)
{
    if ((percentile < 0) | (percentile > 100))
        stop("Error! percentile must be between 0 and 100 (inclusive). ")
    positivePatPart <- vec[Hae==1]
    nonzeroPart <- positivePatPart[positivePatPart > 0]
    
    threshold <- quantile(nonzeroPart, percentile/100, type=3)
    #vec[vec > threshold] <- threshold
    #   print(paste("In CapPercentile threshold:", threshold))
    #   print(paste("In CapPercentile max(vec):", max(vec)))
    return (threshold)
}

clean2 <- function(dataDir='F:\\Jie\\Shire_follow_up\\03_Results\\for200+\\'
                   , resultDir="F:\\Jie\\Shire_follow_up\\03_Results\\for200+\\" 
                   , i)
{
    load(paste0(dataDir, 'splitedDataB4clean2.RData'))
    #data <- arglist$data[[i]] #[1] 43280    86
    #save test data for simulation i
    ts <- dataSplited[[i]]$ts
    save(ts, file=paste0(resultDir, 'splitedDataB4ModelTsSim', i, '.RData'))
    dataB4Model <- dataSplited[[i]]$tr_vl %>%
        # remove constant columns
    {
        dataLastStep <- .
        levels <- sapply(., function(x)length(table(x)))
        temp2 <- .[, levels>1]
        print(paste("Number of constant variables:", ncol(dataLastStep)-ncol(temp2)))
        temp2
    } %>%
        
        #how about the collinear variables?
        #     i.	Collinear variable removal (for example, whether a variable needs to be removed is determined 
        #           using only the training + validation data, then the test data need to remove the corresponding variables);
        
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
        #ENDTEST
        
        # cap variables with extreme values selected by Nadea
        #     ii.	Cap extreme values;
        
    {
        print("Capping extreme values..");cat("\n")
        .
    } %>%
    {
        #       vars2Cap <- read.csv("vars2cap.csv", header=F, 
        #                             sep=",", colClasses="character")
        vars2Cap1 <- colnames(.)[grepl("_AFREQ", colnames(.))]
        #       print(paste("num of vars2Cap:", length(vars2Cap)))
        #       print("colnames(.):")
        #       print(colnames(.))
        flag <- sapply(.[, vars2Cap1], function(x)length(x[.$HAE==1][x[.$HAE==1]>0])>0)
        vars2Cap <- vars2Cap1[flag]
        dataLastStep <- .
        if (bTestMode)
            dataB4Capping <<- .
        #       apply(select(., one_of(vars2Cap[,1])), 
        #             2, PrintMaxAndPercentile, percentile=99)
        
        # compute 99% percentile
        capping_vct_onPos <<- sapply(dataLastStep, function(x)CapPercentile2(x, Hae=dataLastStep$HAE, percentile=99))
        names(capping_vct_onPos) <- gsub("(.+)(\\.99%)", "\\1", names(capping_vct_onPos), perl=T)
        
        save(capping_vct_onPos, file=paste0(resultDir, 'capping_vct_onPos.RData'))
        dataMutated <- 
            mutate_each(dataLastStep, funs(CapPercentile(., Hae=dataLastStep$HAE, percentile=99)), 
                        one_of(vars2Cap))
        # print(paste("after mutate max(ER_AFREQ):", max(dataMutated$ER_AFREQ)))
        # print("after mutation")
        #       apply(select(dataMutated, one_of(vars2Cap[,1])), 
        #             2, PrintMaxAndPercentile, percentile=99)
        dataMutated
    } %>%
        #TEST     
    {
        if (bTestMode)
        {
            afreqVars <- colnames(dataB4Capping)[grepl("_AFREQ", colnames(dataB4Capping))]
            for (iVar in 1:length(afreqVars))
            {
                positivePart <- dataB4Capping[[afreqVars[iVar]]][dataB4Capping$HAE==1]
                nonzeroPart <- positivePart[positivePart > 0]
                if (length(nonzeroPart) == 0)
                    next
                threshold <- quantile(nonzeroPart, 99/100, type=3)
                currentVar <- afreqVars[iVar]
                if (any(.[[afreqVars[iVar]]] > threshold))
                    stop(paste("Test Failed! Variable", afreqVars[iVar], "is not capped properly."))
            }
            if (F)
                stop("Test Failed! The final rule of extreme value capping is not determined yet. ")
            .
        } else
            .
    } %>% 
    {
        trVl <- .
        if(nrow(ts)+nrow(trVl)!=216348){
            stop("the splited rowNum is not correct!\n")
        }
        save(trVl, file=paste0(resultDir, 'splitedDataB4ModelTrVlSim', i, '.RData'))
        cat('trVl data has been saved !\n', 'and the nrow is', nrow(trVl), '!\n')
    }
}#end of clean2

#step4: normalize splited data after clean2 and save the min and range matrics for later ussage on corresponding test data
#scale_metrics <<- list()
library(plyr)
normlize <- function(dataDir, resultDir, i)
{
    load(paste0(dataDir, 'splitedDataB4ModelTrVlSim', i, '.RData')) ##split_result
    load(paste0(dataDir, 'splitedDataB4ModelTsSim', i, '.RData'))
    min <- apply(trVl, 2, min, na.rm=T)
    range <- apply(trVl, 2, function(x)max(x, na.rm=T)-min(x, na.rm=T))
    #scale_metrics[[i]] <<- data.frame(var=names(trVl), min=min, range=range)
    trVl_nm <- apply(trVl, 2, function(x)(x-min(x))/(max(x, na.rm=T)-min(x, na.rm=T)))
    temp1 <- lapply(colnames(ts), function(v){
        nm <- as.numeric(as.matrix((ts[, v]-min[match(v, names(trVl))])/range[match(v, names(trVl))]))
        return(nm)
    })
    ts_nm <- t(ldply(temp1, quickdf))
    colnames(ts_nm) <- names(ts)
    save(trVl_nm, file=paste0(dataDir, 'splitedDataB4ModelTrVlNormSim', i, '.RData'))
    save(ts_nm, file=paste0(dataDir, 'splitedDataB4ModelTsNormSim', i, '.RData'))
    norm_metrics <- data.frame(min=min, range=range)
    write.csv(norm_metrics, paste0(dataDir, 'norm_matrics_sim', i, '.csv'), row.names = T)
    
}
