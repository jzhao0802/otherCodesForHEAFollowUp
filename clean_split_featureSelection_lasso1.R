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
# arglist <- list(bTestMode=F
#                 , dataDir='F:\\Lichao\\work\\Projects\\HAE\\data\\from Dong\\'
#                 , result_dir= "F:\\Jie\\Shire_follow_up\\03_Results\\for80+"
#                 )
Clean <- function(arglist)
{
    bTestMode <- arglist$bTestMode
    
    dataDir <- arglist$dataDir
    
    if (is.null(arglist$result_dir))
    {
        timeStamp <- as.character(Sys.time())
        timeStamp <- gsub(":", ".", timeStamp)  # replace ":" by "."
        resultDir <- paste("./Results/", timeStamp, "/", sep = '')
        dir.create(resultDir, showWarnings = TRUE, recursive = TRUE, mode = "0777")
    } else
        resultDir <- arglist$result_dir
    
    print("Loading and preparing data..");cat("\n")
    dat_hae_1111_rf_nov26 <- 
        read.csv("F:/Lichao/work/Projects/HAE/data/Zhenxing/dat_hae_1111_rf_nov26_flag.csv", header=T, sep=",", check.names=F)
    #   dat_nonhae_1111_rf <- 
    #     read.csv("C:/Work/Projects/HAE/data/from Dong/dat_nonhae_1111_rf.csv", 
    #              header=T, sep=",", check.names=F)
    # load(paste(dataDir, "dat_hae_1111_rf_nov26.RData", sep=""))
    load(paste(dataDir, "dat_nonhae_1111_rf.RData", sep=""))
    # load(paste(dataDir, "nonhae_tmp10000.RData", sep=""))
    # load(paste(dataDir, "Rep_Samp/adj_ppv_samp_1.RData", sep=""))
    
    
    # !!!!!!!!!
    # the following needs to be changed, as at the beginning 
    # both positives and negatives have PATIENT_ID
    # but the HAE_PATIENT_ID needs to be computed for every 
    # remaining negative patient in the code a few lines below
    # !!!!!!!!!
    posOrgData <- 
        tbl_df(dat_hae_1111_rf_nov26) %>%
        filter(FLAG_==0) %>%
        select(-FLAG_) %>%
        select(-matches("REGION")) %>%
        select(-PATIENT_ID) %>%
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
        select(-matches("REGION")) %>%
        select(-HAE_PATIENT) %>%
        #TEST     
        {
            if (bTestMode)
            {
                if (any(grepl("REGION", colnames(.))))
                    stop("Test Failed! Variables with 'REGION' in negOrgData not all removed.")
                .
            } else
                .
        } %>%
        #ENDTEST
        filter(GENDER!='U') %>%
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
    
    
    
    binonlyInits <- c("PRC_60", "PRC_64", "DIAG_32")
    
    
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
        {
            print("Saving the result before unmatched removal ..")
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
        } %>%
        #ENDTEST
        
        
        
        # final result
        
        {
            result <<- .
        }
    
    # 
    print("Saving the result..")
    write.table(result, sep=",", 
                file=paste(resultDir, "CleanDataB4Spliting.csv", sep=""), 
                row.names=F)
    
    if (bTestMode)
        print("Finished with all tests passed.")
    else
        print("Finished without testing")
}
#Clean(arglist)
#[1] 216392     86

#step2: split 5 simulation
arglist <- list(dataDir='F:\\Jie\\Shire_follow_up\\03_Results\\'
                , resultDir="F:\\Jie\\Shire_follow_up\\03_Results\\" 
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
#split(arglist=arglist)


#step3: for each simulation on training+validation do   a.	Cleanings / feature selection to perform inside the evaluation / simulation loop (i.e., the initial processing need to be performed on the training + validation data, then for the test data they need to be processed according to the conclusion from the training + validation data): 

clean2 <- function(dataDir='F:\\Jie\\Shire_follow_up\\03_Results\\'
                   , resultDir="F:\\Jie\\Shire_follow_up\\03_Results\\" 
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
        
            {
                print("Removing collinear variables..");cat("\n")
                .
            } %>%
            {
                file_corr <- file(paste(resultDir, "colinear_vars.txt", sep=""), "w")
                threshold <- 0.8
                if (bTestMode)
                    collThreshold <<- threshold
                writeLines(paste("Correlation threshold:", threshold), file_corr)
                dataLastStep <- data.matrix(.)
                # cor matrix
                corrMat <- cor(dataLastStep, dataLastStep)
                corrMat <- corrMat - diag(ncol(corrMat))
                # remove one variable of the correlated pair
                vars2Remove <- NULL
                for (iVar in 1:(ncol(dataLastStep)-1))
                {
                    thisVar <- colnames(dataLastStep)[iVar]
                    if (thisVar %in% vars2Remove)
                        next
                    for (jVar in (iVar+1):ncol(dataLastStep))
                    {
                        if (colnames(dataLastStep)[jVar] %in% vars2Remove)
                            next
                        if (abs(corrMat[iVar, jVar]) >= threshold)
                        {
                            vars2Remove <- c(vars2Remove, colnames(dataLastStep)[jVar])
                            writeLines(paste(colnames(dataLastStep)[iVar], 
                                             "is correlated with", 
                                             colnames(dataLastStep)[jVar], 
                                             ":", corrMat[iVar, jVar]), file_corr)
                        }
                        
                    }
                }
                
                writeLines("vars2Remove due to collinearity:", file_corr)
                writeLines(paste(vars2Remove,collapse=","), file_corr)
                
                close(file_corr)
                if (!is.null(vars2Remove))
                    select(., -one_of(vars2Remove))
                else
                    .
            } %>%
            #TEST     
            {
                if (bTestMode)
                {
                    dataLastStep <- data.matrix(.)
                    corrMat <- cor(dataLastStep, dataLastStep)
                    corrMat <- corrMat - diag(ncol(corrMat))
                    if (max(abs(corrMat)) >= collThreshold)
                        stop("Test Failed! Colliear variables are not completely removed.")
                    .
                } else
                    .
            } %>%
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
                vars2Cap <- colnames(.)[grepl("_AFREQ", colnames(.))]
                #       print(paste("num of vars2Cap:", length(vars2Cap)))
                #       print("colnames(.):")
                #       print(colnames(.))
                
                dataLastStep <- .
                if (bTestMode)
                    dataB4Capping <<- .
                #       apply(select(., one_of(vars2Cap[,1])), 
                #             2, PrintMaxAndPercentile, percentile=99)
                
                # compute 99% percentile
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
                if(nrow(ts)+nrow(trVl)!=216392){
                    stop("the splited rowNum is not correct!\n")
                }
                save(trVl, file=paste0(resultDir, 'splitedDataB4ModelTrVlSim', i, '.RData'))
                cat('trVl data has been saved !\n', 'and the nrow is', nrow(trVl), '!\n')
            }
}#end of split

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

#step5: split data into 5 folds for cv

split_cv <- function(dataDir='F:\\Jie\\Shire_follow_up\\03_Results\\'
                  , resultDir="F:\\Jie\\Shire_follow_up\\03_Results\\" 
                  , n.simu=5
                  , n.folds=5
                  , norm=T)
{   
    for(i in 1:n.simu){
        if(norm==T){
            load(paste0(dataDir, "splitedDataB4ModelTrVlNormSim", i, ".RData"))
            trVl <- as.data.frame(trVl_nm)
            rm(trVl_nm)
        }else{
            
            load(paste0(dataDir, "splitedDataB4ModelTrVlSim", i, ".RData"))   
        }
        for(j in 1:n.folds){
            set.seed(20)
            foldid <- nrow(trVl)
            foldid[trVl$HAE==1] <- sample(rep(1:n.folds, length=length(which(trVl$HAE==1))))
            foldid[trVl$HAE==0] <- sample(rep(1:n.folds, length=length(which(trVl$HAE==0))))
            table(trVl$HAE, foldid)
            cv_ts <- trVl[foldid==j, ]
            cv_tr <- trVl[foldid!=j, ]
            cv_trTs <- list(cv_tr=cv_tr, cv_ts=cv_ts)
            if(norm==T){
                
                save(cv_trTs, file=paste0(resultDir, 'trVlNormSim', i, '_cv', j, '.RData'))   
            }else{
                save(cv_trTs, file=paste0(resultDir, 'trVlSim', i, '_cv', j, '.RData'))    
            }
            
        }
    }
    
    
    cat("\nsplited cv data saved successfully!\n")
    
}

#step5: Lasso

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
        return(aupr)
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
    trTs <- trTs[, -match("LOOKBACK_DAYS", colnames(trTs))] #remove lookback_days
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
    save(lambda_seq, optN, total_model, file=paste0(modelDir,"//total_model_simu", i, ".RData"))
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
    write.csv(model_output, file = paste0(modelDir, '//model_coefficient_sim', i, '.csv'))
    
    #performance on test data
    
    
    #apply the optimum weight and lambda to the test data
    pred_ts<- predict(total_model, ts_matrix, type="response")[,optN]
    cat(file=traceFile, append=TRUE, 'simulation ', i, ': test_pred end!\n') #added by Jie
    return(pred_ts)    
    
}





#run the main function for cv lasso on simulations

run_lasso <- function(resultDir, dataDir, norm, n.folds, wt, crit, Btest, crit_nm){
    modelDir <- paste0(resultDir, 'Lasso_', crit_nm, '//wt=', wt, '_norm=', norm)
    if(!dir.exists(modelDir)) dir.create(modelDir, showWarnings = T, recursive = TRUE)
    traceFile <- paste0(modelDir, '//traceFile.csv')

    #n.sim parallele running start!
    cat(file=traceFile, append=T, 'parallele on n.simu simulation start!\n')
    
    sfInit(parallel=TRUE, cpus=n.simu, type='SOCK')
    #sfSource("F:\\Jie\\Shire\\03_code\\subFunc_v3.R")
    sfSource("F:\\Jie\\Shire_follow_up\\02_Code\\clean_split_featureSelection_lasso.R")
    
    cat(file=traceFile, append=TRUE, 'n.simu simulations parallel sfExport running!\n')
    sfExport( 'crit', 'traceFile', 'dataDir', 'modelDir', 'n.folds', "wt", "crit_nm")
    sfExport('createCurve_v2', 'par_cv', 'par_lasso_simu')
    sfClusterEval(library("glmnet"))
    sfClusterEval(library("ROCR"))
    sfClusterEval(library("plyr"))
    sfClusterEval(library("dplyr"))
    sfClusterEval(library("snowfall"))
    sfClusterEval(library("caTools"))
    
    pred_ts_allSim <- sfClusterApplyLB(1:n.simu, par_lasso_simu, norm, Btest)
    save(pred_ts_allSim, file=paste0(modelDir, '//pred_allSim.RData'))
    sfStop()
    cat(unlist(lapply(pred_ts_allSim, length)), '\n')
    
}

#summarize the performance on test
summarize_perf <- function(resultDir, n.simu, target_recall, wt, norm, crit_nm){
    modelDir <- paste0(resultDir, 'Lasso_', crit_nm, '//wt=', wt, '_norm=', norm)
    load(paste0(modelDir, '//pred_allSim.RData'))
    perf_list <- lapply(1:n.simu, function(i){
        pred_ts <- pred_ts_allSim[[i]]
        load(paste0(modelDir, '//resp_ts_sim', i, '.RData')) #response_ts
        temp1 <- msOnTest_sep_v2(pred_ts, response_ts, target_recall)
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

