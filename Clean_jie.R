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

arglist <- list(bTestMode=F, dataDir='F:\\Lichao\\work\\Projects\\HAE\\data\\from Dong\\')
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
     
    
  
  
    # how about the collinear variables?
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
#ENDTEST
    
    
    
    # final result
    
    {
      result <<- .
    }
  
  # 
  print("Saving the result..")
  write.table(result, sep=",", 
              file=paste(resultDir, "CleanDataB4Similarity.csv", sep=""), 
              row.names=F)
  
  if (bTestMode)
    print("Finished with all tests passed.")
  else
    print("Finished without testing")
}