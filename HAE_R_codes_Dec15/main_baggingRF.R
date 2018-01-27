# main function for running bagging forest and performance
rm(list=ls())
# R work directory
wk_dir = "F:\\Jie\\Shire_follow_up\\02_Code\\HAE_R_codes_Dec15\\"

# Setup R work directory
setwd(wk_dir)
# Load R packages
source("loadpackage.R")
# Auxiliary functions
source("auxfunctions.R")
# 
source("funs_baggingRF.R")

split_simu(simu=1, nonhaeFile='new_200K'
           , outDir="F:\\Jie\\Shire_follow_up\\03_Results\\for_new200K\\")
split_simu(simu=2, nonhaeFile='new_200K'
           , outDir="F:\\Jie\\Shire_follow_up\\03_Results\\for_new200K\\")
split_simu(simu=3, nonhaeFile='new_200K'
           , outDir="F:\\Jie\\Shire_follow_up\\03_Results\\for_new200K\\")
split_simu(simu=4, nonhaeFile='new_200K'
           , outDir="F:\\Jie\\Shire_follow_up\\03_Results\\for_new200K\\")
split_simu(simu=5, nonhaeFile='new_200K'
           , outDir="F:\\Jie\\Shire_follow_up\\03_Results\\for_new200K\\")


run_bagging_rf(n.simu=5
               , outDir="F:\\Jie\\Shire_follow_up\\03_Results\\for_new200K\\"
               , lasso_rf_iters=20
               )

# run_bagging_rf(n.simu=5
#                , outDir="D:\\jzhao\\Shire_followup\\03_Results\\for_new200K\\"
#                , lasso_rf_iters=5
# )
# 
# run_bagging_rf(n.simu=5
#                , outDir="D:\\jzhao\\Shire_followup\\03_Results\\for_new200K\\"
#                , lasso_rf_iters=1
# )
# Setup R work directory
setwd(wk_dir)
# Load R packages
source("loadpackage.R")
# Auxiliary functions
source("auxfunctions.R")
source("auxfunctions.R")
source("funs_baggingRF.R")
perf_new200_iter20 <- get_perf_allSimu(outdir="F:\\Jie\\Shire_follow_up\\03_Results\\for_new200K\\"
                                       , iters=20
                                       , n.simu=5
                                       , recall_tar=seq(0.5, 0.05, -0.05)
                                       )

perf_new200_iter5 <- get_perf_allSimu(outdir="F:\\Jie\\Shire_follow_up\\03_Results\\for_new200K\\"
                                       , iters=5
                                       , n.simu=5
                                       , recall_tar=seq(0.5, 0.05, -0.05)
)

perf_new200_iter1 <- get_perf_allSimu(outdir="F:\\Jie\\Shire_follow_up\\03_Results\\for_new200K\\"
                                      , iters=1
                                      , n.simu=5
                                      , recall_tar=seq(0.5, 0.05, -0.05)
)

perf_new200 <- data.frame(perf_new200_iter20=perf_new200_iter20
           , perf_new200_iter5=perf_new200_iter5
           , perf_new200_iter1=perf_new200_iter1
           )

write.csv(perf_new200, paste0("F:\\Jie\\Shire_follow_up\\03_Results\\for_new200K\\perf_baggingRF.csv"))


# for Dong's 200K & 1233 + iters=200
split_simu2(simu=1, nonhaeFile='old_200K&1233'
           , outDir="F:\\Jie\\Shire_follow_up\\03_Results\\for_old200K&1233\\")
split_simu2(simu=2, nonhaeFile='old_200K&1233'
           , outDir="F:\\Jie\\Shire_follow_up\\03_Results\\for_old200K&1233\\")
split_simu2(simu=3, nonhaeFile='old_200K&1233'
           , outDir="F:\\Jie\\Shire_follow_up\\03_Results\\for_old200K&1233\\")
split_simu2(simu=4, nonhaeFile='old_200K&1233'
           , outDir="F:\\Jie\\Shire_follow_up\\03_Results\\for_old200K&1233\\")
split_simu2(simu=5, nonhaeFile='old_200K&1233'
           , outDir="F:\\Jie\\Shire_follow_up\\03_Results\\for_old200K&1233\\")


run_bagging_rf(n.simu=5
               , outDir="F:\\Jie\\Shire_follow_up\\03_Results\\for_old200K&1233\\"#using patients and featues all the same as Dong used
               , lasso_rf_iters=200
)
perf_old200_1233_iter200 <- get_perf_allSimu(outdir="F:\\Jie\\Shire_follow_up\\03_Results\\for_old200K&1233\\"
                                       , iters=200
                                       , n.simu=5
                                       , recall_tar=seq(0.5, 0.05, -0.05)
)


write.csv(perf_old200_1233_iter200, paste0("F:\\Jie\\Shire_follow_up\\03_Results\\for_old200K&1233\\perf_baggingRF.csv"))



# for Dong's 200K & 1233/973(but with feataure changed below in ordert to apply the model to 3M(acturally ~2.3M after cleaning))
# 1. remove region
# 2. use GENDERM to replace GENDERU
# split_simu3(simu=1, nonhaeFile='for_old200K', haeFile=1233
#             , outDir="F:\\Jie\\Shire_follow_up\\03_Results\\")
# split_simu3(simu=2, nonhaeFile='for_old200K', haeFile=1233
#             , outDir="F:\\Jie\\Shire_follow_up\\03_Results\\")
# split_simu3(simu=3, nonhaeFile='for_old200K', haeFile=1233
#             , outDir="F:\\Jie\\Shire_follow_up\\03_Results\\")
# split_simu3(simu=4, nonhaeFile='for_old200K', haeFile=1233
#             , outDir="F:\\Jie\\Shire_follow_up\\03_Results\\")
# split_simu3(simu=5, nonhaeFile='for_old200K', haeFile=1233
#             , outDir="F:\\Jie\\Shire_follow_up\\03_Results\\")

run_split(n.simu=5, nonhaeFile='for_old200K', haeFile=1087
          , outDir="F:\\Jie\\Shire_follow_up\\03_Results\\", split_fun=3)
run_split(n.simu=5, nonhaeFile='for_new200K', haeFile=1087
          , outDir="F:\\Jie\\Shire_follow_up\\03_Results\\", split_fun=3)

t0 <- proc.time()
run_bagging_rf(n.simu=5
               , outDir="F:\\Jie\\Shire_follow_up\\03_Results\\for_old200K&1087\\"
               , lasso_rf_iters=20
)
cat((proc.time()-t0)[3]/60, '!\n')
run_bagging_rf(n.simu=5
               , outDir="F:\\Jie\\Shire_follow_up\\03_Results\\for_old200K&1087\\"
               , lasso_rf_iters=1
)

perf_old200_1233_iter200 <- get_perf_allSimu(outdir="F:\\Jie\\Shire_follow_up\\03_Results\\for_old200K&1233\\"
                                             , iters=200
                                             , n.simu=5
                                             , recall_tar=seq(0.5, 0.05, -0.05)
)


write.csv(perf_old200_1233_iter200, paste0("F:\\Jie\\Shire_follow_up\\03_Results\\for_old200K&1233\\perf_baggingRF.csv"))















# main function for running bagging forest and performance
rm(list=ls())
# R work directory
wk_dir = "F:\\Jie\\Shire_follow_up\\02_Code\\HAE_R_codes_Dec15\\"

# Setup R work directory
setwd(wk_dir)
# Load R packages
source("loadpackage.R")
# Auxiliary functions
source("auxfunctions.R")
# 
source("funs_baggingRF.R")

t0 <- proc.time()
perf_old200K_1233_on3M <- run_perf_3M(outDir="F:\\Jie\\Shire_follow_up\\03_Results\\for_old200K&1233\\"
                                      , lasso_rf_iters=200
                                      , n.simu=5
                                      , recall_tar=seq(0.5, 0.05, -0.05)
                                      , fileNm_3M='neg_3M_clean2335697'
                                      , path_3M='F:\\Jie\\Shire_follow_up\\01_data\\newdata_200K_3M\\'
                                      )
cat((proc.time()-t0)[3]/60, 'min!\n')
# 898.5297 min!

t0 <- proc.time()
perf_old200K_1233_on3M_20 <- run_perf_3M(outDir="F:\\Jie\\Shire_follow_up\\03_Results\\for_old200K&1233\\"
                                      , lasso_rf_iters=20
                                      , n.simu=5
                                      , recall_tar=seq(0.5, 0.05, -0.05)
                                      , fileNm_3M='neg_3M_clean2335697'
                                      , path_3M='F:\\Jie\\Shire_follow_up\\01_data\\newdata_200K_3M\\'
)
cat((proc.time()-t0)[3]/60, 'min!\n')

t0 <- proc.time()
perf_old200K_1233_on3M_1 <- run_perf_3M(outDir="F:\\Jie\\Shire_follow_up\\03_Results\\for_old200K&1233\\"
                                      , lasso_rf_iters=1
                                      , n.simu=5
                                      , recall_tar=seq(0.5, 0.05, -0.05)
                                      , fileNm_3M='neg_3M_clean2335697'
                                      , path_3M='F:\\Jie\\Shire_follow_up\\01_data\\newdata_200K_3M\\'
)
cat((proc.time()-t0)[3]/60, 'min!\n')


avg_ppv_0.05_old200K_1233 <- as.data.frame(cbind(iters=rep(c(200, 20, 1), each=5), rbind(perf_old200K_1233_on3M, perf_old200K_1233_on3M_20, perf_old200K_1233_on3M_1))) %>%
    aggregate(.~iters , data=., mean) 


t0 <- proc.time()
perf_old200K_973_on3M_20 <- run_perf_3M(outDir="F:\\Jie\\Shire_follow_up\\03_Results\\for_old200K&973\\"
                                        , lasso_rf_iters=20
                                        , n.simu=5
                                        , recall_tar=seq(0.5, 0.05, -0.05)
                                        , fileNm_3M='neg_3M_clean2335697'
                                        , path_3M='F:\\Jie\\Shire_follow_up\\01_data\\newdata_200K_3M\\'
)
cat((proc.time()-t0)[3]/60, 'min!\n')
write.csv(perf_old200K_973_on3M_20, "F:\\Jie\\Shire_follow_up\\03_Results\\for_old200K&973\\iters=20\\perf_on5Simu.csv")
# 112.4527 min!

t0 <- proc.time()
perf_old200K_973_on3M_1 <- run_perf_3M(outDir="F:\\Jie\\Shire_follow_up\\03_Results\\for_old200K&973\\"
                                        , lasso_rf_iters=1
                                        , n.simu=5
                                        , recall_tar=seq(0.5, 0.05, -0.05)
                                        , fileNm_3M='neg_3M_clean2335697'
                                        , path_3M='F:\\Jie\\Shire_follow_up\\01_data\\newdata_200K_3M\\'
)
cat((proc.time()-t0)[3]/60, 'min!\n')
write.csv(perf_old200K_973_on3M_1, "F:\\Jie\\Shire_follow_up\\03_Results\\for_old200K&973\\iters=1\\perf_on5Simu.csv")
# 10.88833 min!

avg_ppv_0.05_old200K_973 <- as.data.frame(cbind(iters=rep(c(20, 1), each=5), rbind(perf_old200K_973_on3M_20, perf_old200K_973_on3M_1))) %>%
    aggregate(.~iters , data=., mean) 



# Apr11
t0 <- proc.time()
perf_old200K_1087_on3M_20 <- run_perf_3M(outDir="F:\\Jie\\Shire_follow_up\\03_Results\\for_old200K&1087\\"
                                        , lasso_rf_iters=20
                                        , n.simu=5
                                        , recall_tar=seq(0.5, 0.05, -0.05)
                                        , fileNm_3M='neg_3M_clean2335697'
                                        , path_3M='F:\\Jie\\Shire_follow_up\\01_data\\newdata_200K_3M\\'
)
cat((proc.time()-t0)[3]/60, 'min!\n')
write.csv(perf_old200K_1087_on3M_20, "F:\\Jie\\Shire_follow_up\\03_Results\\for_old200K&1087\\iters=20\\perf_on5Simu.csv")
# 112.4527 min!

t0 <- proc.time()
perf_new200K_1087_on3M_20 <- run_perf_3M(outDir="F:\\Jie\\Shire_follow_up\\03_Results\\for_new200K&1087\\"
                                       , lasso_rf_iters=20
                                       , n.simu=5
                                       , recall_tar=seq(0.5, 0.05, -0.05)
                                       , fileNm_3M='neg_3M_clean2335697'
                                       , path_3M='F:\\Jie\\Shire_follow_up\\01_data\\newdata_200K_3M\\'
)
cat((proc.time()-t0)[3]/60, 'min!\n')
write.csv(perf_new200K_1087_on3M_20, "F:\\Jie\\Shire_follow_up\\03_Results\\for_new200K&1087\\iters=20\\perf_on5Simu.csv")
# 10.88833 min!

t0 <- proc.time()
perf_old200K_1087_on3M_1 <- run_perf_3M(outDir="F:\\Jie\\Shire_follow_up\\03_Results\\for_old200K&1087\\"
                                         , lasso_rf_iters=1
                                         , n.simu=5
                                         , recall_tar=seq(0.5, 0.05, -0.05)
                                         , fileNm_3M='neg_3M_clean2335697'
                                         , path_3M='F:\\Jie\\Shire_follow_up\\01_data\\newdata_200K_3M\\'
)
cat((proc.time()-t0)[3]/60, 'min!\n')
write.csv(perf_old200K_1087_on3M_1, "F:\\Jie\\Shire_follow_up\\03_Results\\for_old200K&1087\\iters=1\\perf_on5Simu.csv")
# 112.4527 min!

t0 <- proc.time()
perf_new200K_1087_on3M_1 <- run_perf_3M(outDir="F:\\Jie\\Shire_follow_up\\03_Results\\for_new200K&1087\\"
                                         , lasso_rf_iters=1
                                         , n.simu=5
                                         , recall_tar=seq(0.5, 0.05, -0.05)
                                         , fileNm_3M='neg_3M_clean2335697'
                                         , path_3M='F:\\Jie\\Shire_follow_up\\01_data\\newdata_200K_3M\\'
)
cat((proc.time()-t0)[3]/60, 'min!\n')
write.csv(perf_new200K_1087_on3M_1, "F:\\Jie\\Shire_follow_up\\03_Results\\for_new200K&1087\\iters=1\\perf_on5Simu.csv")
# 10.88833 min!


avg_ppv_0.05_200K_1087 <- as.data.frame(cbind(iters=rep(c(20, 1), each=5), rbind(perf_old200K_973_on3M_20, perf_old200K_973_on3M_1))) %>%
    aggregate(.~iters , data=., mean) 


perf_old200_iter200_973 <- 
    get_perf_allSimu(outdir="F:\\Jie\\Shire_follow_up\\03_Results\\for_old200K&973\\"
                     , iters=200
                     , n.simu=5
                     , recall_tar=seq(0.5, 0.05, -0.05)
    )
write.csv(perf_old200_iter200_973
          , "F:\\Jie\\Shire_follow_up\\03_Results\\for_old200K&973\\iters=200\\perf_on200K_tst.csv")
perf_old200_iter20_973 <- 
    get_perf_allSimu(outdir="F:\\Jie\\Shire_follow_up\\03_Results\\for_old200K&973\\"
                     , iters=20
                     , n.simu=5
                     , recall_tar=seq(0.5, 0.05, -0.05)
    )
write.csv(perf_old200_iter20_973
          , "F:\\Jie\\Shire_follow_up\\03_Results\\for_old200K&973\\iters=20\\perf_on200K_tst.csv")

perf_old200_iter1_973 <- 
    get_perf_allSimu(outdir="F:\\Jie\\Shire_follow_up\\03_Results\\for_old200K&973\\"
                     , iters=1
                     , n.simu=5
                     , recall_tar=seq(0.5, 0.05, -0.05)
    )
write.csv(perf_old200_iter1_973
          , "F:\\Jie\\Shire_follow_up\\03_Results\\for_old200K&973\\iters=1\\perf_on200K_tst.csv")

perf_old200_973 <- data.frame(perf_old200_iter200_973=perf_old200_iter200_973
                                  , perf_old200_iter20_973=perf_old200_iter20_973
                                  , perf_old200_iter1_973=perf_old200_iter1_973
                                  )
write.csv(perf_old200_973, 
          "F:\\Jie\\Shire_follow_up\\03_Results\\for_old200K&973\\perf_on200K_tst.csv")


perf_new200_iter20_973 <- 
    get_perf_allSimu(outdir="F:\\Jie\\Shire_follow_up\\03_Results\\for_new200K&973\\"
                     , iters=20
                     , n.simu=5
                     , recall_tar=seq(0.5, 0.05, -0.05)
    )
write.csv(perf_new200_iter20_973
          , "F:\\Jie\\Shire_follow_up\\03_Results\\for_new200K&973\\iters=20\\perf_on200K_tst.csv")





# Apr14

# main function for running bagging forest and performance
rm(list=ls())
# R work directory
wk_dir = "F:\\Jie\\Shire_follow_up\\02_Code\\HAE_R_codes_Dec15\\"

# Setup R work directory
setwd(wk_dir)
# Load R packages
source("loadpackage.R")
# Auxiliary functions
source("auxfunctions.R")
# 
source("funs_baggingRF.R")

# run_split(n.simu=5, haeFile=1233
#           , nonhaeFile='for_new200K'
#           , dataDir="F:\\Jie\\Shire_follow_up\\01_data\\"
#           , outDir="F:\\Jie\\Shire_follow_up\\03_Results\\nodup\\"
#           , split_fun=3
# )

run_bagging_rf(n.simu = 5, 
               wk_dir = wk_dir, 
               outDir ="F:\\Jie\\Shire_follow_up\\03_Results\\nodup\\for_new200K&1233\\", 
               lasso_rf_iters = 20)



perf_new300_iter20 <- 
    get_perf_allSimu(outdir="F:\\Jie\\Shire_follow_up\\03_Results\\nodup\\for_new200K&1233\\"
                     , iters=20
                     , n.simu=5
                     , recall_tar=seq(0.5, 0.05, -0.05)
    )

t0 <- proc.time()
perf_new200K_1233_on3M_20 <- run_perf_3M(outDir="F:\\Jie\\Shire_follow_up\\03_Results\\nodup\\for_new200K&1233\\"
                                        , wk_dir=wk_dir
                                        , lasso_rf_iters=20
                                        , n.simu=5
                                        , recall_tar=seq(0.5, 0.05, -0.05)
                                        , fileNm_3M='neg_3M_clean2335697'
                                        , path_3M='F:\\Jie\\Shire_follow_up\\01_data\\newdata_200K_3M\\'
)
cat((proc.time()-t0)[3]/60, 'min!\n')
write.csv(perf_new200K_1233_on3M_20
          , "F:\\Jie\\Shire_follow_up\\03_Results\\nodup\\for_new200K&1233\\iters=20\\perf_on3M.csv")



summarize_perf_3M <- function(outdir, iters){
    perf_all <- lapply(1:5, function(i){
        x=read.csv(paste0(outdir, "iters=", iters, "\\simu", i, "\\perf_on3M.csv"))
    })
    library(plyr)
    perf_all_df <- ldply(perf_all, quickdf)
    detach("package:plyr", unload=TRUE)
    library(dplyr)
    perf_all_df1 <- tbl_df(perf_all_df) %>% group_by(., X) %>% summarize(., mean(x))
    write.csv(perf_all_df1, paste0(outdir, "iters=", iters, "\\perf_on3M_allSimu.csv"))
    library(plyr)
}

summarize_perf_3M(outdir="F:\\Jie\\Shire_follow_up\\03_Results\\for_new200K&1233\\"
                  , iters=200)
summarize_perf_3M(outdir="F:\\Jie\\Shire_follow_up\\03_Results\\for_new200K&1233\\"
                  , iters=20)
summarize_perf_3M(outdir="F:\\Jie\\Shire_follow_up\\03_Results\\for_new200K&1233\\"
                  , iters=1)
