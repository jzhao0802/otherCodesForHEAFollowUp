
# Apr15

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

run_split(n.simu=5, haeFile=973
          , nonhaeFile='for_new200K'
          , dataDir="F:\\Jie\\Shire_follow_up\\01_data\\"
          , outDir="F:\\Jie\\Shire_follow_up\\03_Results\\Lichao_test\\"
          , split_fun=3
)


run_bagging_rf(n.simu = 5, 
               wk_dir = wk_dir, 
               outDir ="F:\\Jie\\Shire_follow_up\\03_Results\\Lichao_test\\for_new200K&973\\", 
               lasso_rf_iters = 20)



perf_new300_iter20 <- 
    get_perf_allSimu(outdir="F:\\Jie\\Shire_follow_up\\03_Results\\Lichao_test\\for_new200K&973\\"
                     , iters=20
                     , n.simu=5
                     , recall_tar=seq(0.5, 0.05, -0.05)
    )

t0 <- proc.time()
perf_new200K_973_on3M_20 <- run_perf_3M(outDir="F:\\Jie\\Shire_follow_up\\03_Results\\Lichao_test\\for_new200K&973\\"
                                         , wk_dir=wk_dir
                                         , lasso_rf_iters=20
                                         , n.simu=5
                                         , recall_tar=seq(0.5, 0.05, -0.05)
                                         , fileNm_3M='neg_3M_clean2335697'
                                         , path_3M='F:\\Jie\\Shire_follow_up\\01_data\\newdata_200K_3M\\'
)
cat((proc.time()-t0)[3]/60, 'min!\n')
write.csv(perf_new200K_973_on3M_20
          , "F:\\Jie\\Shire_follow_up\\03_Results\\Lichao_test\\for_new200K&973\\iters=20\\perf_on3M.csv")

t0 <- proc.time()
 run_perf_3M_forPRcurve(outDir="F:\\Jie\\Shire_follow_up\\03_Results\\for_new200K&973\\"
                                        , wk_dir=wk_dir
                                        , lasso_rf_iters=20
                                        , n.simu=5
                                        , recall_tar=seq(0.5, 0.05, -0.05)
                                        , fileNm_3M='neg_3M_clean2335697'
                                        , path_3M='F:\\Jie\\Shire_follow_up\\01_data\\newdata_200K_3M\\'
)
cat((proc.time()-t0)[3]/60, 'min!\n')


temp <- get_perf_allSimu_forPRcurve(outdir="F:\\Jie\\Shire_follow_up\\03_Results\\for_new200K&973\\"
                                    , iters=20
                                    , n.simu=5
                                    , recall_tar=seq(0.5, 0.05, -0.05)
)


