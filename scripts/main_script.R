numpar = 1000

library(orderly)

## Format and extract data from SSuN files
id1 <- orderly::orderly_run("datamaker", parameters = list(npar = numpar), use_draft = FALSE)
orderly::orderly_commit(id1)

#main
#two alpha scenarios
#two ncasual scenarios
#two nstable scenarios
#two QALY vals scenarios
#two act efficacy scenarios
#seix relsample scenarios

paramsdf = data.frame(
  alpha_msm_min = rep(0,17),
  alpha_msm_max = rep(1,17),
  alpha_msw_min = rep(0,17),
  alpha_msw_max = rep(1,17),
  alpha_women_min = rep(0,17),
  alpha_women_max = rep(1,17),
  ncasual_msm_min = rep(0,17),
  ncasual_msm_max = rep(1,17),
  ncasual_msw_min = rep(0,17),
  ncasual_msw_max = rep(1,17),
  ncasual_women_min = rep(0,17),
  ncasual_women_max = rep(1,17),
  nstable_msm_min = rep(0,17),
  nstable_msm_max = rep(1,17),
  nstable_msw_min = rep(0,17),
  nstable_msw_max = rep(1,17),
  nstable_women_min = rep(0,17),
  nstable_women_max = rep(1,17),
  QALYvals_men_min = rep(0,17),
  QALYvals_men_max = rep(1,17),
  QALYvals_women_min = rep(0,17),
  QALYvals_women_max = rep(1,17),
  peractefficacy = rep(0.6,17),
  relsample_HIV = rep(1, 17),
  relsample_PrEP = rep(1, 17),
  relsample_history = rep(1,17)
  )

paramsdf$alpha_msm_min[2] = 0.2
paramsdf$alpha_msm_max[2] = 0.2
paramsdf$alpha_msw_min[2] = 0.2
paramsdf$alpha_msw_max[2] = 0.2
paramsdf$alpha_women_min[2] = 0.5
paramsdf$alpha_women_max[2] = 0.5
paramsdf$alpha_msm_min[3] = 0.55
paramsdf$alpha_msm_max[3] = 0.55
paramsdf$alpha_msw_min[3] = 0.5
paramsdf$alpha_msw_max[3] = 0.5
paramsdf$alpha_women_min[3] = 0.7
paramsdf$alpha_women_max[3] = 0.7
paramsdf$ncasual_msm_min[4] = 1
paramsdf$ncasual_msm_max[4] = 1
paramsdf$ncasual_msw_min[4] = 1
paramsdf$ncasual_msw_max[4] = 1
paramsdf$ncasual_women_min[4] = 1
paramsdf$ncasual_women_max[4] = 1
paramsdf$ncasual_msm_min[5] = 7
paramsdf$ncasual_msm_max[5] = 7
paramsdf$ncasual_msw_min[5] = 7
paramsdf$ncasual_msw_max[5] = 7
paramsdf$ncasual_women_min[5] = 7
paramsdf$ncasual_women_max[5] = 7
paramsdf$nstable_msm_min[6] = 12
paramsdf$nstable_msm_max[6] = 12
paramsdf$nstable_msw_min[6] = 12
paramsdf$nstable_msw_max[6] = 12
paramsdf$nstable_women_min[6] = 7
paramsdf$nstable_women_max[6] = 7
paramsdf$nstable_msm_min[7] = 36
paramsdf$nstable_msm_max[7] = 36
paramsdf$nstable_msw_min[7] = 36
paramsdf$nstable_msw_max[7] = 36
paramsdf$nstable_women_min[7] = 36
paramsdf$nstable_women_max[7] = 36
paramsdf$QALYvals_men_min[8] = 0.19
paramsdf$QALYvals_men_max[8] = 0.19
paramsdf$QALYvals_women_min[8] = 3.26
paramsdf$QALYvals_women_max[8] = 3.26
paramsdf$QALYvals_men_min[9] = 5.48
paramsdf$QALYvals_men_max[9] = 5.48
paramsdf$QALYvals_women_min[9] = 39.14
paramsdf$QALYvals_women_max[9] = 39.14
paramsdf$peractefficacy[10] = 0.4
paramsdf$peractefficacy[11] = 0.8
paramsdf$relsample_HIV[12] = 0.5
paramsdf$relsample_HIV[13] = 1.5
paramsdf$relsample_PrEP[14] = 0.5
paramsdf$relsample_PrEP[15] = 1.5
paramsdf$relsample_history[16] = 0.5
paramsdf$relsample_history[17] = 1.5



# Sample parameters
for (k in c(1:17)){
  id1 <- orderly::orderly_run("modelsample", parameters = list(npar = numpar, OverallDiagnoses = 485694,
                                                               alpha_msm_min = paramsdf$alpha_msm_min[k],
                                                               alpha_msm_max = paramsdf$alpha_msm_max[k],
                                                               alpha_msw_min = paramsdf$alpha_msw_min[k],
                                                               alpha_msw_max = paramsdf$alpha_msw_max[k],
                                                               alpha_women_min = paramsdf$alpha_women_min[k],
                                                               alpha_women_max = paramsdf$alpha_women_max[k],
                                                               ncasual_msm_min = paramsdf$ncasual_msm_min[k],
                                                               ncasual_msm_max = paramsdf$ncasual_msm_max[k],
                                                               ncasual_msw_min = paramsdf$ncasual_msw_min[k],
                                                               ncasual_msw_max = paramsdf$ncasual_msw_max[k],
                                                               ncasual_women_min = paramsdf$ncasual_women_min[k],
                                                               ncasual_women_max = paramsdf$ncasual_women_max[k],
                                                               nstable_msm_min = paramsdf$nstable_msm_min[k],
                                                               nstable_msm_max = paramsdf$nstable_msm_max[k],
                                                               nstable_msw_min = paramsdf$nstable_msw_min[k],
                                                               nstable_msw_max = paramsdf$nstable_msw_max[k],
                                                               nstable_women_min = paramsdf$nstable_women_min[k],
                                                               nstable_women_max = paramsdf$nstable_women_max[k],
                                                               QALYvals_men_min = paramsdf$QALYvals_men_min[k],
                                                               QALYvals_men_max = paramsdf$QALYvals_men_max[k],
                                                               QALYvals_women_min = paramsdf$QALYvals_women_min[k],
                                                               QALYvals_women_max = paramsdf$QALYvals_women_max[k],
                                                               relsample_HIV = paramsdf$relsample_HIV[k],
                                                               relsample_PrEP = paramsdf$relsample_PrEP[k],
                                                               relsample_history = paramsdf$relsample_history[k]
                                                               ), use_draft = FALSE)
  orderly::orderly_commit(id1)
}









# Run model, full SSuN size
for (j in c(485694)){
id1 <- orderly::orderly_run("runmodel", parameters = list(npar = numpar,  peractefficacy = 0.6, YearlyDiagnoses = j, OverallDiagnoses = 485694), use_draft = FALSE)
orderly::orderly_commit(id1)
}

# Run model per 10000 initial cases
for (k in 1:1){
for (j in c(10000)){
  id1 <- orderly::orderly_run("runmodel", parameters = list(npar = numpar,
                                                            peractefficacy = paramsdf$peractefficacy[k],
                                                            alpha_msm_min = paramsdf$alpha_msm_min[k],
                                                            alpha_msm_max = paramsdf$alpha_msm_max[k],
                                                            alpha_msw_min = paramsdf$alpha_msw_min[k],
                                                            alpha_msw_max = paramsdf$alpha_msw_max[k],
                                                            alpha_women_min = paramsdf$alpha_women_min[k],
                                                            alpha_women_max = paramsdf$alpha_women_max[k],
                                                            ncasual_msm_min = paramsdf$ncasual_msm_min[k],
                                                            ncasual_msm_max = paramsdf$ncasual_msm_max[k],
                                                            ncasual_msw_min = paramsdf$ncasual_msw_min[k],
                                                            ncasual_msw_max = paramsdf$ncasual_msw_max[k],
                                                            ncasual_women_min = paramsdf$ncasual_women_min[k],
                                                            ncasual_women_max = paramsdf$ncasual_women_max[k],
                                                            nstable_msm_min = paramsdf$nstable_msm_min[k],
                                                            nstable_msm_max = paramsdf$nstable_msm_max[k],
                                                            nstable_msw_min = paramsdf$nstable_msw_min[k],
                                                            nstable_msw_max = paramsdf$nstable_msw_max[k],
                                                            nstable_women_min = paramsdf$nstable_women_min[k],
                                                            nstable_women_max = paramsdf$nstable_women_max[k],
                                                            QALYvals_men_min = paramsdf$QALYvals_men_min[k],
                                                            QALYvals_men_max = paramsdf$QALYvals_men_max[k],
                                                            QALYvals_women_min = paramsdf$QALYvals_women_min[k],
                                                            QALYvals_women_max = paramsdf$QALYvals_women_max[k],
                                                            relsample_HIV = paramsdf$relsample_HIV[k],
                                                            relsample_PrEP = paramsdf$relsample_PrEP[k],
                                                            relsample_history = paramsdf$relsample_history[k],
                                                            YearlyDiagnoses = j, OverallDiagnoses = 485694), use_draft = FALSE)
  orderly::orderly_commit(id1)
}
}


# run sensitivity scenarios
run_combination <- function(k) {

  id1 <- orderly::orderly_run("runmodel", parameters = list(npar = 1000,
                                                            peractefficacy = paramsdf$peractefficacy[k],
                                                            alpha_msm_min = paramsdf$alpha_msm_min[k],
                                                            alpha_msm_max = paramsdf$alpha_msm_max[k],
                                                            alpha_msw_min = paramsdf$alpha_msw_min[k],
                                                            alpha_msw_max = paramsdf$alpha_msw_max[k],
                                                            alpha_women_min = paramsdf$alpha_women_min[k],
                                                            alpha_women_max = paramsdf$alpha_women_max[k],
                                                            ncasual_msm_min = paramsdf$ncasual_msm_min[k],
                                                            ncasual_msm_max = paramsdf$ncasual_msm_max[k],
                                                            ncasual_msw_min = paramsdf$ncasual_msw_min[k],
                                                            ncasual_msw_max = paramsdf$ncasual_msw_max[k],
                                                            ncasual_women_min = paramsdf$ncasual_women_min[k],
                                                            ncasual_women_max = paramsdf$ncasual_women_max[k],
                                                            nstable_msm_min = paramsdf$nstable_msm_min[k],
                                                            nstable_msm_max = paramsdf$nstable_msm_max[k],
                                                            nstable_msw_min = paramsdf$nstable_msw_min[k],
                                                            nstable_msw_max = paramsdf$nstable_msw_max[k],
                                                            nstable_women_min = paramsdf$nstable_women_min[k],
                                                            nstable_women_max = paramsdf$nstable_women_max[k],
                                                            QALYvals_men_min = paramsdf$QALYvals_men_min[k],
                                                            QALYvals_men_max = paramsdf$QALYvals_men_max[k],
                                                            QALYvals_women_min = paramsdf$QALYvals_women_min[k],
                                                            QALYvals_women_max = paramsdf$QALYvals_women_max[k],
                                                            relsample_HIV = paramsdf$relsample_HIV[k],
                                                            relsample_PrEP = paramsdf$relsample_PrEP[k],
                                                            relsample_history = paramsdf$relsample_history[k],
                                                            YearlyDiagnoses =10000, OverallDiagnoses = 485694), use_draft = FALSE)
  orderly::orderly_commit(id1)
}


library(parallel)
cl <- makeCluster(detectCores() - 1)
clusterExport(cl, varlist = c("paramsdf"))
k_values = 2:17
parLapply(cl, k_values, run_combination)

#make figures from main run
id1 <- orderly::orderly_run("figuremaker",  parameters = list(npar = 1000, peractefficacy = 0.6), use_draft = FALSE)
orderly::orderly_commit(id1)


## make figures from sensitivity run
id1 <- orderly::orderly_run("figuremaker_sensitivity", parameters = list(npar = numpar), use_draft = FALSE)
orderly::orderly_commit(id1)

