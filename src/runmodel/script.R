
### Read inputs ###
df_model <- readRDS("inputs/model_params.rds")

omega_msm <-  readRDS("inputs/omega_msm.rds")
omega_msw <-  readRDS("inputs/omega_msw.rds")
omega_women <-  readRDS("inputs/omega_women.rds")

degdist_msm <-  readRDS("inputs/degdist_msm.rds")
degdist_msw <-  readRDS("inputs/degdist_msw.rds")
degdist_women <-  readRDS("inputs/degdist_women.rds")


# initialise model outputs into df_model

# number of repeat-GC diagnoses per year
df_model$withoutvaccine = 0
df_model$withvaccine = 0
# number of repeat-GC diagnoses over follow-up period
df_model$Overallwithoutvaccine = 0
df_model$Overallwithvaccine = 0
# with 1 or more repeat-GC diagnoses over follow-up period
df_model$Firstinfectionswithoutvaccine = 0
df_model$Firstinfectionswithvaccine = 0
# Life years observed until first infection or end of follow-up
df_model$Lifeyearswithoutvaccine = 0
df_model$Lifeyearswithvaccine = 0

# model prevalence to match FOI
df_model$prev = 0
# Log Likelihood of model for subgroup
df_model$LL = 0

# #  with 0 and 1 or more repeat-GC over follow-up period, without vaccine
df_model$N0model = 0
df_model$N1model = 0

## FOI from model 
df_model$Fmodel = 0
df_model$Fmodel_withvaccine = 0

##QALY loss from model over Years_QALY years
df_model$QALYlosswithoutvaccine = 0
df_model$QALYlosswithvaccine = 0

# set probability of diagnosis after infection (set at 1)
df_model$probdiagstable = 1
df_model$probdiagcasual = 1

# scaling factor for N so results are per e.g. 10,000 initial cases
df_model$N_scale = 0

# set time horizon for health impacts for QALYs
Years_QALY = 5


for (i in 1:nrow(df_model)){
  
  if (df_model$sex_orientation[i] == "MSM"){
    degdist <- degdist_msm[,  c(1, df_model$degdistmarker[i])]
    omegamat <- omega_msm[ , seq_len(ncol(omega_msm)-1)]
  } else if (df_model$sex_orientation[i] == "non-MSM men"){
    degdist <- degdist_msw[, c(1, df_model$degdistmarker[i])]
    omegamat <- omega_msw[, seq_len(ncol(omega_msw)-1)]
  } else{
    degdist <- degdist_women[,  c(1, df_model$degdistmarker[i])]
    omegamat <- omega_women[, seq_len(ncol(omega_women)-1)]
  }
  
  omegavals <- as.numeric(omegamat[df_model$paramnum[i], ])
  
  
  # find prevalence to satisfy FOI
  temp = FindPrev_fun(df_model$alpha[i], df_model$N1[i], df_model$F[i]/1000,  
                     degdist[, 1], degdist[,2], omegavals, 
                     sexactscasual = df_model$sexactscasual[i], 
                     sexactsstable = df_model$sexactsstable[i], 
                     probdiagstable = df_model$probdiagstable[i], 
                     probdiagcasual = df_model$probdiagcasual[i], 
                     dur = df_model$dur[i], N = df_model$N[i])
  
  df_model$prev[i] = temp[1]
  df_model$LL[i] = temp[2]

  
  # run model with and without vaccine
  tempmodeloutput_withoutvaccine = model(df_model$prev[i], df_model$alpha[i], 
                                         degdist[, 1], degdist[, 2], omegavals,
                                         sexactscasual = df_model$sexactscasual[i],
                                         sexactsstable = df_model$sexactsstable[i],
                                         probdiagstable = df_model$probdiagstable[i],
                                         probdiagcasual = df_model$probdiagcasual[i],
                                         dur = df_model$dur[i])
  
  tempmodeloutput_withvaccine = model(df_model$prev[i], (1-peractefficacy)*df_model$alpha[i],
                                      degdist[, 1], degdist[, 2],  omegavals,
                                      sexactscasual = df_model$sexactscasual[i],
                                      sexactsstable = df_model$sexactsstable[i],
                                      probdiagstable = df_model$probdiagstable[i],
                                      probdiagcasual = df_model$probdiagcasual[i],
                                      dur = df_model$dur[i])
  
  

  ## if YearlyDiagnoses == OverallDiagnoses, stick with current numbers (different for each parameter set)
  ## if unequal, rescale so results per yearly diagnoses (same for all parameter sets)
  if (YearlyDiagnoses == OverallDiagnoses){
    N_scale = 1
  } else{
    N_scale = YearlyDiagnoses/sum(df_model$N[df_model$paramnum == df_model$paramnum[i]])
  }
  
  df_model$N_scale[i] = N_scale
  
  
  # number of repeat-GC diagnoses per year
  df_model$withoutvaccine[i] = N_scale*df_model$N[i]*tempmodeloutput_withoutvaccine[4]
  df_model$withvaccine[i] = N_scale*df_model$N[i]*tempmodeloutput_withvaccine[4]
  
  
  if (OverallDiagnoses == YearlyDiagnoses){
    # number of repeat-GC diagnoses over follow-up period
    df_model$Overallwithoutvaccine[i] = df_model$dur[i]*df_model$withoutvaccine[i]
    df_model$Overallwithvaccine[i] =   df_model$dur[i]*df_model$withvaccine[i]
  } else{
    # number of repeat-GC diagnoses over time horizon for health impacts for QALYs
    df_model$Overallwithoutvaccine[i] = Years_QALY*df_model$withoutvaccine[i]
    df_model$Overallwithvaccine[i] =   Years_QALY*df_model$withvaccine[i]
  }

  
  # number of individuals with at least one repeat-GC diagnosis
  df_model$Firstinfectionswithoutvaccine[i] = N_scale*df_model$N[i]*tempmodeloutput_withoutvaccine[1]
  df_model$Firstinfectionswithvaccine[i] = N_scale*df_model$N[i]*tempmodeloutput_withvaccine[1]
  
  # number of lifeyears until first repeat-GC diagnoses or end of follow-up period
  df_model$Lifeyearswithoutvaccine[i] = N_scale*df_model$N[i]*tempmodeloutput_withoutvaccine[2]
  df_model$Lifeyearswithvaccine[i] = N_scale*df_model$N[i]*tempmodeloutput_withvaccine[2]
  
  df_model$N0model[i] = N_scale*df_model$N[i]*(1 - tempmodeloutput_withoutvaccine[1])
  df_model$N1model[i] = N_scale*df_model$N[i]*tempmodeloutput_withoutvaccine[1]
  
  df_model$Fmodel[i]  = tempmodeloutput_withoutvaccine[1]/tempmodeloutput_withoutvaccine[2]
  df_model$Fmodel_vaccine[i]  = tempmodeloutput_withvaccine[1]/tempmodeloutput_withvaccine[2]
  
  
  df_model$QALYlosswithoutvaccine[i] = N_scale*Years_QALY*df_model$N[i]*df_model$QALYs[i]*tempmodeloutput_withoutvaccine[5]
  df_model$QALYlosswithvaccine[i] = N_scale*Years_QALY*df_model$N[i]*df_model$QALYs[i]*tempmodeloutput_withvaccine[5]
}

df_model$GroupEfficacy = 1 - df_model$withvaccine/df_model$withoutvaccine


palette1 = c("lightgrey", "#fbb4ae", "#FFFF8F", "#b3cde3", "#fed9a6", "#decbe4", "#ccebc5", "#e5d8bd", "white")
palette2 = c("black", "#e41a1c", "#FDDA0D", "#377eb8","#ff7f00", "#984ea3", "#4daf4a", "#a65628","darkgrey")


## Obtain results for different strategies (combining groups under different eligibility criteria)

df_strategy_msm  = strategy_df(df_model, "MSM", palette1, palette2)
df_strategy_msw  = strategy_df(df_model, "non-MSM men", palette1, palette2)
df_strategy_women  = strategy_df(df_model, "Women", palette1, palette2)

df_strategy_msm$sexorientation = "MSM"
df_strategy_msw$sexorientation = "MSW"
df_strategy_women$sexorientation = "Women"

df_strategy = rbind(df_strategy_women, df_strategy_msw, df_strategy_msm)

## Read in data from datamaker for comparisons

df_data <- readRDS("inputs/SSuNdata.rds")
df_data$Lifeyears = df_data$Y.W. + df_data$Y.X.
df_data$Lifeyearsmin = df_data$Y.W.min + df_data$Y.X.min
df_data$Lifeyearsmax = df_data$Y.W.max + df_data$Y.X.max



df_FOI <- model_data_comparison(df_data, df_model, "F", "Fmin", "Fmax",   modeloutputs = c("F", "Fmodel", "Fmodel_vaccine"),
                            labels = c(
                              "Sampled target",
                              "Model (unvaccinated)",
                              "Model (vaccinated)"
                            ))
df_FOI$F[df_FOI$type == "Model (unvaccinated)"] = 1000*df_FOI$F[df_FOI$type == "Model (unvaccinated)"]
df_FOI$Fmin[df_FOI$type == "Model (unvaccinated)"] = 1000*df_FOI$Fmin[df_FOI$type == "Model (unvaccinated)"]
df_FOI$Fmax[df_FOI$type == "Model (unvaccinated)"] = 1000*df_FOI$Fmax[df_FOI$type == "Model (unvaccinated)"]

df_FOI$F[df_FOI$type == "Model (vaccinated)"] = 1000*df_FOI$F[df_FOI$type == "Model (vaccinated)"]
df_FOI$Fmin[df_FOI$type == "Model (vaccinated)"] = 1000*df_FOI$Fmin[df_FOI$type == "Model (vaccinated)"]
df_FOI$Fmax[df_FOI$type == "Model (vaccinated)"] = 1000*df_FOI$Fmax[df_FOI$type == "Model (vaccinated)"]



df_NX <- model_data_comparison(df_data, df_model, "N.X.", "N.X.min", "N.X.max", modeloutputs = c("N1", "Firstinfectionswithoutvaccine"),
                             labels = c("Sampled target", "Model"))



df_data$N = df_data$N.X. + df_data$N.W.
df_data$Nmin = df_data$N.X.min + df_data$N.W.min
df_data$Nmax = df_data$N.X.max + df_data$N.W.max

df_N <- model_data_comparison(df_data, df_model, "N", "Nmin", "Nmax",  modeloutputs = c("N", "N"), labels = c("Sampled target", "Model"))
df_Lifeyears <- model_data_comparison(df_data, df_model, "Lifeyears", "Lifeyearsmin", "Lifeyearsmax", modeloutputs = "Lifeyearswithoutvaccine", labels = "Model")

## save outputs

if (!dir.exists("data")) dir.create("data")
saveRDS(df_model, "data/model_paramsandoutputs.rds")
saveRDS(df_strategy, "data/strategies.rds")
saveRDS(df_FOI, "data/df_FOI.rds")
saveRDS(df_NX, "data/df_NX.rds")
saveRDS(df_Lifeyears, "data/df_Lifeyears.rds")
saveRDS(df_N, "data/df_N.rds")
