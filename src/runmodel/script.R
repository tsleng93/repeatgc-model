
### Read inputs ###
df_model <- readRDS("inputs/model_params.rds")

omega_msm <-  readRDS("inputs/omega_msm.rds")
omega_msw <-  readRDS("inputs/omega_msw.rds")
omega_women <-  readRDS("inputs/omega_women.rds")

if (omega_sens == "min"){
  
  omega_msm[, 3:511] = omega_msm[,3]
  omega_msw[, 3:103] = omega_msw[,3]
  omega_women[, 3:101] = omega_women[,3]
  
  #omega_msm <- omega_msm[rep(which.min(omega_msm[, 511]), nrow(omega_msm)), ]
  #omega_msw <- omega_msw[rep(which.min(omega_msw[, 103]), nrow(omega_msw)), ]
  #omega_women <- omega_women[rep(which.min(omega_women[, 101]), nrow(omega_women)), ]
} else if (omega_sens == "max"){
  omega_msm <- omega_msm[rep(which.max(omega_msm[, 511]), nrow(omega_msm)), ]
  omega_msw <- omega_msw[rep(which.max(omega_msw[, 103]), nrow(omega_msw)), ]
  omega_women <- omega_women[rep(which.max(omega_women[, 101]), nrow(omega_women)), ]
}




degdist_msm <-  readRDS("inputs/degdist_msm.rds")
degdist_msw <-  readRDS("inputs/degdist_msw.rds")
degdist_women <-  readRDS("inputs/degdist_women.rds")

sampled_degdist_msm <-  readRDS("inputs/sampled_degdist_msm.rds")
sampled_degdist_msw <-  readRDS("inputs/sampled_degdist_msw.rds")
sampled_degdist_women <-  readRDS("inputs/sampled_degdist_women.rds")



degdist_list_msm <- split(sampled_degdist_msm, 
                          sampled_degdist_msm$paramnum)

degdist_list_msw <- split(sampled_degdist_msw, 
                          sampled_degdist_msw$paramnum)

degdist_list_women <- split(sampled_degdist_women, 
                          sampled_degdist_women$paramnum)

if (degdist_sens != "NULL"){
  auc_results <- sampled_degdist_msm %>%
    group_by(paramnum) %>%
    arrange(numpartners, .by_group = TRUE) %>%
    mutate(
      cumulative = cumsum(nohistorynonuser_prob)
    ) %>%
    summarise(
      auc = sum(
        (head(cumulative, -1) + tail(cumulative, -1)) / 2 *
          diff(numpartners)
      ),
      .groups = "drop"
    )
  
  # Identify extreme simulations
  lowest_auc_msm <- auc_results %>%
    slice_min(auc, n = 1)
  
  highest_auc_msm <- auc_results %>%
    slice_max(auc, n = 1)
  
  auc_results <- sampled_degdist_msw %>%
    group_by(paramnum) %>%
    arrange(numpartners, .by_group = TRUE) %>%
    mutate(
      cumulative = cumsum(nohistorynonuser_prob)
    ) %>%
    summarise(
      auc = sum(
        (head(cumulative, -1) + tail(cumulative, -1)) / 2 *
          diff(numpartners)
      ),
      .groups = "drop"
    )
  
  # Identify extreme simulations
  lowest_auc_msw <- auc_results %>%
    slice_min(auc, n = 1)
  
  highest_auc_msw <- auc_results %>%
    slice_max(auc, n = 1)
  
  auc_results <- sampled_degdist_women %>%
    group_by(paramnum) %>%
    arrange(numpartners, .by_group = TRUE) %>%
    mutate(
      cumulative = cumsum(nohistorynonuser_prob)
    ) %>%
    summarise(
      auc = sum(
        (head(cumulative, -1) + tail(cumulative, -1)) / 2 *
          diff(numpartners)
      ),
      .groups = "drop"
    )
  
  # Identify extreme simulations
  lowest_auc_women <- auc_results %>%
    slice_min(auc, n = 1)
  
  highest_auc_women <- auc_results %>%
    slice_max(auc, n = 1)
  
  
  
}

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


if (YearlyDiagnoses == OverallDiagnoses) {
  
  df_model$N_scale <- 1
  
} else {
  
  N_totals <- df_model %>%
    group_by(paramnum) %>%
    summarise(total_N = sum(N), .groups = "drop")
  
  df_model <- df_model %>%
    left_join(N_totals, by = "paramnum") %>%
    mutate(N_scale = YearlyDiagnoses / total_N) %>%
    select(-total_N)
  
}


### Initialise output vectors ###

n <- nrow(df_model)

withoutvaccine <- numeric(n)
withvaccine <- numeric(n)

Overallwithoutvaccine <- numeric(n)
Overallwithvaccine <- numeric(n)

Firstinfectionswithoutvaccine <- numeric(n)
Firstinfectionswithvaccine <- numeric(n)

Lifeyearswithoutvaccine <- numeric(n)
Lifeyearswithvaccine <- numeric(n)

prev <- numeric(n)
LL <- numeric(n)

N0model <- numeric(n)
N1model <- numeric(n)

Fmodel <- numeric(n)
Fmodel_vaccine <- numeric(n)

QALYlosswithoutvaccine <- numeric(n)
QALYlosswithvaccine <- numeric(n)

probdiagstable <- rep(1, n)
probdiagcasual <- rep(1, n)



sex_orientation = df_model$sex_orientation
paramnum = df_model$paramnum
degdistmarker = df_model$degdistmarker
alpha = df_model$alpha
N1 = df_model$N1
F = df_model$F
sexactscasual = df_model$sexactscasual
sexactsstable = df_model$sexactsstable
probdiagstable = df_model$probdiagstable
probdiagcasual = df_model$probdiagcasual
dur = df_model$dur
N = df_model$N
QALYs = df_model$QALYs
N_scale = df_model$N_scale

for (i in 1:nrow(df_model)){
  
  if (sex_orientation[i] == "MSM"){
    
    if (degdist_sens == "NULL"){
      
      degdist <- degdist_list_msm[[as.character(paramnum[i])]]
      #degdist <- sampled_degdist_msm |> filter(paramnum == paramnum[i])
    } else if (degdist_sens == "min"){
      degdist <- degdist_list_msm[[as.character(highest_auc_msm$paramnum)]]
      
      #degdist <- sampled_degdist_msm |> filter(paramnum == highest_auc_msm$paramnum)
    } else if (degdist_sens == "max"){
      degdist <- degdist_list_msm[[as.character(lowest_auc_msm$paramnum)]]
      #degdist <- sampled_degdist_msm |> filter(paramnum == lowest_auc_msm$paramnum)
      
    }
    degdist <- degdist[, c(2, degdistmarker[i] + 7)]
    #degdist <- degdist_msm[,  c(1, degdistmarker[i])]
    omegamat <- omega_msm[ , seq_len(ncol(omega_msm)-1)]
  } else if (sex_orientation[i] == "non-MSM men"){
    if (degdist_sens == "NULL"){
      degdist <- degdist_list_msw[[as.character(paramnum[i])]]
      
      #degdist <- sampled_degdist_msw |> filter(paramnum == paramnum[i])
    } else if (degdist_sens == "min"){
      degdist <- degdist_list_msw[[as.character(highest_auc_msw$paramnum)]]
      
      
      #degdist <- sampled_degdist_msw |> filter(paramnum == highest_auc_msw$paramnum)
    } else if (degdist_sens == "max"){
      
      degdist <- degdist_list_msw[[as.character(lowest_auc_msw$paramnum)]]
      
      #degdist <- sampled_degdist_msw |> filter(paramnum == lowest_auc_msw$paramnum)
    }
    degdist <- degdist[, c(2, degdistmarker[i] + 7)]
    #degdist <- degdist_msw[, c(1, degdistmarker[i])]
    omegamat <- omega_msw[, seq_len(ncol(omega_msw)-1)]
  } else{
    if (degdist_sens == "NULL"){
      #degdist <- sampled_degdist_women |> filter(paramnum == paramnum[i])
      degdist <- degdist_list_women[[as.character(paramnum[i])]]
      
    } else if (degdist_sens == "min"){
      degdist <- degdist_list_women[[as.character(highest_auc_women$paramnum)]]
      
      #degdist <- sampled_degdist_women |> filter(paramnum == highest_auc_women$paramnum)
    } else if (degdist_sens == "max"){
      degdist <- degdist_list_women[[as.character(lowest_auc_women$paramnum)]]
      
      
      #degdist <- sampled_degdist_women |> filter(paramnum == lowest_auc_women$paramnum)
    }
    degdist <- degdist[, c(2, degdistmarker[i] + 7)]
    #degdist <- degdist_women[,  c(1, degdistmarker[i])]
    omegamat <- omega_women[, seq_len(ncol(omega_women)-1)]
  }
  
  omegavals <- as.numeric(omegamat[paramnum[i], ])
  
  
  # find prevalence to satisfy FOI
  temp = FindPrev_fun(alpha[i], N1[i], F[i]/1000,  
                     degdist[, 1], degdist[,2], omegavals, 
                     sexactscasual = sexactscasual[i], 
                     sexactsstable = sexactsstable[i], 
                     probdiagstable = probdiagstable[i], 
                     probdiagcasual = probdiagcasual[i], 
                     dur = dur[i], N = N[i])
  
  prev[i] = temp[1]
  LL[i] = temp[2]

  
  # run model with and without vaccine
  tempmodeloutput_withoutvaccine = model(prev[i], alpha[i], 
                                         degdist[, 1], degdist[, 2], omegavals,
                                         sexactscasual = sexactscasual[i],
                                         sexactsstable = sexactsstable[i],
                                         probdiagstable = probdiagstable[i],
                                         probdiagcasual = probdiagcasual[i],
                                         dur = dur[i])
  
  tempmodeloutput_withvaccine = model(prev[i], (1-peractefficacy)*alpha[i],
                                      degdist[, 1], degdist[, 2],  omegavals,
                                      sexactscasual = sexactscasual[i],
                                      sexactsstable = sexactsstable[i],
                                      probdiagstable = probdiagstable[i],
                                      probdiagcasual = probdiagcasual[i],
                                      dur = dur[i])
  
  

  ## if YearlyDiagnoses == OverallDiagnoses, stick with current numbers (different for each parameter set)
  ## if unequal, rescale so results per yearly diagnoses (same for all parameter sets)
  # if (YearlyDiagnoses == OverallDiagnoses){
  #   N_scale = 1
  # } else{
  #   N_scale = YearlyDiagnoses/sum(N[paramnum == paramnum[i]])
  # }
  # 
  # N_scale[i] = N_scale
  
  #N_scale = N_scale[i]
  
  # number of repeat-GC diagnoses per year
  withoutvaccine[i] = N_scale[i]*N[i]*tempmodeloutput_withoutvaccine[4]
  withvaccine[i] = N_scale[i]*N[i]*tempmodeloutput_withvaccine[4]
  
  
  if (OverallDiagnoses == YearlyDiagnoses){
    # number of repeat-GC diagnoses over follow-up period
    Overallwithoutvaccine[i] = dur[i]*withoutvaccine[i]
    Overallwithvaccine[i] =   dur[i]*withvaccine[i]
  } else{
    # number of repeat-GC diagnoses over time horizon for health impacts for QALYs
    Overallwithoutvaccine[i] = Years_QALY*withoutvaccine[i]
    Overallwithvaccine[i] =   Years_QALY*withvaccine[i]
  }

  
  # number of individuals with at least one repeat-GC diagnosis
  Firstinfectionswithoutvaccine[i] = N_scale[i]*N[i]*tempmodeloutput_withoutvaccine[1]
  Firstinfectionswithvaccine[i] = N_scale[i]*N[i]*tempmodeloutput_withvaccine[1]
  
  # number of lifeyears until first repeat-GC diagnoses or end of follow-up period
  Lifeyearswithoutvaccine[i] = N_scale[i]*N[i]*tempmodeloutput_withoutvaccine[2]
  Lifeyearswithvaccine[i] = N_scale[i]*N[i]*tempmodeloutput_withvaccine[2]
  
  N0model[i] = N_scale[i]*N[i]*(1 - tempmodeloutput_withoutvaccine[1])
  N1model[i] = N_scale[i]*N[i]*tempmodeloutput_withoutvaccine[1]
  
  Fmodel[i]  = tempmodeloutput_withoutvaccine[1]/tempmodeloutput_withoutvaccine[2]
  Fmodel_vaccine[i]  = tempmodeloutput_withvaccine[1]/tempmodeloutput_withvaccine[2]
  
  
  QALYlosswithoutvaccine[i] = N_scale[i]*Years_QALY*N[i]*QALYs[i]*tempmodeloutput_withoutvaccine[5]
  QALYlosswithvaccine[i] = N_scale[i]*Years_QALY*N[i]*QALYs[i]*tempmodeloutput_withvaccine[5]
}


### Add output vectors back to df_model ###

df_model$withoutvaccine <- withoutvaccine
df_model$withvaccine <- withvaccine

df_model$Overallwithoutvaccine <- Overallwithoutvaccine
df_model$Overallwithvaccine <- Overallwithvaccine

df_model$Firstinfectionswithoutvaccine <- Firstinfectionswithoutvaccine
df_model$Firstinfectionswithvaccine <- Firstinfectionswithvaccine

df_model$Lifeyearswithoutvaccine <- Lifeyearswithoutvaccine
df_model$Lifeyearswithvaccine <- Lifeyearswithvaccine

df_model$prev <- prev
df_model$LL <- LL

df_model$N0model <- N0model
df_model$N1model <- N1model

df_model$Fmodel <- Fmodel
df_model$Fmodel_vaccine <- Fmodel_vaccine

df_model$QALYlosswithoutvaccine <- QALYlosswithoutvaccine
df_model$QALYlosswithvaccine <- QALYlosswithvaccine

#df_model$probdiagstable <- probdiagstable
#df_model$probdiagcasual <- probdiagcasual

#df_model$N_scale <- N_scale

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


df_D <- model_data_comparison(df_data, df_model, "D", "Dmin", "Dmax", modeloutputs = "Overallwithoutvaccine", labels = "Model")


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
saveRDS(df_D, "data/df_D.rds")
saveRDS(df_N, "data/df_N.rds")