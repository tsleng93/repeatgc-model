
### Sample from SSuN aggregate ranges ###

df <- readRDS("inputs/SSuNdata.rds") # read in data from datamaker

set.seed(1234)

Nvals =  sample_values_df_normal(n_times = npar, min_values = df$Nmin , max_values = df$Nmax , mean_values= df$N)
Nvals[Nvals < 0] = 0
quantile(rowSums(Nvals), c(0.025, 0.5, 0.975))
Nvals = makelong(Nvals, df, npar)
Nvals$value[Nvals$GC.History == "with history"] = relsample_history*Nvals$value[Nvals$GC.History == "with history"]
Nvals$value[Nvals$HIV.Status.PrEP.user == "PLHIV"] = relsample_HIV*Nvals$value[Nvals$HIV.Status.PrEP.user == "PLHIV"]
Nvals$value[Nvals$HIV.Status.PrEP.user == "HIV- PrEP user"] = relsample_PrEP*Nvals$value[Nvals$HIV.Status.PrEP.user == "HIV- PrEP user"]

Fvals = sample_values_df_lognormal(n_times = npar, min_values = df$Fmin, max_values = df$Fmax, mean_values = df$F)
Fvals = makelong(Fvals, df, npar)

## observation period - assume this is fixed relative to N (which will mean we approximately match uncertainty in life years observed)
dur  = (df$Y.W.)/(df$N.W.)
dur = rep(dur, npar)

NXvals = Nvals
NXvals$value = 0.95*Nvals$value*(1 - exp(-dur*Fvals$value/1000))

D_rel = df$D/(df$N.X.) # relative diagnoses to number of infections
D_rel  = rep(D_rel, npar)
Dvals = D_rel*NXvals$value


### Sample other parameters based on ranges from literature ###

sexactscasual_msm <- sample(1:7, npar, replace = TRUE)
sexactsstable_msm <- sample(12:36, npar, replace = TRUE)

sexactscasual_msw <- sample(1:7, npar, replace = TRUE)
sexactsstable_msw <- sample(12:36, npar, replace = TRUE)

sexactscasual_women <- sample(1:7, npar, replace = TRUE)
sexactsstable_women <- sample(7:36, npar, replace = TRUE)  

alpha_msm <- runif(n = npar, min = 0.2, max = 0.55)
alpha_msw <- runif(n = npar, min = 0.2, max = 0.5)
alpha_women <- runif(n = npar, min = 0.5, max = 0.7)

QALYvals_men = sample_values_df_lognormal(n_times = npar, min_values = 0.19, max_values = 5.48, mean_values = 1.43)
QALYvals_women = sample_values_df_lognormal(n_times = npar, min_values = 3.26, max_values = 39.14, mean_values = 14.22)



### for sensitivity scenarios where these are fixed if min == max
if (ncasual_msm_min == ncasual_msm_max){
  sexactscasual_msm[] = ncasual_msm_min
}
if (nstable_msm_min == nstable_msm_max){
  sexactsstable_msm[] = nstable_msm_min
}
if (ncasual_msw_min == ncasual_msw_max){
  sexactscasual_msw[] = ncasual_msw_min
}
if (nstable_msw_min == nstable_msw_max){
  sexactsstable_msw[] = nstable_msw_min
}
if (ncasual_women_min == ncasual_women_max){
  sexactscasual_women[] = ncasual_women_min
}
if (nstable_women_min == nstable_women_max){
  sexactsstable_women[] = nstable_women_min
}

if (alpha_msm_min == alpha_msm_max){
  alpha_msm[] = alpha_msm_min
}
if (alpha_msw_min == alpha_msw_max){
  alpha_msw[] = alpha_msw_min
}
if (alpha_women_min == alpha_women_max){
  alpha_women[] = alpha_women_min
}

if (QALYvals_men_min == QALYvals_men_max){
  QALYvals_men[,1] = QALYvals_men_min
}
if (QALYvals_women_min == QALYvals_women_max){
  QALYvals_women[,1] = QALYvals_women_min
}


alpha_msm <- rep(alpha_msm, each = sum(df$Gender.Sexual.orientation.group == "MSM"))
alpha_msw <- rep(alpha_msw, each = sum(df$Gender.Sexual.orientation.group == "non-MSM men"))
alpha_women <- rep(alpha_women, each = sum(df$Gender.Sexual.orientation.group == "Women"))

alpha <- numeric(length = npar*48)
alpha[df$Gender.Sexual.orientation.group == "MSM"] = alpha_msm
alpha[df$Gender.Sexual.orientation.group == "non-MSM men"] = alpha_msw
alpha[df$Gender.Sexual.orientation.group == "Women"] = alpha_women


sexactsstable_msm <- rep(sexactsstable_msm, each = sum(df$Gender.Sexual.orientation.group == "MSM"))
sexactsstable_msw <- rep(sexactsstable_msw, each = sum(df$Gender.Sexual.orientation.group == "non-MSM men"))
sexactsstable_women <- rep(sexactsstable_women, each = sum(df$Gender.Sexual.orientation.group == "Women"))

sexactsstable <- numeric(length = npar*48)
sexactsstable[df$Gender.Sexual.orientation.group == "MSM"] = sexactsstable_msm
sexactsstable[df$Gender.Sexual.orientation.group == "non-MSM men"] = sexactsstable_msw
sexactsstable[df$Gender.Sexual.orientation.group == "Women"] = sexactsstable_women

sexactscasual_msm <- rep(sexactscasual_msm, each = sum(df$Gender.Sexual.orientation.group == "MSM"))
sexactscasual_msw <- rep(sexactscasual_msw, each = sum(df$Gender.Sexual.orientation.group == "non-MSM men"))
sexactscasual_women <- rep(sexactscasual_women, each = sum(df$Gender.Sexual.orientation.group == "Women"))

sexactscasual <- numeric(length = npar*48)
sexactscasual[df$Gender.Sexual.orientation.group == "MSM"] = sexactscasual_msm
sexactscasual[df$Gender.Sexual.orientation.group == "non-MSM men"] = sexactscasual_msw
sexactscasual[df$Gender.Sexual.orientation.group == "Women"] = sexactscasual_women

QALYs_msm <- rep(QALYvals_men[,1], each = sum(df$Gender.Sexual.orientation.group == "MSM"))
QALYs_msw <- rep(QALYvals_men[,1], each = sum(df$Gender.Sexual.orientation.group == "non-MSM men"))
QALYs_women <- rep(QALYvals_women[,1], each = sum(df$Gender.Sexual.orientation.group == "Women"))

QALYs <- numeric(length = npar*48)
QALYs[df$Gender.Sexual.orientation.group == "MSM"] = QALYs_msm
QALYs[df$Gender.Sexual.orientation.group == "non-MSM men"] = QALYs_msw
QALYs[df$Gender.Sexual.orientation.group == "Women"] = QALYs_women

QALYs <- QALYs/1000 # because estimate from literature is per 1000 incident cases

#input for run_model
df_model = data.frame(paramnum = Fvals$paramnum, 
                      sex_orientation = Fvals$Gender.Sexual.orientation.group,
                      gc_history = Fvals$GC.History,
                      HIV_prepuse = Fvals$HIV.Status.PrEP.user,
                      ethnicity = Fvals$Race.group,
                      F = Fvals$value,
                      N = Nvals$value,
                      dur = dur,
                      D = Dvals,
                      sexactscasual = sexactscasual,
                      sexactsstable = sexactsstable,
                      alpha = alpha,
                      N1 = NXvals$value,
                      QALYs = QALYs)



# add in marker for which degree distribution
df_model$degdistmarker = 0
df_model$degdistmarker[df_model$gc_history == "no history" & df_model$HIV_prepuse == "HIV- non-PrEP user"] = 2
df_model$degdistmarker[df_model$gc_history == "no history" & df_model$HIV_prepuse == "HIV- PrEP user"] = 3
df_model$degdistmarker[df_model$gc_history == "no history" & df_model$HIV_prepuse == "PLHIV"] = 4
df_model$degdistmarker[df_model$gc_history == "with history" & df_model$HIV_prepuse == "HIV- non-PrEP user"] = 5
df_model$degdistmarker[df_model$gc_history == "with history" & df_model$HIV_prepuse == "HIV- PrEP user"] = 6
df_model$degdistmarker[df_model$gc_history == "with history" & df_model$HIV_prepuse == "PLHIV"] = 7


if (!dir.exists("data")) dir.create("data")

saveRDS(df_model, file = "data/model_params.rds")

