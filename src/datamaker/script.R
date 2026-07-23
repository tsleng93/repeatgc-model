
if (!dir.exists("data")) dir.create("data")
if (!dir.exists("figs")) dir.create("figs")

### Aggregrated data from SSuN ###

# read in data from aggregation 3
df_agg3 <- read.csv(aggregation_3)
# read in data from aggregation 1
df_agg1 <- read.csv(aggregation_1)

# convert strings to data
imax = 40
df_agg3 <- stringstonumbers_df(df_agg3, 7 + 2*(0:2))
df_agg3 <- stringstonumbers_df(df_agg3, 55 + 2*(0:(imax-1)))
df_agg3 <- column_minandmax(df_agg3, 6 + 2*(1:21), "numeric")
df_agg3 <- column_minandmax(df_agg3, 56 + 2*(1:imax), "percent")


df_agg3 <- df_agg3 |> select(
  "Gender.Sexual.orientation.group","GC.History",
  "HIV.Status.PrEP.user","Race.group",
  "N","N.X.","N.W.","D",
  "Y.X.","Y.W.","F",
  "Nmin","Nmax",
  "N.X.min","N.X.max",
  "N.W.min","N.W.max",
  "Dmin","Dmax",
  "Y.X.min","Y.X.max",
  "Y.W.min","Y.W.max",
  "Fmin","Fmax")

# convert strings to data
imax = 44
df_agg1 <- stringstonumbers_df(df_agg1, 7 + 2*(0:2))
df_agg1 <- stringstonumbers_df(df_agg1, 57 + 2*(0:(imax-1)))
df_agg1 <- column_minandmax(df_agg1, 6 + 2*(1:21), "numeric")
df_agg1 <- column_minandmax(df_agg1, 56 + 2*(1:imax), "percent")

df_agg1 <- df_agg1 |> select(
  "Gender.Sexual.orientation.group","GC.History",
  "HIV.Status.PrEP.user","Race.group",
  "N","N.X.","N.W.","D",
  "Y.X.","Y.W.","F",
  "Nmin","Nmax",
  "N.X.min","N.X.max",
  "N.W.min","N.W.max",
  "Dmin","Dmax",
  "Y.X.min","Y.X.max",
  "Y.W.min","Y.W.max",
  "Fmin","Fmax")


# for MSW and women, use aggregation 1 for PrEP users and HIV+
df_agg3 <- df_agg3[df_agg3$Gender.Sexual.orientation.group == "MSM" | (df_agg3$HIV.Status.PrEP.user == "Negative non-PrEP user"), ]
df_agg1 <- df_agg1[df_agg1$Gender.Sexual.orientation.group != "MSM" & (df_agg1$HIV.Status.PrEP.user != "Negative non-PrEP user"), ]

df <- rbind(df_agg3, df_agg1)

df$GC.History[df$GC.History == "No"] = "no history"
df$GC.History[df$GC.History == "Yes"] = "with history"
df$GC.History[df$GC.History == "Yes"] = "with history"
df$HIV.Status.PrEP.user[df$HIV.Status.PrEP.user == "Negative non-PrEP user"] = "HIV- non-PrEP user"
df$HIV.Status.PrEP.user[df$HIV.Status.PrEP.user == "Negative PrEP user"] = "HIV- PrEP user"
df$HIV.Status.PrEP.user[df$HIV.Status.PrEP.user == "Positive"] = "PLHIV"

saveRDS(df, file = "data/SSuNdata.rds")

### Degree distributions from SSuN ###

#read in MSM deg dist
degdist_msm <- degdistmaker(degdist, sheetName = "MSM Num Male Parts")
saveRDS(degdist_msm, file = "data/degdist_msm.rds")

## read in MSW deg dist
degdist_msw <- degdistmaker(degdist, sheetName = "nonMSMmale num female parts")
saveRDS(degdist_msw, file = "data/degdist_msw.rds")

## read in Women deg dist
degdist_women <- degdistmaker(degdist, sheetName = "Women num male parts")
saveRDS(degdist_women, file = "data/degdist_women.rds")

p1 <- Fig_degdist(degdist_msm, degdist_msw, degdist_women, xmax = 20)

p1
ggsave("figs/degdists_plot.png", p1, width = 10, height = 5, units = "in", dpi = 1200)



### Proportion casual by partner number ###

options(mc.cores = 1)
rstan_options(auto_write = TRUE)
rstan_options(threads_per_chain = 1)

data_msm = data.frame(partners = c(1,2,3), M = c(21,91,145), N = c(66, 138, 219)) # from NEST
data_msw = data.frame(partners = c(1,2,3), M = c(23,68,81), N = c(72, 90, 102)) # from NATSAL
data_women = data.frame(partners = c(1,2,3), M = c(24,55,49), N = c(79, 66, 58)) # from NATSAL

## MSM
omega_msm <- stan_distributions(M = c(21,91,145), N = c(66, 138, 219), npar = npar, partnermax = max(degdist_msm$partnernum), orientation = "MSM", stanfile = stanfile, seed = 123)
## MSW
omega_msw <- stan_distributions(M = c(23,68,81), N = c(72, 90, 102), npar = npar, partnermax = max(degdist_msw$partnernum), orientation = "MSW", stanfile = stanfile, seed = 456)
## Women data
omega_women <- stan_distributions(M = c(24,55,49), N = c(79, 66, 58), npar = npar, partnermax = max(degdist_women$partnernum), orientation = "Women", stanfile = stanfile, seed = 789)



### Make trace plots ###

trace1_msm <- readRDS("figs/trace1_MSM.rds")
trace1_msw <- readRDS("figs/trace1_MSW.rds")
trace1_women <- readRDS("figs/trace1_Women.rds")

trace2_msm <- readRDS("figs/trace2_MSM.rds")
trace2_msw <- readRDS("figs/trace2_MSW.rds")
trace2_women <- readRDS("figs/trace2_Women.rds")

p <-  (trace1_msm + trace1_msw  + trace1_women) /
  (trace2_msm + trace2_msw + trace2_women) +
  plot_layout(guides = 'collect') &
  theme(legend.position = "bottom")

p
ggsave("figs/trace_plot.png", p, width = 12, height = 8, units = "in", dpi = 1200)


### Make posterior plots ###

overlay1_msm <- readRDS("figs/overlay1_MSM.rds")
overlay1_msw <- readRDS("figs/overlay1_MSW.rds")
overlay1_women <- readRDS("figs/overlay1_Women.rds")

overlay2_msm <- readRDS("figs/overlay2_MSM.rds")
overlay2_msw <- readRDS("figs/overlay2_MSW.rds")
overlay2_women <- readRDS("figs/overlay2_Women.rds")

p <-  (overlay1_msm + overlay1_msw  + overlay1_women) /
  (overlay2_msm + overlay2_msw + overlay2_women) +
  plot_layout(guides = 'collect') &
  theme(legend.position = "bottom")

p
ggsave("figs/overlay_plot.png", p, width = 12, height = 8, units = "in", dpi = 1200)


### Make omega plots ###

omegaplot <- Fig_omega(omega_msm, omega_msw, omega_women, data_msm, data_msw, data_women, 20)
ggsave("figs/omega_plot.png", omegaplot, width = 10, height = 5, units = "in", dpi = 1200)
