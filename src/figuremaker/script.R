
if (!dir.exists("figs")) dir.create("figs")


palette_model <- c( "#7fc97f", "black")
palette_full <- palette_model
palette2 <- c("black", "#e41a1c", "#FDDA0D", "#377eb8","#ff7f00", "#984ea3", "#4daf4a", "#a65628","darkgrey")


### read in files
df_strategy_orig <- readRDS("inputs/strategies.rds")

df_strategy <- df_strategy_orig

df_strategy$numberneededtotreat = 1/df_strategy$caseavertedperdose
df_strategy$numberneededtotreat[df_strategy$numberneededtotreat < 0] = -1000

df_strategy$numberneededtotreat_QALY = df_strategy$vaccinatednum/df_strategy$QALYsgained
df_strategy$numberneededtotreat_QALY[df_strategy$numberneededtotreat_QALY < 0] = -10000
df_strategy$numberneededtotreat_QALY[is.na(df_strategy$numberneededtotreat_QALY)] = -10000

#change names
df_strategy$strategy[df_strategy$strategy == "1. No vaccination"] = "No vaccination"
df_strategy$strategy[df_strategy$strategy == "2. by HIV status"] = "by HIV status"
df_strategy$strategy[df_strategy$strategy == "3. by PrEP use"] = "by PrEP use"
df_strategy$strategy[df_strategy$strategy == "4. by history"] = "by history"
df_strategy$strategy[df_strategy$strategy == "5. by HIV status + PrEP use"] = "by HIV status + PrEP use"
df_strategy$strategy[df_strategy$strategy == "6. by history + HIV status"] = "by history + HIV status"
df_strategy$strategy[df_strategy$strategy == "7. by history + PrEP use"] = "by history + PrEP use"
df_strategy$strategy[df_strategy$strategy == "8. by history + HIV status + PrEP use"] = "by history + HIV status + PrEP use"
df_strategy$strategy[df_strategy$strategy == "9. to all"] = "to all"

df_strategy$stratval[df_strategy$strategy == "No vaccination"] = 1
df_strategy$stratval[df_strategy$strategy == "by HIV status"] = 2
df_strategy$stratval[df_strategy$strategy == "by PrEP use"] = 3
df_strategy$stratval[df_strategy$strategy == "by history"] = 4
df_strategy$stratval[df_strategy$strategy == "by HIV status + PrEP use"] = 5
df_strategy$stratval[df_strategy$strategy == "by history + HIV status"] = 6
df_strategy$stratval[df_strategy$strategy == "by history + PrEP use"] = 7
df_strategy$stratval[df_strategy$strategy == "by history + HIV status + PrEP use"] = 8
df_strategy$stratval[df_strategy$strategy == "to all"] = 9

df_strategy$strategy <- factor(df_strategy$strategy, levels = unique(df_strategy$strategy[order(df_strategy$stratval)]))

### Numerical outputs stated in manuscript

quantile(df_strategy$vaccinatednum[df_strategy$sexorientation == "MSM" & df_strategy$strategy == "to all"], c(0.5, 0.025, 0.975))
quantile(df_strategy$vaccinatednum[df_strategy$sexorientation == "MSW" & df_strategy$strategy == "to all"], c(0.5, 0.025, 0.975))
quantile(df_strategy$vaccinatednum[df_strategy$sexorientation == "Women" & df_strategy$strategy == "to all"], c(0.5, 0.025, 0.975))

quantile(df_strategy$vaccinated[df_strategy$sexorientation == "MSM" & df_strategy$strategy == "by history"], c(0.5, 0.025, 0.975))
quantile(df_strategy$vaccinated[df_strategy$sexorientation == "MSW" & df_strategy$strategy == "by history"], c(0.5, 0.025, 0.975))
quantile(df_strategy$vaccinated[df_strategy$sexorientation == "Women" & df_strategy$strategy == "by history"], c(0.5, 0.025, 0.975))

quantile(df_strategy$vaccinated[df_strategy$sexorientation == "MSM" & df_strategy$strategy == "by HIV status + PrEP use"], c(0.5, 0.025, 0.975))
quantile(df_strategy$vaccinated[df_strategy$sexorientation == "MSW" & df_strategy$strategy == "by HIV status + PrEP use"], c(0.5, 0.025, 0.975))
quantile(df_strategy$vaccinated[df_strategy$sexorientation == "Women" & df_strategy$strategy == "by HIV status + PrEP use"], c(0.5, 0.025, 0.975))

quantile(df_strategy$vaccinated[df_strategy$sexorientation == "MSM" & df_strategy$strategy == "by history + HIV status + PrEP use"], c(0.5, 0.025, 0.975))
quantile(df_strategy$vaccinated[df_strategy$sexorientation == "MSW" & df_strategy$strategy == "by history + HIV status + PrEP use"], c(0.5, 0.025, 0.975))
quantile(df_strategy$vaccinated[df_strategy$sexorientation == "Women" & df_strategy$strategy == "by history + HIV status + PrEP use"], c(0.5, 0.025, 0.975))

quantile(df_strategy$FOI[df_strategy$sexorientation == "MSM" & df_strategy$strategy == "No vaccination"], c(0.5, 0.025, 0.975))
quantile(df_strategy$FOI[df_strategy$sexorientation == "MSW" & df_strategy$strategy == "No vaccination"], c(0.5, 0.025, 0.975))
quantile(df_strategy$FOI[df_strategy$sexorientation == "Women" & df_strategy$strategy == "No vaccination"], c(0.5, 0.025, 0.975))

quantile(df_strategy$FOI[df_strategy$sexorientation == "MSM" & df_strategy$strategy == "by history"], c(0.5, 0.025, 0.975))
quantile(df_strategy$FOI[df_strategy$sexorientation == "MSW" & df_strategy$strategy == "by history"], c(0.5, 0.025, 0.975))
quantile(df_strategy$FOI[df_strategy$sexorientation == "Women" & df_strategy$strategy == "by history"], c(0.5, 0.025, 0.975))

quantile(df_strategy$FOI[df_strategy$sexorientation == "MSM" & df_strategy$strategy == "by history + HIV status + PrEP use"], c(0.5, 0.025, 0.975))
quantile(df_strategy$FOI[df_strategy$sexorientation == "MSW" & df_strategy$strategy == "by history + HIV status + PrEP use"], c(0.5, 0.025, 0.975))
quantile(df_strategy$FOI[df_strategy$sexorientation == "Women" & df_strategy$strategy == "by history + HIV status + PrEP use"], c(0.5, 0.025, 0.975))

quantile(df_strategy$FOI[df_strategy$sexorientation == "MSM" & df_strategy$strategy == "to all"], c(0.5, 0.025, 0.975))
quantile(df_strategy$FOI[df_strategy$sexorientation == "MSW" & df_strategy$strategy == "to all"], c(0.5, 0.025, 0.975))
quantile(df_strategy$FOI[df_strategy$sexorientation == "Women" & df_strategy$strategy == "to all"], c(0.5, 0.025, 0.975))

quantile(df_strategy$impact[df_strategy$sexorientation == "MSM" & df_strategy$strategy == "to all"], c(0.5, 0.025, 0.975))
quantile(df_strategy$impact[df_strategy$sexorientation == "MSW" & df_strategy$strategy == "to all"], c(0.5, 0.025, 0.975))
quantile(df_strategy$impact[df_strategy$sexorientation == "Women" & df_strategy$strategy == "to all"], c(0.5, 0.025, 0.975))

quantile(df_strategy$caseaverted[df_strategy$sexorientation == "MSM" & df_strategy$strategy == "to all"], c(0.5, 0.025, 0.975))
quantile(df_strategy$caseaverted[df_strategy$sexorientation == "MSW" & df_strategy$strategy == "to all"], c(0.5, 0.025, 0.975))
quantile(df_strategy$caseaverted[df_strategy$sexorientation == "Women" & df_strategy$strategy == "to all"], c(0.5, 0.025, 0.975))

Years_QALY = 5
df_strategy$caseavertedpercent = 100*(df_strategy$caseaverted)/(Years_QALY * df_strategy$numrepeat)

quantile(df_strategy$caseavertedpercent[df_strategy$sexorientation == "MSM" & df_strategy$strategy == "to all"], c(0.5, 0.025, 0.975))
quantile(df_strategy$caseavertedpercent[df_strategy$sexorientation == "MSW" & df_strategy$strategy == "to all"], c(0.5, 0.025, 0.975))
quantile(df_strategy$caseavertedpercent[df_strategy$sexorientation == "Women" & df_strategy$strategy == "to all"], c(0.5, 0.025, 0.975))

quantile(df_strategy$numberneededtotreat[df_strategy$sexorientation == "MSM" & df_strategy$strategy == "by history"], c(0.5, 0.025, 0.975))
quantile(df_strategy$numberneededtotreat[df_strategy$sexorientation == "MSM" & df_strategy$strategy == "to all"], c(0.5, 0.025, 0.975))

quantile(df_strategy$numberneededtotreat[df_strategy$sexorientation == "MSW" & df_strategy$strategy == "by HIV status"], c(0.5, 0.025, 0.975))
quantile(df_strategy$numberneededtotreat[df_strategy$sexorientation == "Women" & df_strategy$strategy == "by HIV status"], c(0.5, 0.025, 0.975))

quantile(df_strategy$numberneededtotreat[df_strategy$sexorientation == "MSW" & df_strategy$strategy == "to all"], c(0.5, 0.025, 0.975))
quantile(df_strategy$numberneededtotreat[df_strategy$sexorientation == "Women" & df_strategy$strategy == "to all"], c(0.5, 0.025, 0.975))

quantile(df_strategy$QALYsgained[df_strategy$sexorientation == "MSM" & df_strategy$strategy == "to all"], c(0.5, 0.025, 0.975))
quantile(df_strategy$QALYsgained[df_strategy$sexorientation == "MSW" & df_strategy$strategy == "to all"], c(0.5, 0.025, 0.975))
quantile(df_strategy$QALYsgained[df_strategy$sexorientation == "Women" & df_strategy$strategy == "to all"], c(0.5, 0.025, 0.975))

quantile(df_strategy$numberneededtotreat_QALY[df_strategy$sexorientation == "Women" & df_strategy$strategy == "to all"], c(0.5, 0.025, 0.975))
quantile(df_strategy$numberneededtotreat_QALY[df_strategy$sexorientation == "MSM" & df_strategy$strategy == "to all"], c(0.5, 0.025, 0.975))
quantile(df_strategy$numberneededtotreat_QALY[df_strategy$sexorientation == "MSW" & df_strategy$strategy == "to all"], c(0.5, 0.025, 0.975))

quantile(df_strategy$numberneededtotreat_QALY[df_strategy$sexorientation == "MSM" & df_strategy$strategy == "by history"], c(0.5, 0.025, 0.975))



p1 <- plot_convergence(df_strategy, "MSM", c(3000, 1000), ylim = c(0, 12))
p2 <- plot_convergence(df_strategy, "MSW", c(3000, 1000), ylim = c(0,12))
p3 <- plot_convergence(df_strategy, "Women", c(3000, 1000), ylim = c(0,12))
ptot <- p1/ p2+p3 +
  plot_layout(guides = "collect", axes = "collect", axis_titles = "collect")  & 
  theme(legend.position = "bottom")

ggsave("figs/convergence_assessment.png", width = 8, height = 10)

ptot


dftemp <- df_strategy[df_strategy$sexorientation == "MSM" & df_strategy$paramnum == 1, ]
dftemp <- dftemp[dftemp$strategy != "No vaccination",]


df_probs_msm <- manyruns_efficiencyfrontier(df_strategy, "MSM")
df_probs_msw <- manyruns_efficiencyfrontier(df_strategy, "MSW")
df_probs_women <- manyruns_efficiencyfrontier(df_strategy, "Women")

df_probs <- rbind(df_probs_msm, df_probs_msw, df_probs_women)
df_probs <- df_probs |> filter(strategy != "No vaccination")
df_probs$colval <- df_strategy[df_strategy$strategy!= "No vaccination" & df_strategy$paramnum == 1, ]$colval
df_probs$fillval <- df_strategy[df_strategy$strategy!= "No vaccination" & df_strategy$paramnum == 1, ]$fillval
df_probs$strategy <- factor(
  df_probs$strategy,
  levels = levels(df_strategy$strategy)
)

df_probs_msm$P_optimal[df_probs_msm$strategy == "by history"]
df_probs_msw$P_optimal[df_probs_msw$strategy == "by HIV status"]
df_probs_women$P_optimal[df_probs_women$strategy == "by HIV status"]


pfigtemp <- barplot_vertical(df_probs, scaler = 100, outputstring =  "P_optimal", title = "a", ymin = 0, ymax = 105, ylabel = "Probability strategy is \n most efficient (%)", textlabels = "P_optimal")

pfigtemp

pfigtemp2 <- barplot_vertical(df_probs, scaler = 100, outputstring =  "P_frontier", title = "b", ymin = 0, ymax = 105, ylabel = "Probability strategy is \n on efficiency frontier (%)", textlabels = "P_optimal")

pfigtemp2

p_fig5 <- pfigtemp/
  pfigtemp2 +
  plot_layout(guides = 'collect') & 
  theme(legend.position = "bottom")

ggsave("figs/Fig5_combined.png", p_fig5, width = 9, height = 9)
ggsave("figs/Fig5_combined.pdf", p_fig5, width = 9, height = 9)


temp_MSW <- prob_optimal(df_strategy, "MSW", "by HIV status")
temp_Women <- prob_optimal(df_strategy, "Women", "by HIV status")


prob_compare2(df_strategy, "to all")

## Main Figures

pfig1a <- figplot_vertical_vaccinatednum(df_strategy, scaler = 1, outputstring =  "vaccinatednum", title = "a", ymin = 0, ymax = 5500, ylabel = "# offered vaccination \n at diagnosis", colsorrows = "cols", textlabels = "vaccinated" )
pfig1b <- figplot_vertical(df_strategy, scaler = 1000, outputstring =  "FOI", title = "b", ymin = 0, ymax = 250, ylabel = "Rate of Repeat-GC diagnosis \n per 1000 person years", textlabels = "FOI")

p_fig1 <- pfig1a/
  pfig1b +
  plot_layout(guides = 'collect') & 
  theme(legend.position = "bottom")

ggsave("figs/Fig1_combined.png", p_fig1, width = 9, height = 9)
ggsave("figs/Fig1_combined.pdf", p_fig1, width = 9, height = 9)


pfig2a <- figplot_vertical(df_strategy %>% filter(stratval > 1), scaler = 1, outputstring =  "caseaverted", title = "a", ymin = 0, ymax = 2800, ylabel = "Total # of repeat diagnoses \n averted over five years", colsorrows = "cols", textlabels = "caseavertedpercent")
pfig2b <- figplot_vertical(df_strategy %>% filter(stratval > 1), scaler = 1, outputstring =  "numberneededtotreat", title = "b", ymin = 0, ymax = 125, y = "Number needed to vaccinate \n to avert one repeat diagnosis \n over five years", colsorrows = "cols", textlabels = "numberneededtotreat")

p_fig2 <- pfig2a/
  pfig2b +
  plot_layout(guides = 'collect') & 
  theme(legend.position = "bottom")

ggsave("figs/Fig2_combined.png", p_fig2,  width = 9, height = 9)
ggsave("figs/Fig2_combined.pdf", p_fig2,  width = 9, height = 9)


##outputs 2 year duration of protection
df_strategy$caseaverted2yr  = 0.4*df_strategy$caseaverted
df_strategy$numberneededtotreat2yr  = 2.5*df_strategy$numberneededtotreat

pfig2c <- figplot_vertical(df_strategy %>% filter(stratval > 1), scaler = 1, outputstring =  "caseaverted2yr", title = "a", ymin = 0, ymax = 1200, ylabel = "Total # of repeat diagnoses \n averted over two years", colsorrows = "cols", textlabels = "caseavertedpercent")
pfig2d <- figplot_vertical(df_strategy %>% filter(stratval > 1), scaler = 1, outputstring =  "numberneededtotreat2yr", title = "b", ymin = 0, ymax = 350, y = "Number needed to vaccinate \n to avert one repeat diagnosis \n over two years", colsorrows = "cols", textlabels = "numberneededtotreat2yr")

p_fig2yr <- pfig2c/
  pfig2d +
  plot_layout(guides = 'collect') & 
  theme(legend.position = "bottom")

ggsave("figs/Fig2_combined_2yr.png", p_fig2yr,  width = 9, height = 9)


pfig3a <- figplot_vertical(df_strategy %>% filter(stratval > 1), scaler = 1, outputstring =  "QALYsgained", title = "a", ymin = 0, ymax = 12, ylabel = "QALYs gained from  \n Repeat-GC averted over five years", colsorrows = "cols", textlabels = "QALYsgained")
pfig3b <- figplot_vertical_nntQALY(df_strategy %>% filter(stratval > 1), scaler = 0.001, outputstring =  "numberneededtotreat_QALY", title = "b", ymin = 0, ymax = 50, y = "Number (thousands) needed to \n vaccinate  to gain one QALY \n from repeat-GC averted over five years", colsorrows = "cols", textlabels = "numberneededtotreat_QALY")


p_fig3 <- pfig3a /
  pfig3b +
  plot_layout(guides = 'collect') & 
  theme(legend.position = "bottom")

ggsave("figs/Fig3_combined.png", p_fig3, width = 9, height = 9)
ggsave("figs/Fig3_combined.pdf", p_fig3, width = 9, height = 9)



# find efficiency frontiers using BCEA
ptemp <- efficiencyplot(df_strategy_orig, sexorientation = "MSM", title = "MSM", xlab = "% of Repeat-GC averted", ylab = "% of cases vaccinated", xmin = 0, xmax = 45, palette = palette2)
ptemp <- efficiencyplot(df_strategy_orig, sexorientation = "MSW", title = "MSW", xlab = "% of Repeat-GC averted", ylab = "% of cases vaccinated", xmin = 0, xmax = 45, palette = palette2)
ptemp <- efficiencyplot(df_strategy_orig, sexorientation = "Women", title = "Women", xlab = "% of Repeat-GC averted", ylab = "% of cases vaccinated", xmin = 0, xmax = 45, palette = palette2)


#efficiency plots
pfig4a <- efficiencyplot2(df_strategy_orig, sexorientation = "MSM", title = "a", xlab = "# QALYs gained from averting \n repeat-GC over five years \n among MSM", ylab = "# of MSM vaccinated", xmin = 0, xmax = 3, palette = palette2,   frontierstrats = c("1. No vaccination", "4. by history" , "6. by history + HIV status", "8. by history + HIV status + PrEP use", "9. to all"), ymin = 0, ymax = 5000)
pfig4b <- efficiencyplot2(df_strategy_orig, sexorientation = "MSW", title = "b", xlab = "# QALYs gained from averting \n repeat-GC over five years \n among MSW", ylab = "# of MSW vaccinated", xmin = 0, xmax = 3, palette = palette2, frontierstrats = c("1. No vaccination", "2. by HIV status" , "4. by history", "6. by history + HIV status", "9. to all"), ymin = 0, ymax = 5000)
pfig4c <- efficiencyplot2(df_strategy_orig, sexorientation = "Women", title = "c", xlab = "# QALYs gained from averting \n repeat-GC over five years \n among women", ylab = "# of women vaccinated", xmin = 0, xmax = 3, palette = palette2, frontierstrats = c("1. No vaccination", "2. by HIV status" , "5. by HIV status + PrEP use", "6. by history + HIV status", "8. by history + HIV status + PrEP use", "9. to all"), ymin = 0, ymax = 5000)


p_frontier <- pfig4a/
  pfig4b + pfig4c

ggsave("figs/Fig4_combined.png", p_frontier, width = 8, height = 10)
ggsave("figs/Fig4_combined.pdf", p_frontier, width = 8, height = 10)



##calculate incremental NNVs across frontier

frontier = c("by history", "by history + HIV status", "by history + HIV status + PrEP use", "to all")
notonfrontier = c("by HIV status", "by PrEP use", "by HIV status + PrEP use", "by history + PrEP use")
incremental_table_msm <- create_incremental_table(df_strategy, "MSM", frontier, notonfrontier)
write.csv(incremental_table_msm, "figs/incremental_table_msm.csv", row.names = FALSE)


frontier = c("by HIV status", "by history", "by history + HIV status", "to all")
notonfrontier = c("by PrEP use", "by HIV status + PrEP use", "by history + PrEP use", "by history + HIV status + PrEP use")
incremental_table_msw <- create_incremental_table(df_strategy, "MSW", frontier, notonfrontier)
write.csv(incremental_table_msw, "figs/incremental_table_msw.csv", row.names = FALSE)


frontier = c("by HIV status", "by HIV status + PrEP use", "by history + HIV status", "by history + HIV status + PrEP use", "to all")
notonfrontier = c("by PrEP use", "by history", "by history + HIV status + PrEP use")
incremental_table_women <- create_incremental_table(df_strategy, "Women", frontier, notonfrontier)
write.csv(incremental_table_women, "figs/incremental_table_women.csv", row.names = FALSE)


## Supplementary Figures

df_0 <- readRDS("inputs/df_N.rds")
df <- df_0[df_0$type != "Sampled target",]
p1 <- FOIcomparisonplot(df, subgroup = "MSM", subgrouplabel = "MSM", ymin = 0, ymax = 65000, "N", "Nmin", "Nmax", "# of initial cases", palette_full)
p2 <- FOIcomparisonplot(df, subgroup = "non-MSM men", subgrouplabel = "MSW", ymin = 0, ymax = 65000, "N", "Nmin", "Nmax", "# of initial cases", palette_full)
p3 <- FOIcomparisonplot(df, subgroup = "Women", subgrouplabel = "Women", ymin = 0, ymax = 65000, "N", "Nmin", "Nmax", "# of initial cases", palette_full)

p_N <- p1 /
  p2 + p3 +
  plot_layout(guides = 'collect', heights = c(2,1,1)) & 
  theme(legend.position = "bottom")
ggsave("figs/SuppFig_N_model.png", p_N, width = 12, height = 15)


df_0 <- readRDS("inputs/df_FOI.rds")
df <- df_0[df_0$type != "Model (vaccinated)",]
df <- df[df$type != "Sampled target",]

df$type[df$type == "Model (unvaccinated)"] = "Model"
p1 <- FOIcomparisonplot(df, subgroup = "MSM", subgrouplabel = "MSM", ymin = 0, ymax = 500, "F", "Fmin", "Fmax", "Annual rate of repeat-GC diagnosis", palette_full)
p2 <- FOIcomparisonplot(df, subgroup = "non-MSM men", subgrouplabel = "MSW", ymin = 0, ymax = 500, "F", "Fmin", "Fmax", "Annual rate of repeat-GC diagnosis", palette_full)
p3 <- FOIcomparisonplot(df, subgroup = "Women", subgrouplabel = "Women", ymin = 0, ymax = 500, "F", "Fmin", "Fmax", "Annual rate of repeat-GC diagnosis", palette_full)

p_F <- p1 /
  p2 + p3 +
  plot_layout(guides = 'collect', heights = c(2,1,1)) & 
  theme(legend.position = "bottom")
ggsave("figs/SuppFig_F_model.png", p_F, width = 12, height = 15)


df_0 <- readRDS("inputs/df_NX.rds")
df <- df_0[df_0$type != "Sampled target",]
p1 <- FOIcomparisonplot(df, subgroup = "MSM", subgrouplabel = "MSM", ymin = 0, ymax = 25000, "N.X.", "N.X.min", "N.X.max", "# of cases with repeat-GC diagnosis", palette_full)
p2 <- FOIcomparisonplot(df, subgroup = "non-MSM men", subgrouplabel = "MSW", ymin = 0, ymax = 25000, "N.X.", "N.X.min", "N.X.max", "# of cases with repeat-GC diagnosis", palette_full)
p3 <- FOIcomparisonplot(df, subgroup = "Women", subgrouplabel = "Women", ymin = 0, ymax = 25000, "N.X.", "N.X.min", "N.X.max", "# of cases with repeat-GC diagnosis", palette_full)

p_NX <- p1 /
  p2 + p3 +
  plot_layout(guides = 'collect', heights = c(2,1,1)) & 
  theme(legend.position = "bottom")
ggsave("figs/SuppFig_NX.png", p_NX, width = 12, height = 15)


df_0 <- readRDS("inputs/df_D.rds")
df <- df_0[df_0$type != "Sampled target",]
p1 <- FOIcomparisonplot(df, subgroup = "MSM", subgrouplabel = "MSM", ymin = 0, ymax = 60000, "D", "Dmin", "Dmax", "Total # of Repeat-GC", palette_model)
p2 <- FOIcomparisonplot(df, subgroup = "non-MSM men", subgrouplabel = "MSW", ymin = 0, ymax = 60000, "D", "Dmin", "Dmax", "Total # of Repeat-GC", palette_model)
p3 <- FOIcomparisonplot(df, subgroup = "Women", subgrouplabel = "Women", ymin = 0, ymax = 60000, "D", "Dmin", "Dmax", "Total # of Repeat-GC", palette_model)
p <- p1 /
  p2 + p3 +
  plot_layout(guides = 'collect', heights = c(2,1,1)) & 
  theme(legend.position = "bottom")
p
ggsave("figs/SuppFig_D.png", width = 12, height = 15)


df_model <- readRDS("inputs/model_paramsandoutputs.rds")
df_model$prev =  100*df_model$prev
#Prevalence plot
p1 <- prevplot(df_model, subgroup = "MSM", subgrouplabel = "MSM", ymin = 0, ymax = 5, ylab = "Prevalence among partners \n x diagnosis probability (%)", outputstring = "prev")
p2 <- prevplot(df_model, subgroup = "non-MSM men", subgrouplabel = "MSW", ymin = 0, ymax = 5, ylab = "Prevalence among partners \n x diagnosis probability (%)", outputstring = "prev")
p3 <- prevplot(df_model, subgroup = "Women", subgrouplabel = "Women", ymin = 0, ymax = 5, ylab = "Prevalence among partners \n x diagnosis probability (%)", outputstring = "prev")

p_prev <- p1 /
  p2 + p3 +
  plot_layout(guides = 'collect', heights = c(2,1,1)) & 
  theme(legend.position = "bottom")

p_prev
ggsave("figs/prevplot.png", p_prev, width = 12, height = 15)


#Effectiveness
pfig_effectiveness <- figplot_vertical(df_strategy %>% filter(stratval > 1), scaler = 1, outputstring =  "efficacy", title = "", ymin = 0, ymax = 55, ylabel = "% repeat-GC averted \n among vaccinated", colsorrows = "cols", textlabels = "efficacy" )
ggsave("figs/SuppFig_Effectiveness.png", pfig_effectiveness, width = 9, height = 5)

























