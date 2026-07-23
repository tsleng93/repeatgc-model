
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


## Main Figures

pfig1a <- figplot_vertical_vaccinatednum(df_strategy, scaler = 1, outputstring =  "vaccinatednum", title = "A", ymin = 0, ymax = 5500, ylabel = "# offered vaccination \n at diagnosis", colsorrows = "cols", textlabels = "vaccinated" )
pfig1b <- figplot_vertical(df_strategy, scaler = 1000, outputstring =  "FOI", title = "B", ymin = 0, ymax = 250, ylabel = "Rate of Repeat-GC diagnosis \n per 1000 person years", textlabels = "FOI")

p_fig1 <- pfig1a/
  pfig1b +
  plot_layout(guides = 'collect') & 
  theme(legend.position = "bottom")

ggsave("figs/Fig1_combined.png", p_fig1, width = 9, height = 9)

pfig2a <- figplot_vertical(df_strategy %>% filter(stratval > 1), scaler = 1, outputstring =  "impactnum", title = "A", ymin = 0, ymax = 75, ylabel = "Reduction in rate of Repeat-GC \n diagnosis per 1000 person-years", colsorrows = "cols", textlabels = "impact")
pfig2b <- figplot_vertical(df_strategy %>% filter(stratval > 1), scaler = 1, outputstring =  "numberneededtotreat", title = "B", ymin = 0, ymax = 55, y = "Number needed to vaccinate \n to avert one repeat diagnosis \n over five years", colsorrows = "cols", textlabels = "numberneededtotreat")

p_fig2 <- pfig2a/
  pfig2b +
  plot_layout(guides = 'collect') & 
  theme(legend.position = "bottom")

ggsave("figs/Fig2_combined.png", p_fig2,  width = 9, height = 9)


pfig3a <- figplot_vertical(df_strategy %>% filter(stratval > 1), scaler = 1, outputstring =  "QALYsgained", title = "A", ymin = 0, ymax = 11, ylabel = "QALYs gained from  \n Repeat-GC averted over five years", colsorrows = "cols", textlabels = "QALYsgained")
pfig3b <- figplot_vertical_nntQALY(df_strategy %>% filter(stratval > 1), scaler = 0.001, outputstring =  "numberneededtotreat_QALY", title = "B", ymin = 0, ymax = 45, y = "Number (thousands) needed to \n vaccinate  to gain one QALY \n from repeat-GC averted over five years", colsorrows = "cols", textlabels = "numberneededtotreat_QALY")


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
pfig4a <- efficiencyplot2(df_strategy_orig, sexorientation = "MSM", title = "A - MSM", xlab = "# QALYs gained from averting \n repeat-GC over five years", ylab = "# of cases vaccinated", xmin = 0, xmax = 3, palette = palette2,   frontierstrats = c("1. No vaccination", "4. by history" , "6. by history + HIV status", "8. by history + HIV status + PrEP use", "9. to all"), ymin = 0, ymax = 5000)
pfig4b <- efficiencyplot2(df_strategy_orig, sexorientation = "MSW", title = "B - MSW", xlab = "# QALYs gained from averting \n repeat-GC over five years", ylab = "# of cases vaccinated", xmin = 0, xmax = 3, palette = palette2, frontierstrats = c("1. No vaccination", "2. by HIV status" , "4. by history", "6. by history + HIV status", "9. to all"), ymin = 0, ymax = 5000)
pfig4c <- efficiencyplot2(df_strategy_orig, sexorientation = "Women", title = "C - Women", xlab = "# QALYs gained from averting \n repeat-GC over five years", ylab = "# of cases vaccinated", xmin = 0, xmax = 3, palette = palette2, frontierstrats = c("1. No vaccination", "2. by HIV status" , "5. by HIV status + PrEP use", "6. by history + HIV status", "8. by history + HIV status + PrEP use", "9. to all"), ymin = 0, ymax = 5000)


p_frontier <- pfig4a/
  pfig4b + pfig4c

ggsave("figs/Fig4_combined.png", p_frontier, width = 8, height = 10)






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


df_model <- readRDS("inputs/model_paramsandoutputs.rds")
df_model$prev =  100*df_model$prev
#Prevalence plot
p1 <- prevplot(df_model, subgroup = "MSM", subgrouplabel = "MSM", ymin = 0, ymax = 5, ylab = "Prevalence among partners (%)", outputstring = "prev")
p2 <- prevplot(df_model, subgroup = "non-MSM men", subgrouplabel = "MSW", ymin = 0, ymax = 5, ylab = "Prevalence among partners (%)", outputstring = "prev")
p3 <- prevplot(df_model, subgroup = "Women", subgrouplabel = "Women", ymin = 0, ymax = 5, ylab = "Prevalence among partners (%)", outputstring = "prev")

p_prev <- p1 /
  p2 + p3 +
  plot_layout(guides = 'collect', heights = c(2,1,1)) & 
  theme(legend.position = "bottom")

p_prev
ggsave("figs/prevplot.png", p_prev, width = 12, height = 15)


#Effectiveness
pfig_effectiveness <- figplot_vertical(df_strategy %>% filter(stratval > 1), scaler = 1, outputstring =  "efficacy", title = "", ymin = 0, ymax = 45, ylabel = "% repeat-GC averted \n among vaccinated", colsorrows = "cols", textlabels = "efficacy" )
ggsave("figs/SuppFig_Effectiveness.png", pfig_effectiveness, width = 9, height = 5)






















