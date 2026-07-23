
## sensitivity - per act efficacy
if (!dir.exists("figs")) dir.create("figs")

df_strategy_main <- readRDS("inputs/strategies_main.rds")
df_strategy_2 <- readRDS("inputs/strategies_2.rds")
df_strategy_3 <- readRDS("inputs/strategies_3.rds")
df_strategy_4 <- readRDS("inputs/strategies_4.rds")
df_strategy_5 <- readRDS("inputs/strategies_5.rds")
df_strategy_6 <- readRDS("inputs/strategies_6.rds")
df_strategy_7 <- readRDS("inputs/strategies_7.rds")
df_strategy_8 <- readRDS("inputs/strategies_8.rds")
df_strategy_9 <- readRDS("inputs/strategies_9.rds")
df_strategy_10 <- readRDS("inputs/strategies_10.rds")
df_strategy_11 <- readRDS("inputs/strategies_11.rds")
df_strategy_12 <- readRDS("inputs/strategies_12.rds")
df_strategy_13 <- readRDS("inputs/strategies_13.rds")
df_strategy_14 <- readRDS("inputs/strategies_14.rds")
df_strategy_15 <- readRDS("inputs/strategies_15.rds")
df_strategy_16 <- readRDS("inputs/strategies_16.rds")
df_strategy_17 <- readRDS("inputs/strategies_17.rds")



df_strategy_main2 <- df_strategy_main
df_strategy_main3 <- df_strategy_main
df_strategy_main4 <- df_strategy_main
df_strategy_main5 <- df_strategy_main
df_strategy_main6 <- df_strategy_main
df_strategy_main7 <- df_strategy_main
df_strategy_main8 <- df_strategy_main
df_strategy_main9 <- df_strategy_main
df_strategy_main10 <- df_strategy_main
df_strategy_main11 <- df_strategy_main
df_strategy_main12 <- df_strategy_main
df_strategy_main13 <- df_strategy_main
df_strategy_main14 <- df_strategy_main
df_strategy_main15 <- df_strategy_main
df_strategy_main16 <- df_strategy_main
df_strategy_main17 <- df_strategy_main


df_strategy_2$sensitivity = "b) per-act transmission probability"
df_strategy_3$sensitivity = "b) per-act transmission probability"
df_strategy_4$sensitivity = "c) sex acts per short-term partner"
df_strategy_5$sensitivity = "c) sex acts per short-term partner"
df_strategy_6$sensitivity = "d) sex acts per stable partner"
df_strategy_7$sensitivity = "d) sex acts per stable partner"
df_strategy_8$sensitivity = "h) QALY loss per infection"
df_strategy_9$sensitivity = "h) QALY loss per infection"
df_strategy_10$sensitivity = "a) per-act efficacy"
df_strategy_11$sensitivity = "a) per-act efficacy"
df_strategy_12$sensitivity = "e) relative uptake, PLHIV"
df_strategy_13$sensitivity = "e) relative uptake, PLHIV"
df_strategy_14$sensitivity = "f) relative uptake, PrEP users"
df_strategy_15$sensitivity = "f) relative uptake, PrEP users"
df_strategy_16$sensitivity = "g) relative uptake, GC history"
df_strategy_17$sensitivity = "g) relative uptake, GC history"

df_strategy_main2$sensitivity = "b) per-act transmission probability"
df_strategy_main3$sensitivity = "b) per-act transmission probability"
df_strategy_main4$sensitivity = "c) sex acts per short-term partner"
df_strategy_main5$sensitivity = "c) sex acts per short-term partner"
df_strategy_main6$sensitivity = "d) sex acts per stable partner"
df_strategy_main7$sensitivity = "d) sex acts per stable partner"
df_strategy_main8$sensitivity = "h) QALY loss per infection"
df_strategy_main9$sensitivity = "h) QALY loss per infection"
df_strategy_main10$sensitivity = "a) per-act efficacy"
df_strategy_main11$sensitivity = "a) per-act efficacy"
df_strategy_main12$sensitivity = "e) relative uptake, PLHIV"
df_strategy_main13$sensitivity = "e) relative uptake, PLHIV"
df_strategy_main14$sensitivity = "f) relative uptake, PrEP users"
df_strategy_main15$sensitivity = "f) relative uptake, PrEP users"
df_strategy_main16$sensitivity = "g) relative uptake, GC history"
df_strategy_main17$sensitivity = "g) relative uptake, GC history"


df_strategy_2$stratnum = 9
df_strategy_3$stratnum= 9
df_strategy_4$stratnum = 8
df_strategy_5$stratnum = 8
df_strategy_6$stratnum = 7
df_strategy_7$stratnum = 7
df_strategy_8$stratnum = 1
df_strategy_9$stratnum = 1
df_strategy_10$stratnum = 10
df_strategy_11$stratnum = 10
df_strategy_12$stratnum = 4
df_strategy_13$stratnum = 4
df_strategy_14$stratnum = 3
df_strategy_15$stratnum = 3
df_strategy_16$stratnum = 2
df_strategy_17$stratnum = 2

df_strategy_main2$stratnum = 9
df_strategy_main3$stratnum = 9
df_strategy_main4$stratnum = 8
df_strategy_main5$stratnum = 8
df_strategy_main6$stratnum = 7
df_strategy_main7$stratnum = 7
df_strategy_main8$stratnum = 1
df_strategy_main9$stratnum = 1
df_strategy_main10$stratnum = 10
df_strategy_main11$stratnum = 10
df_strategy_main12$stratnum = 4
df_strategy_main13$stratnum = 4
df_strategy_main14$stratnum = 3
df_strategy_main15$stratnum = 3
df_strategy_main16$stratnum = 2
df_strategy_main17$stratnum = 2

df_strategy_2$highorlow  = "low"
df_strategy_3$highorlow  = "high"
df_strategy_4$highorlow  = "low"
df_strategy_5$highorlow  = "high"
df_strategy_6$highorlow = "low"
df_strategy_7$highorlow  = "high"
df_strategy_8$highorlow  = "low"
df_strategy_9$highorlow = "high"
df_strategy_10$highorlow  = "low"
df_strategy_11$highorlow = "high"
df_strategy_12$highorlow  = "low"
df_strategy_13$highorlow = "high"
df_strategy_14$highorlow  = "low"
df_strategy_15$highorlow = "high"
df_strategy_16$highorlow  = "low"
df_strategy_17$highorlow = "high"

df_strategy_main2$highorlow = "main"
df_strategy_main3$highorlow  = "main"
df_strategy_main4$highorlow  = "main"
df_strategy_main5$highorlow  = "main"
df_strategy_main6$highorlow  = "main"
df_strategy_main7$highorlow  = "main"
df_strategy_main8$highorlow = "main"
df_strategy_main9$highorlow  = "main"
df_strategy_main10$highorlow  = "main"
df_strategy_main11$highorlow = "main"
df_strategy_main12$highorlow  = "main"
df_strategy_main13$highorlow = "main"
df_strategy_main14$highorlow  = "main"
df_strategy_main15$highorlow = "main"
df_strategy_main16$highorlow  = "main"
df_strategy_main17$highorlow = "main"

df_strategy_2$sensmarker = "sensitivity"
df_strategy_3$sensmarker  = "sensitivity"
df_strategy_4$sensmarker  = "sensitivity"
df_strategy_5$sensmarker  = "sensitivity"
df_strategy_6$sensmarker  = "sensitivity"
df_strategy_7$sensmarker  = "sensitivity"
df_strategy_8$sensmarker= "sensitivity"
df_strategy_9$sensmarker  = "sensitivity"
df_strategy_10$sensmarker  = "sensitivity"
df_strategy_11$sensmarker= "sensitivity"
df_strategy_12$sensmarker  = "sensitivity"
df_strategy_13$sensmarker = "sensitivity"
df_strategy_14$sensmarker  = "sensitivity"
df_strategy_15$sensmarker = "sensitivity"
df_strategy_16$sensmarker  = "sensitivity"
df_strategy_17$sensmarker = "sensitivity"

df_strategy_main2$sensmarker = "main"
df_strategy_main3$sensmarker  = "main"
df_strategy_main4$sensmarker  = "main"
df_strategy_main5$sensmarker = "main"
df_strategy_main6$sensmarker  = "main"
df_strategy_main7$sensmarker  = "main"
df_strategy_main8$sensmarker = "main"
df_strategy_main9$sensmarker  = "main"
df_strategy_main10$sensmarker  = "main"
df_strategy_main11$sensmarker = "main"
df_strategy_main12$sensmarker  = "main"
df_strategy_main13$sensmarker = "main"
df_strategy_main14$sensmarker  = "main"
df_strategy_main15$sensmarker = "main"
df_strategy_main16$sensmarker  = "main"
df_strategy_main17$sensmarker = "main"

df_strategy_combined = rbind(df_strategy_main2, df_strategy_main3,
                             df_strategy_main4, df_strategy_main5,
                             df_strategy_main6, df_strategy_main7,
                             df_strategy_main8, df_strategy_main9,
                             df_strategy_main10, df_strategy_main11,
                             df_strategy_main12, df_strategy_main13,
                             df_strategy_main14, df_strategy_main15,
                             df_strategy_main16, df_strategy_main17,
                             df_strategy_2, df_strategy_3,
                             df_strategy_4, df_strategy_5,
                             df_strategy_6, df_strategy_7,
                             df_strategy_8, df_strategy_9,
                             df_strategy_10, df_strategy_11,
                             df_strategy_12, df_strategy_13,
                             df_strategy_14, df_strategy_15,
                             df_strategy_16, df_strategy_17)


df_strategy_combined$sensitivity <- factor(df_strategy_combined$sensitivity, levels = unique(df_strategy_combined$sensitivity[order(df_strategy_combined$stratnum)]))


df_strategy_combined$numberneededtotreat = 1/df_strategy_combined$caseavertedperdose
df_strategy_combined$numberneededtotreat[df_strategy_combined$numberneededtotreat < 0] = -1000
df_strategy_combined$numberneededtotreat_QALY = df_strategy_combined$vaccinatednum/df_strategy_combined$QALYsgained
df_strategy_combined$numberneededtotreat_QALY[df_strategy_combined$numberneededtotreat_QALY < 0] = -10000
df_strategy_combined$numberneededtotreat_QALY[is.na(df_strategy_combined$numberneededtotreat_QALY)] = -10000

df_summary <- df_strategy_combined %>%
  group_by(strategy, sexorientation, sensitivity, highorlow, sensmarker) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")

#Incidence
df_summary_temp = df_summary[df_summary$strategy == "9. to all", ]

p1 <- figplot(df_summary_temp, scaler = 1, outputstring =  "impactnum", title = paste("50% per act efficacy", "", sep = ""), ymin = 0, ymax = 100, ylabel = "Reduction in repeat-GC \n rate per 1000 person-years", colsorrows = "rows")
p2 <- figplot(df_summary_temp, scaler = 1, outputstring =  "impact", title = paste("50% per act efficacy", "", sep = ""), ymin = 0, ymax = 60, ylabel = "% reduction in repeat-GC \n rate per 1000 person-years", colsorrows = "rows")
p3 <- figplot(df_summary_temp, scaler = 1, outputstring =  "numberneededtotreat", title = paste("50% per act efficacy", "", sep = ""), ymin = 0, ymax = 60, ylabel = "Number needed to \n vaccinate to avert one \n repeat-GC over five years", colsorrows = "rows")
p4 <- figplot(df_summary_temp, scaler = 1, outputstring =  "efficacy", title = paste("50% per act efficacy", "", sep = ""), ymin = 0, ymax = 100, ylabel = "% effectiveness", colsorrows = "rows")
p5 <- figplot(df_summary_temp, scaler = 1, outputstring =  "QALYsgained", title = paste("50% per act efficacy", "", sep = ""), ymin = 0, ymax = 12, ylabel = "QALYs gained from repeat-GC \n averted over five years", colsorrows = "rows")
p6 <- figplot(df_summary_temp, scaler = 0.001, outputstring =  "numberneededtotreat_QALY", title = paste("50% per act efficacy", "", sep = ""), ymin = 0, ymax = 45, ylabel = "Number (thousands) needed \n to vaccinate to \n gain one QALY", colsorrows = "rows")

library(patchwork)
p1 <- p1 + theme(legend.position = "none")
p2 <- p2 + theme(legend.position = "none")
sens1 <- (p1+ p2 + p5 + plot_layout(axes = "collect")) 
sens1
ggsave("figs/sensitivity_mainimpact.png", width = 10, height = 7)

p3 <- p3 + theme(legend.position = "none")
sens2 <- (p3 + p6 + plot_layout(axes = "collect")) 
sens2
ggsave("figs/sensitivity_mainefficiency.png", width = 9, height = 7)
