if (!dir.exists("figs")) dir.create("figs")

### Read inputs ###
degdist_msm <-  readRDS("inputs/degdist_msm.rds")
degdist_msw <-  readRDS("inputs/degdist_msw.rds")
degdist_women <-  readRDS("inputs/degdist_women.rds")


sampled_degdist_msm <-  readRDS("inputs/sampled_degdist_msm.rds")
sampled_degdist_msw <-  readRDS("inputs/sampled_degdist_msw.rds")
sampled_degdist_women <-  readRDS("inputs/sampled_degdist_women.rds")


library(dplyr)
library(ggplot2)



p1 <- plot_degdist_comparison(degdist_msm,
                        sampled_degdist_msm,
                        "nohistorynonuser_N",
                        "nohistorynonuser",
                        "nohistorynonusermin",
                        "nohistorynonusermax",
                        "HIV- non-PrEP user \n no history")


p2 <- plot_degdist_comparison(degdist_msm,
                        sampled_degdist_msm,
                        "nohistoryPrEPuser_N",
                        "nohistoryPrEPuser",
                        "nohistoryPrEPusermin",
                        "nohistoryPrEPusermax",
                        "HIV- PrEP user \n no history")

p3 <- plot_degdist_comparison(degdist_msm,
                        sampled_degdist_msm,
                        "nohistoryHIV_N",
                        "nohistoryHIV",
                        "nohistoryHIVmin",
                        "nohistoryHIVmax",
                        "PLHIV \n no history")


p4 <- plot_degdist_comparison(degdist_msm,
                        sampled_degdist_msm,
                        "yeshistorynonuser_N",
                        "yeshistorynonuser",
                        "yeshistorynonusermin",
                        "yeshistorynonusermax",
                        "HIV- non-PrEP user \n with history")

p5 <- plot_degdist_comparison(degdist_msm,
                        sampled_degdist_msm,
                        "yeshistoryPrEPuser_N",
                        "yeshistoryPrEPuser",
                        "yeshistoryPrEPusermin",
                        "yeshistoryPrEPusermax",
                        "HIV- PrEP user \n with history")

p6<- plot_degdist_comparison(degdist_msm,
                        sampled_degdist_msm,
                        "yeshistoryHIV_N",
                        "yeshistoryHIV",
                        "yeshistoryHIVmin",
                        "yeshistoryHIVmax",
                        "PLHIV \n with history")


p_tot <- (p1 + p4)/(p2+p5) + (p3 + p6)  +
  plot_layout(guides = "collect", axes = "collect", axis_titles = "collect")  + 
  plot_annotation(title = "MSM") &
  theme(legend.position = "bottom",     plot.title = element_text(size = 16, face = "bold"))


p_tot
ggsave("figs/degdistsmsm.png", width = 8, height = 10)


p1 <- plot_degdist_comparison(degdist_msw,
                              sampled_degdist_msw,
                              "nohistorynonuser_N",
                              "nohistorynonuser",
                              "nohistorynonusermin",
                              "nohistorynonusermax",
                              "HIV- non-PrEP user \n no history")


p2 <- plot_degdist_comparison(degdist_msw,
                              sampled_degdist_msw,
                              "nohistoryPrEPuser_N",
                              "nohistoryPrEPuser",
                              "nohistoryPrEPusermin",
                              "nohistoryPrEPusermax",
                              "HIV- PrEP user \n no history")

p3 <- plot_degdist_comparison(degdist_msw,
                              sampled_degdist_msw,
                              "nohistoryHIV_N",
                              "nohistoryHIV",
                              "nohistoryHIVmin",
                              "nohistoryHIVmax",
                              "PLHIV \n no history")


p4 <- plot_degdist_comparison(degdist_msw,
                              sampled_degdist_msw,
                              "yeshistorynonuser_N",
                              "yeshistorynonuser",
                              "yeshistorynonusermin",
                              "yeshistorynonusermax",
                              "HIV- non-PrEP user \n with history")

p5 <- plot_degdist_comparison(degdist_msw,
                              sampled_degdist_msw,
                              "yeshistoryPrEPuser_N",
                              "yeshistoryPrEPuser",
                              "yeshistoryPrEPusermin",
                              "yeshistoryPrEPusermax",
                              "HIV- PrEP user \n with history")

p6<- plot_degdist_comparison(degdist_msw,
                             sampled_degdist_msw,
                             "yeshistoryHIV_N",
                             "yeshistoryHIV",
                             "yeshistoryHIVmin",
                             "yeshistoryHIVmax",
                             "PLHIV \n with history")


p_tot <- (p1 + p4)/(p2+p5) + (p3 + p6)  +
  plot_layout(guides = "collect", axes = "collect", axis_titles = "collect")  + 
  plot_annotation(title = "MSW") &
  theme(legend.position = "bottom",     plot.title = element_text(size = 16, face = "bold"))


p_tot
ggsave("figs/degdistsmsw.png", width = 8, height = 10)



p1 <- plot_degdist_comparison(degdist_women,
                              sampled_degdist_women,
                              "nohistorynonuser_N",
                              "nohistorynonuser",
                              "nohistorynonusermin",
                              "nohistorynonusermax",
                              "HIV- non-PrEP user \n no history")


p2 <- plot_degdist_comparison(degdist_women,
                              sampled_degdist_women,
                              "nohistoryPrEPuser_N",
                              "nohistoryPrEPuser",
                              "nohistoryPrEPusermin",
                              "nohistoryPrEPusermax",
                              "HIV- PrEP user \n no history")

p3 <- plot_degdist_comparison(degdist_women,
                              sampled_degdist_women,
                              "nohistoryHIV_N",
                              "nohistoryHIV",
                              "nohistoryHIVmin",
                              "nohistoryHIVmax",
                              "PLHIV \n no history")


p4 <- plot_degdist_comparison(degdist_women,
                              sampled_degdist_women,
                              "yeshistorynonuser_N",
                              "yeshistorynonuser",
                              "yeshistorynonusermin",
                              "yeshistorynonusermax",
                              "HIV- non-PrEP user \n with history")

p5 <- plot_degdist_comparison(degdist_women,
                              sampled_degdist_women,
                              "yeshistoryPrEPuser_N",
                              "yeshistoryPrEPuser",
                              "yeshistoryPrEPusermin",
                              "yeshistoryPrEPusermax",
                              "HIV- PrEP user \n with history")

p6<- plot_degdist_comparison(degdist_women,
                             sampled_degdist_women,
                             "yeshistoryHIV_N",
                             "yeshistoryHIV",
                             "yeshistoryHIVmin",
                             "yeshistoryHIVmax",
                             "PLHIV \n with history")


p_tot <- (p1 + p4)/(p2+p5) + (p3 + p6)  +
  plot_layout(guides = "collect", axes = "collect", axis_titles = "collect")  + 
  plot_annotation(title = "Women") &
  theme(legend.position = "bottom",     plot.title = element_text(size = 16, face = "bold"))


p_tot
ggsave("figs/degdistswomen.png", width = 8, height = 10)



my_palette = c("#a6cee3" , "#1f78b4", "#b2df8a","#33a02c", "#fb9a99",  "#e31a1c")


p_cum_combined_msm= plot_cumdegdist_combined(
  sampled_degdist = sampled_degdist_msm,
  modeloutputs = c(
    "nohistorynonuser_prob",
    "yeshistorynonuser_prob",
    "nohistoryPrEPuser_prob",
    "yeshistoryPrEPuser_prob",
    "nohistoryHIV_prob",
    "yeshistoryHIV_prob"
  ),
  labels = c(
    "HIV- non-PrEP user, no history",
    "HIV- non-PrEP user, with history",
    "HIV- PrEP user, no history",
    "HIV- PrEP user, with history",
    "PLHIV, no history",
    "PLHIV, with history"
  ),
  palette = my_palette,
  title = "MSM"
)

p_cum_combined_msm

p_cum_combined_msw <- plot_cumdegdist_combined(
  sampled_degdist = sampled_degdist_msw,
  modeloutputs = c(
    "nohistorynonuser_prob",
    "yeshistorynonuser_prob",
    "nohistoryPrEPuser_prob",
    "yeshistoryPrEPuser_prob",
    "nohistoryHIV_prob",
    "yeshistoryHIV_prob"
  ),
  labels = c(
    "HIV- non-PrEP user, no history",
    "HIV- non-PrEP user, with history",
    "HIV- PrEP user, no history",
    "HIV- PrEP user, with history",
    "PLHIV, no history",
    "PLHIV, with history"
  ),
  palette = my_palette,
  title = "MSW"
)

p_cum_combined_msw


p_cum_combined_women <- plot_cumdegdist_combined(
  sampled_degdist = sampled_degdist_women,
  modeloutputs = c(
    "nohistorynonuser_prob",
    "yeshistorynonuser_prob",
    "nohistoryPrEPuser_prob",
    "yeshistoryPrEPuser_prob",
    "nohistoryHIV_prob",
    "yeshistoryHIV_prob"
  ),
  labels = c(
    "HIV- non-PrEP user, no history",
    "HIV- non-PrEP user, with history",
    "HIV- PrEP user, no history",
    "HIV- PrEP user, with history",
    "PLHIV, no history",
    "PLHIV, with history"
  ),
  palette = my_palette,
  title = "Women"
)

p_cum_combined_women


p_tot <- (p_cum_combined_msm)/(p_cum_combined_msw) + (p_cum_combined_women)  +
  plot_layout(guides = "collect", axes = "collect", axis_titles = "collect")  & 
  theme(legend.position = "bottom")


p_tot
ggsave("figs/cumdegdists.png", width = 6, height = 10)

