
# used
stringstonumbers_df <- function(df, colindex){

  # change strings to numbers
  df$N <- as.numeric(gsub(",", "", df$N))
  df$N.X. <- as.numeric(gsub(",", "", df$N.X.))
  df$N.W. <- as.numeric(gsub(",", "", df$N.W.))
  df$D <- as.numeric(gsub(",", "", df$D))
  df$N..2.times. <- as.numeric(gsub(",", "", df$N..2.times.))
  df$N..3.times. <- as.numeric(gsub(",", "", df$N..3.times.))

  df$N.1.3months. <- as.numeric(gsub(",", "", df$N.1.3months.))
  df$N.3.6months. <- as.numeric(gsub(",", "", df$N.3.6months.))
  df$N.6.9months. <- as.numeric(gsub(",", "", df$N.6.9months.))
  df$N.9.12months. <- as.numeric(gsub(",", "", df$N.9.12months.))
  df$N.12.24months. <- as.numeric(gsub(",", "", df$N.12.24months.))
  df$N.24.months. <- as.numeric(gsub(",", "", df$N.24.months.))


  for (i in (colindex)){
    df[,i] = as.numeric(gsub("%", "", df[,i]))
  }


  df
}

# used
extract_numeric_values <- function(s) {
  if (nchar(s) > 0) {
    numeric_values <- as.numeric(unlist(strsplit(gsub("\\[|\\]", "", s), ",")))
    return(c(numeric_values[1], numeric_values[2]))
  } else {
    return(c(NA, NA))
  }
}

#used
extract_percent_values <- function(s) {
  if (nchar(s) > 0) {
    percent_values <- as.numeric(unlist(strsplit(gsub("\\[|\\]|%", "", s), ",")))
    return(c(min(percent_values), max(percent_values)))
  } else {
    return(c(NA, NA))
  }
}

#used
column_minandmax <- function(df, colindex, numericorpercent){

  for (i in (colindex)){

    column_name <- colnames(df)[i-1]
    column_name_min <- paste0(column_name, "min")
    column_name_max <- paste0(column_name, "max")

    if (numericorpercent == "numeric"){
      numeric_columns <- t(as.data.frame(t(do.call(rbind, lapply(df[,i], extract_numeric_values)))))
    } else{
      numeric_columns <- t(as.data.frame(t(do.call(rbind, lapply(df[,i], extract_percent_values)))))
    }
    colnames(numeric_columns) <- c(column_name_min, column_name_max)
    df <- cbind(df, numeric_columns)
  }

  df


}

#used
degdistmaker <- function(fileName, sheetName){

  temp <- read.xlsx(fileName, sheetName = sheetName)

  dftemp <- data.frame(partnernum = temp[2:nrow(temp), 1], nohistorynonuser = temp[2:nrow(temp), 38], nohistoryPrEPuser = temp[2:nrow(temp), 40], nohistoryHIV = temp[2:nrow(temp), 42], yeshistorynonuser = temp[2:nrow(temp), 44], yeshistoryPrEPuser = temp[2:nrow(temp), 46], yeshistoryHIV = temp[2:nrow(temp), 48])
  dftemp[, 1:7] <- lapply(dftemp[, 1:7], as.numeric)
  dftemp[, 2:7] <- sweep(dftemp[, 2:7], 2, colSums(dftemp[, 2:7]), FUN = "/")

  dftemp
}

degdistmaker_sample <- function(fileName, sheetName){
  
  temp <- read.xlsx(fileName, sheetName = sheetName)
  
  dftemp <- data.frame(partnernum = temp[2:nrow(temp), 1], 
                       nohistorynonuser = temp[2:nrow(temp), 38], 
                       nohistorynonuserrange = temp[2:nrow(temp), 39], 
                       nohistoryPrEPuser = temp[2:nrow(temp), 40],
                       nohistoryPrEPuserrange = temp[2:nrow(temp), 41],
                       nohistoryHIV = temp[2:nrow(temp), 42],
                       nohistoryHIVrange = temp[2:nrow(temp), 43],
                       yeshistorynonuser = temp[2:nrow(temp), 44],
                       yeshistorynonuserrange = temp[2:nrow(temp), 45], 
                       yeshistoryPrEPuser = temp[2:nrow(temp), 46],
                       yeshistoryPrEPuserrange = temp[2:nrow(temp), 47], 
                       yeshistoryHIV = temp[2:nrow(temp), 48],
                       yeshistoryHIVrange = temp[2:nrow(temp), 49]
  )
  
  dftemp <- column_minandmax(dftemp, 1 + 2*(1:6), "numeric")
  dftemp <- dftemp[,-(1 + 2*(1:6))]
  dftemp[] <- lapply(dftemp, function(x) {
    if (is.character(x)) as.numeric(x) else x
  })
  
  
  dftemp
}

#used
stan_distributions <- function(M, N, npar, partnermax, orientation, stanfile, seed){

  data_list <- list(
    K = 3,
    Trials = N,
    Successes = M
  )

  #thinning <- 10

  set.seed(seed)
  ## Fit MSM distribution using STAN (MCMC)
  fittemp <- stan(file = stanfile, data = data_list,
                 iter = 20000, chains = 4, thin = 1, seed = seed)

  ## Extract specific R-hat and ESS values for all parameters

  rhat_values <- summary(fittemp)$summary[, "Rhat"]
  ess_values <- summary(fittemp)$summary[, "n_eff"]

  print("rhat")
  print(rhat_values)
  print("ess")
  print(ess_values)


  posterior <- as.array(fittemp)
  parameter_names <- dimnames(posterior)$parameters


  palette = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3")

  parameters_to_plot <- "theta_1"
  ## MCMC diagnostics
  trace_temp1 <- traceplot(fittemp, pars = parameters_to_plot) + ggtitle(paste0("theta_1, ", orientation)) +
    theme_light() +
    xlab("Iteration") +
    ylab(" Value") +
    theme(
      legend.position = "bottom",
      axis.text.x = ggplot2::element_text( angle = 45, hjust = 1),
      axis.text = ggplot2::element_text(size = 14),
      axis.title = ggplot2::element_text(size = 14),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.background=element_rect(fill='white', colour='black'),
      strip.background=element_rect(fill='white', colour='white'),
      strip.text.x = element_text(size = 14),
      strip.text.y = element_text(size = 14),
      axis.text.y = element_text( size = 14),
      legend.title = element_blank(),
      legend.text = element_text (size =14),
      title = element_text (size = 15))  +
    scale_color_manual(values = palette) + coord_cartesian(ylim =c(0, 3))


  overlay_temp1 <- mcmc_dens_overlay(fittemp, pars = parameters_to_plot) + ggtitle(paste0("theta_1, ", orientation)) +
    theme_light() +
    xlab("Value") +
    ylab("Density") + theme(
      legend.position = "bottom",
      axis.text.x = ggplot2::element_text( angle = 45, hjust = 1),
      axis.text = ggplot2::element_text(size = 14),
      axis.title = ggplot2::element_text(size = 14),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.background=element_rect(fill='white', colour='black'),
      strip.background=element_rect(fill='white', colour='white'),
      strip.text.x = element_text(size = 14),
      strip.text.y = element_text(size = 14),
      axis.text.y = element_text( size = 14),
      legend.title = element_blank(),
      legend.text = element_text (size =14),
      title = element_text (size = 15)) + coord_cartesian(xlim =c(0, 3), ylim = c(0, 4)) +
    scale_color_manual(values = palette)


  parameters_to_plot <- "theta_2"
  ## MCMC diagnostics
  trace_temp2 <- traceplot(fittemp, pars = parameters_to_plot) + ggtitle(paste0("theta_2, ", orientation)) +
    theme_light() +
    xlab("Iteration") +
    ylab(" Value") +
    theme(
      legend.position = "bottom",
      axis.text.x = ggplot2::element_text( angle = 45, hjust = 1),
      axis.text = ggplot2::element_text(size = 14),
      axis.title = ggplot2::element_text(size = 14),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.background=element_rect(fill='white', colour='black'),
      strip.background=element_rect(fill='white', colour='white'),
      strip.text.x = element_text(size = 14),
      strip.text.y = element_text(size = 14),
      axis.text.y = element_text( size = 14),
      legend.text = element_text (size =14),
      legend.title = element_blank(),
      title = element_text (size = 15))  +
    scale_color_manual(values = palette) + coord_cartesian(ylim =c(0, .75))


  overlay_temp2 <- mcmc_dens_overlay(fittemp, pars = parameters_to_plot) + ggtitle(paste0("theta_2, ", orientation)) +
    theme_light() +
    xlab("Value") +
    ylab("Density") + theme(
      legend.position = "bottom",
      axis.text.x = ggplot2::element_text( angle = 45, hjust = 1),
      axis.text = ggplot2::element_text(size = 14),
      axis.title = ggplot2::element_text(size = 14),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.background=element_rect(fill='white', colour='black'),
      strip.background=element_rect(fill='white', colour='white'),
      strip.text.x = element_text(size = 14),
      strip.text.y = element_text(size = 14),
      axis.text.y = element_text( size = 14),
      legend.text = element_text (size =14),
      legend.title = element_blank(),
      title = element_text (size = 15)) + coord_cartesian(xlim =c(0, 0.75), ylim = c(0, 10)) +
    scale_color_manual(values = palette)


  saveRDS(trace_temp1, file = paste0("figs/trace1_", orientation ,".rds"))
  saveRDS(trace_temp2, file = paste0("figs/trace2_", orientation ,".rds"))

  saveRDS(overlay_temp1, file = paste0("figs/overlay1_", orientation ,".rds"))
  saveRDS(overlay_temp2, file = paste0("figs/overlay2_", orientation ,".rds"))

  ## generate distribution from posterior parameters
  posterior_samples <- rstan::extract(fittemp)
  saveRDS(posterior_samples, file = paste0("data/posterior_omega_", orientation, ".rds"))

  
  iter <- 20000
  chains <- 4
  target_draws <- 5000
  
  thinning <- max(1, floor((iter / 2 * chains) / target_draws))
 # thinning <- 4000/npar


  temp <- matrix(nrow = npar, ncol = partnermax)

  for (i in 1:npar){
    mutemp = posterior_samples$theta_2[thinning*i]
    lambdatemp = posterior_samples$theta_1[thinning*i]
    temp[i,] =  mutemp + (1-mutemp)*(1-(1:partnermax)^(-lambdatemp))
  }

  omega <- as.data.frame(temp)
  omega$row_id <- 1:nrow(omega)
  saveRDS(omega, file = paste0("data/omega_", orientation, ".rds"))

  return(omega)

}

#used
Fig_degdist <- function(degdist_msm, degdist_msw, degdist_women, xmax){

degdist_msm_2 <- degdist_msm
degdist_msm_2[, 2:ncol(degdist_msm_2)] <- apply(degdist_msm_2[, 2:ncol(degdist_msm_2)], 2, cumsum)
degdist_msm_2 <- degdist_msm_2 |> pivot_longer(
  cols = c("nohistorynonuser", "nohistoryPrEPuser", "nohistoryHIV", "yeshistorynonuser", "yeshistoryPrEPuser", "yeshistoryHIV"),
  names_to = "subgroup",
  values_to = "value"
)
degdist_msm_2$orientation = "MSM"


degdist_msw_2 <- degdist_msw
degdist_msw_2[, 2:ncol(degdist_msw_2)] <- apply(degdist_msw_2[, 2:ncol(degdist_msw_2)], 2, cumsum)
degdist_msw_2 <- degdist_msw_2 |> pivot_longer(
  cols = c("nohistorynonuser", "nohistoryPrEPuser", "nohistoryHIV", "yeshistorynonuser", "yeshistoryPrEPuser", "yeshistoryHIV"),
  names_to = "subgroup",
  values_to = "value"
)
degdist_msw_2$orientation = "MSW"


degdist_women_2 <- degdist_women
degdist_women_2[, 2:ncol(degdist_women_2)] <- apply(degdist_women_2[, 2:ncol(degdist_women_2)], 2, cumsum)

degdist_women_2 <- degdist_women_2 |> pivot_longer(
  cols = c("nohistorynonuser", "nohistoryPrEPuser", "nohistoryHIV", "yeshistorynonuser", "yeshistoryPrEPuser", "yeshistoryHIV"),
  names_to = "subgroup",
  values_to = "value"
)


degdist_women_2$orientation = "Women"

degdist_2 <- rbind(degdist_msm_2, degdist_msw_2, degdist_women_2)

degdist_2$subgroup[degdist_2$subgroup == "nohistoryHIV"] = "PLHIV, no history"
degdist_2$subgroup[degdist_2$subgroup == "yeshistoryHIV"] = "PLHIV, with history"
degdist_2$subgroup[degdist_2$subgroup == "nohistorynonuser"] = "HIV- non-PrEP user, no history"
degdist_2$subgroup[degdist_2$subgroup == "yeshistorynonuser"] = "HIV- non-PrEP user, with history"
degdist_2$subgroup[degdist_2$subgroup == "nohistoryPrEPuser"] = "HIV- PrEP user, no history"
degdist_2$subgroup[degdist_2$subgroup == "yeshistoryPrEPuser"] = "HIV- PrEP user, with history"




palette = c("#a6cee3" ,"#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c")

p <- ggplot(data = degdist_2, aes(x = partnernum, y = 100*value, color = subgroup)) + geom_line() +
  xlab("# of sexual partners in 3 months, n") +
  ylab("% of subgroup with n or fewer partners") +
  theme_bw() +
  scale_color_manual(values = palette) +
  theme(
    legend.position = "bottom",
    axis.text.x = ggplot2::element_text( angle = 45, hjust = 1),
    axis.text = ggplot2::element_text(size = 14),
    axis.title = ggplot2::element_text(size = 14),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.background=element_rect(fill='white', colour='black'),
    strip.background=element_rect(fill='white', colour='white'),
    strip.text.x = element_text(size = 14),
    strip.text.y = element_text(size = 14),
    axis.text.y = element_text( size = 14),
    legend.title = element_blank(),
    legend.text = element_text (size =14),
    title = element_text (size = 15)) + coord_cartesian(xlim =c(0, xmax)) + facet_grid(cols = vars(orientation))

}


#used
Fig_omega <- function(omega_msm, omega_msw, omega_women, data_msm, data_msw, data_women, xmax){

  
  omega_msm_sens = omega_msm
  omega_msw_sens = omega_msw
  omega_women_sens = omega_women
  
  omega_msm_sens[, 3:511] = omega_msm[,3]
  omega_msw_sens[, 3:103] = omega_msw[,3]
  omega_women_sens[, 3:101] = omega_women[,3]
  

  # Reshape the data to long format
  df_long_msm <- omega_msm |>
    pivot_longer(
      cols = -row_id,  # All columns except row_id
      names_to = "variable",
      values_to = "value"
    ) |>
    mutate(variable = as.integer(gsub("V", "", variable)))  # Convert variable names to integers
  df_long_msm$orientation = "MSM"
  
  
  df_long_msm_sens <- omega_msm_sens |>
    pivot_longer(
      cols = -row_id,  # All columns except row_id
      names_to = "variable",
      values_to = "value"
    ) |>
    mutate(variable = as.integer(gsub("V", "", variable)))  # Convert variable names to integers
  df_long_msm_sens$orientation = "MSM"

df_long_msw <- omega_msw |>
  pivot_longer(
    cols = -row_id,  # All columns except row_id
    names_to = "variable",
    values_to = "value"
  ) |>
  mutate(variable = as.integer(gsub("V", "", variable)))  # Convert variable names to integers
df_long_msw$orientation = "MSW"

df_long_msw_sens <- omega_msw_sens |>
  pivot_longer(
    cols = -row_id,  # All columns except row_id
    names_to = "variable",
    values_to = "value"
  ) |>
  mutate(variable = as.integer(gsub("V", "", variable)))  # Convert variable names to integers
df_long_msw_sens$orientation = "MSW"

df_long_women <- omega_women |>
  pivot_longer(
    cols = -row_id,  # All columns except row_id
    names_to = "variable",
    values_to = "value"
  ) |>
  mutate(variable = as.integer(gsub("V", "", variable)))  # Convert variable names to integers
df_long_women$orientation = "Women"

df_long_women_sens <- omega_women_sens |>
  pivot_longer(
    cols = -row_id,  # All columns except row_id
    names_to = "variable",
    values_to = "value"
  ) |>
  mutate(variable = as.integer(gsub("V", "", variable)))  # Convert variable names to integers
df_long_women_sens$orientation = "Women"


df_long <- rbind(df_long_msm, df_long_msw, df_long_women)
df_long_sens <- rbind(df_long_msm_sens, df_long_msw_sens, df_long_women_sens)

  summary_stats <- df_long %>%
    group_by(variable, orientation) %>%
    summarize(
      median = median(value),
      q2.5 = quantile(value, 0.025),
      q97.5 = quantile(value, 0.975)
    )
  summary_stats$scenario = "main"
  
  summary_stats_sens <- df_long_sens %>%
    group_by(variable, orientation) %>%
    summarize(
      median = median(value),
      q2.5 = quantile(value, 0.025),
      q97.5 = quantile(value, 0.975)
    )
  summary_stats_sens$scenario = "sensitivity"
  
  summary_stats <- rbind(summary_stats, summary_stats_sens)
  
  
  data_msm$prop = data_msm$M/ data_msm$N
  binom.exact <- binom.confint(data_msm$M, data_msm$N, method = "exact")
  data_msm$lower = binom.exact$lower
  data_msm$upper = binom.exact$upper
  data_msm$orientation = "MSM"

  data_msw$prop = data_msw$M/ data_msw$N
  binom.exact <- binom.confint(data_msw$M, data_msw$N, method = "exact")
  data_msw$lower = binom.exact$lower
  data_msw$upper = binom.exact$upper
  data_msw$orientation = "MSW"

  data_women$prop = data_women$M/ data_women$N
  binom.exact <- binom.confint(data_women$M, data_women$N, method = "exact")
  data_women$lower = binom.exact$lower
  data_women$upper = binom.exact$upper
  data_women$orientation = "Women"

  data_df <- rbind(data_msm, data_msw, data_women)

  p <- ggplot(summary_stats, aes(x = variable)) +
    geom_ribbon(aes(ymin = q2.5, ymax = q97.5, fill = scenario), alpha = 0.1) +
    geom_line(aes(y = median, color = scenario)) +
    labs(x = "total # of partners", y = "Proportion of partners casual", title = "") +
    theme_light() + geom_point(data = data_df, (aes(x = partners, y = prop))) +
    geom_linerange(data = data_df, aes(x = partners, ymin = lower, ymax = upper)) +
    coord_cartesian(xlim = c(1,xmax), ylim = c(0, 1)) + facet_grid(cols = vars(orientation))

  p

}
