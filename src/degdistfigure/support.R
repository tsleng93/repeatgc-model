plot_degdist_comparison <- function(
    degdist,
    sampled_degdist,
    modeloutput,
    datamean,
    datalower,
    dataupper,
    title
) {
  
  library(dplyr)
  library(ggplot2)
  
  # Summarise the 1000 model samples
  model_summary <- sampled_degdist %>%
    group_by(numpartners) %>%
    summarise(
      mean = mean(.data[[modeloutput]], na.rm = TRUE),
      lower = quantile(.data[[modeloutput]], 0.025, na.rm = TRUE),
      upper = quantile(.data[[modeloutput]], 0.975, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename(partnernum = numpartners) %>%
    mutate(type = "Model") %>%
    filter(!(mean == 0 & lower == 0 & upper == 0))
  
  # Prepare the original data estimates
  data_summary <- degdist %>%
    transmute(
      partnernum,
      mean = .data[[datamean]],
      lower = .data[[datalower]],
      upper = .data[[dataupper]],
      type = "Data"
    ) %>%
    filter(!(mean == 0 & lower == 0 & upper == 0))
  
  # Combine
  plot_df <- bind_rows(data_summary, model_summary)
  
  # Consistent dodge position
  dodge <- position_dodge(width = 0.4)
  
  # Plot
  p <- ggplot(
    plot_df,
    aes(
      x = partnernum,
      y = mean,
      ymin = lower,
      ymax = upper,
      colour = type,
      group = type
    )
  ) +
    geom_linerange(
      linewidth = 0.8,
      position = dodge
    ) +
    geom_point(
      size = 3,
      position = dodge
    ) +
    theme_light() +
    scale_color_manual(
      values = c("Data" = "black", "Model" = "steelblue")
    ) +
    labs(
      title = title,
      x = "# of partners",
      y = "# of individuals",
      colour = ""
    ) +
    theme(
      text = element_text(size = 14),
      plot.title = element_text(face = "bold", size = 12),
      axis.title = element_text(face = "italic", size = 12),
      axis.text = element_text(size = 12),
      legend.position = "bottom"
    ) + coord_cartesian(xlim = c(0, 20))
  
  return(p)
}


plot_cumdegdist <- function(
    sampled_degdist,
    modeloutput
) {
  
  library(dplyr)
  library(ggplot2)
  
  # Calculate cumulative degree distribution for each simulation
  sampled_cum <- sampled_degdist %>%
    group_by(paramnum) %>%
    arrange(numpartners, .by_group = TRUE) %>%
    mutate(
      cumulative = cumsum(.data[[modeloutput]])
    ) %>%
    ungroup()
  
  # Summarise across simulations
  model_cum_summary <- sampled_cum %>%
    group_by(numpartners) %>%
    summarise(
      mean = mean(cumulative, na.rm = TRUE),
      lower = quantile(cumulative, 0.025, na.rm = TRUE),
      upper = quantile(cumulative, 0.975, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Plot
  p <- ggplot(
    model_cum_summary,
    aes(
      x = numpartners,
      y = mean
    )
  ) +
    
    geom_ribbon(
      aes(
        ymin = lower,
        ymax = upper
      ),
      fill = "steelblue",
      alpha = 0.2
    ) +
    
    geom_line(
      colour = "steelblue",
      linewidth = 1
    ) +
    
    theme_light() +
    
    scale_y_continuous(
      limits = c(0, 1),
      labels = scales::percent
    ) +
    
    labs(
      title = "Cumulative degree distribution",
      x = "Number of partners",
      y = "Cumulative proportion of no-history non-users"
    ) +
    
    theme(
      text = element_text(size = 14),
      plot.title = element_text(face = "bold", size = 16),
      axis.title = element_text(face = "italic", size = 14),
      axis.text = element_text(size = 12),
      legend.position = "bottom"
    ) + coord_cartesian(xlim = c(0, 20))
  
  return(p)
}


plot_cumdegdist_combined <- function(
    sampled_degdist,
    modeloutputs,
    labels = modeloutputs,
    palette = NULL,
    title
) {
  
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  
  # Calculate cumulative distributions for each simulation and subgroup
  sampled_cum <- sampled_degdist %>%
    select(paramnum, numpartners, all_of(modeloutputs)) %>%
    pivot_longer(
      cols = all_of(modeloutputs),
      names_to = "subgroup",
      values_to = "prob"
    ) %>%
    group_by(paramnum, subgroup) %>%
    arrange(numpartners, .by_group = TRUE) %>%
    mutate(
      cumulative = cumsum(prob)
    ) %>%
    ungroup()
  
  # Summarise across simulations
  model_cum_summary <- sampled_cum %>%
    group_by(numpartners, subgroup) %>%
    summarise(
      mean = mean(cumulative, na.rm = TRUE),
      lower = quantile(cumulative, 0.025, na.rm = TRUE),
      upper = quantile(cumulative, 0.975, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      subgroup = factor(subgroup, levels = modeloutputs, labels = labels)
    )
  
  # Plot
  p <- ggplot(
    model_cum_summary,
    aes(
      x = numpartners,
      y = mean,
      colour = subgroup,
      fill = subgroup,
      group = subgroup
    )
  ) +
    
    geom_ribbon(
      aes(
        ymin = lower,
        ymax = upper
      ),
      alpha = 0.12,
      colour = NA
    ) +
    
    geom_line(
      linewidth = 1
    ) +
    
    guides(
      colour = guide_legend(nrow = 3, byrow = TRUE),
      fill = "none"
    ) +
    
    theme_light() +
    
    scale_y_continuous(
      limits = c(0, 1),
      labels = scales::percent
    ) +
    
    labs(
      title = title,
      x = "Number of partners",
      y = "Cumulative proportion",
      colour = "",
      fill = ""
    ) +
    
    theme(
      text = element_text(size = 14),
      plot.title = element_text(face = "bold", size = 16),
      axis.title = element_text(face = "italic", size = 14),
      axis.text = element_text(size = 12),
      legend.position = "bottom",
      legend.box = "horizontal"
    )
  
  # Optional manual colour palette
  if (!is.null(palette)) {
    p <- p +
      scale_colour_manual(values = palette) +
      scale_fill_manual(values = palette)
  }
  
  p <- p + coord_cartesian(xlim = c(0,20))
  
  return(p)
}