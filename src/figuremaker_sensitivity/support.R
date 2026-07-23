figplot <- function(df, scaler,outputstring,  ymin, ymax, title, ylabel, colsorrows = "cols"){
  
  
  colvals = c("#1b9e77", "#7570b3", "#d95f02")
  
  df[[outputstring]] = scaler*df[[outputstring]]

  jitter_dodge_position <- position_jitterdodge(jitter.width = 0, dodge.width = 0.7)
  
  
  df_summary <- df %>%
    group_by(strategy, sexorientation, sensitivity, sensmarker, highorlow) %>%
    summarize(
      mean = median(!!sym(outputstring)),
    )

  df_summary_1 <- df_summary[df_summary$sensmarker == "main", ]
  df_summary_2 <- df_summary[df_summary$sensmarker != "main", ]
  df_summary_2$meanmain  = rep(unique(df_summary_1$mean), each = 16)
  
  p_foi <- ggplot(df_summary_2, aes(x = sensitivity, ymin = mean, ymax = meanmain, color = highorlow, label = strategy)) +
    
    geom_linerange(size = 2) + 
    # geom_boxplot(aes(
    #   ymin = ymin, lower = lower, middle = middle, upper = upper, ymax = ymax
    # ), stat = "identity", outlier.shape = NA) +
    coord_flip(ylim = c(ymin, ymax)) +
  #  scale_color_manual(values = colvals) +
    theme_light() +
    labs(title = "",
         x = "",
         y = ylabel,
         color = "Label") +
    theme(
      text = element_text(size = 14),  
      plot.title = element_text(face = "bold", size = 16),  
      axis.title = element_text(face = "bold", size = 11),  
      axis.text = element_text(size = 12),  
      legend.title = element_blank(),  
      legend.text = element_text(size = 10),
      legend.position = "bottom"
    ) 
  
  if (colsorrows == "cols"){
    p_foi <- p_foi + facet_grid(cols = vars(sexorientation))
  } else if (colsorrows == "rows"){
    p_foi <- p_foi + facet_grid(rows = vars(sexorientation), switch = "both")
    
  }
  
  p_foi
}
