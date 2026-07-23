FOIcomparisonplot <- function(df, subgroup, subgrouplabel, ymin, ymax, middle, upper, lower, ylab, palette){
  
  jitter_dodge_position <- position_jitterdodge(jitter.width = 0, dodge.width = 0.7)
  
  
  p <- ggplot(data = df[df$Gender.Sexual.orientation.group == subgroup, ], aes_string( x = "combined", y = middle, ymin = lower, ymax = upper, color = "type")) + geom_point(size = 3, position = jitter_dodge_position) + geom_linerange(position = jitter_dodge_position) + coord_flip(ylim = c(ymin, ymax)) +
    theme_light() + scale_color_manual(values = palette) +
    labs(title = paste(subgrouplabel, "", sep = ""),
         x = "",
         y = ylab,
         color = " ") +
    theme(
      text = element_text(size = 14),  
      plot.title = element_text(face = "bold", size = 16),  
      axis.title = element_text(face = "italic", size = 14),  
      axis.text = element_text(size = 12),  
      legend.title = element_blank(),  
      legend.text = element_text(size = 12),
      legend.position = "bottom"
    ) 
  
  p
} 


prevplot <- function(df, subgroup, subgrouplabel, ymin, ymax, ylabel, outputstring){
  
  
  df <- df[c("sex_orientation", "gc_history", "HIV_prepuse", "ethnicity", outputstring)]
  df$combined = paste0( df$HIV_prepuse, ", ", df$gc_history, ", ", df$ethnicity)
  

  df_summary <- df %>%
    group_by(combined, sex_orientation) %>%
    summarize(
      ymin = quantile(!!sym(outputstring), 0.025),
      lower = quantile(!!sym(outputstring), 0.25),
      middle = median(!!sym(outputstring)),
      upper = quantile(!!sym(outputstring), 0.75),
      ymax = quantile(!!sym(outputstring), 0.975)
    )
  
  jitter_dodge_position <- position_jitterdodge(jitter.width = 0, dodge.width = 0.7)
  
  
  p <- ggplot(data = df_summary[df_summary$sex_orientation == subgroup, ], aes( x = combined, y = middle, ymin = lower, ymax = upper)) + geom_point(size = 3) + geom_linerange() + coord_flip(ylim = c(ymin, ymax)) +
    theme_light() + 
    labs(title = paste(subgrouplabel, "", sep = ""),
         x = "",
         y = ylabel,
         color = "Label") +
    theme(
      text = element_text(size = 14),  
      plot.title = element_text(face = "bold", size = 16),  
      axis.title = element_text(face = "italic", size = 14),  
      axis.text = element_text(size = 12),  
      legend.title = element_blank(),  
      legend.text = element_text(size = 12),
      legend.position = "bottom"
    )
  
  p
} 


figplot_vertical <- function(df, scaler,outputstring,  ymin, ymax, title, ylabel, colsorrows = "cols", textlabels = NULL){
  
  colvals = df$colval[df$sexorientation == "MSM"]
  fillvals = df$fillval[df$sexorientation == "MSM"]
  
  
  df[[outputstring]] = scaler*df[[outputstring]]
  
  df_summary <- df %>%
    group_by(strategy, sexorientation) %>%
    summarize(
      ymin = quantile(!!sym(outputstring), 0.025),
      lower = quantile(!!sym(outputstring), 0.25),
      middle = median(!!sym(outputstring)),
      upper = quantile(!!sym(outputstring), 0.75),
      ymax = quantile(!!sym(outputstring), 0.975)
    )
  
  if (is.null(textlabels) == FALSE){
    
    df_summary2 <- df %>%
      group_by(strategy, sexorientation) %>%
      summarize(
        middle2 = median(!!sym(textlabels))
      )
    
    if (textlabels == "numberneededtotreat" ){
      df_summary$label = paste0(round(df_summary2$middle2,0))
    } else if (textlabels == "numberneededtotreat_QALY"){
      df_summary$label = paste0(round(df_summary2$middle2,1), "k")
    } else if (textlabels == "FOI"){
      df_summary$label = paste0(round(df_summary2$middle2,0))
    } else if (textlabels == "vaccinevalue"){
      df_summary$label = paste0(round(df_summary2$middle2,0))
    } else if (textlabels == "vaccinated"){
      df_summary$label = paste0(round(df_summary2$middle2,0), "%")
    }  else if (textlabels == "costssaved"){
      df_summary$label = paste0(round(df_summary2$middle2,0), "k")
    }  else if (textlabels == "QALYsgained"){
        df_summary$label = paste0(round(df_summary2$middle2,1))
    } else{
      df_summary$label = paste0(round(df_summary2$middle2,0), "%")
    }
    

  } else{
    df_summary$label = 0
  }
  

  p_foi <- ggplot(df_summary, aes(x = strategy, fill = strategy, color = strategy)) +
    geom_boxplot(aes(
      ymin = ymin, lower = lower, middle = middle, upper = upper, ymax = ymax
    ), stat = "identity", outlier.shape = NA) +
    coord_cartesian(ylim = c(ymin, ymax)) +
    scale_fill_manual(values = fillvals) +
    scale_color_manual(values = colvals) +
    theme_light() +   
    labs(title = title,
         x = "",
         y = ylabel,
         color = "Label") +
    theme(
      text = element_text(size = 14),  
      plot.title = element_text(face = "bold", size = 16),  
      axis.title = element_text(face = "bold", size = 14),  
      axis.text.y = element_text(size = 12),
      axis.text.x = element_blank(),  
      legend.title = element_blank(),  
      legend.text = element_text(size = 10),
      legend.position = "bottom"
    ) + guides(
      color = guide_legend(nrow = 3, override.aes = list(fill = unique(fillvals), label = "")),
      fill = "none"
    )
  
  
  if (is.null(textlabels) == FALSE){
    
    if (outputstring == "numberneededtotreat"){
      p_foi <- p_foi + geom_text(aes(y = ymax+2, label = label))
    } else if (outputstring == "numberneededtotreat_QALY"){
      p_foi <- p_foi + geom_text(aes(y = ymax+2, label = label))
    } else if (outputstring == "vaccinatednum"){
      p_foi <- p_foi + geom_text(aes(y = ymax+180, label = label))
      
    } else if (outputstring == "vaccinated"){
      p_foi <- p_foi + geom_text(aes(y = ymax+30, label = label))
      
    } else if (outputstring == "efficacy"){
      p_foi <- p_foi + geom_text(aes(y = ymax+2, label = label))
      
    }  else if (outputstring == "QALYsgained"){
      p_foi <- p_foi + geom_text(aes(y = ymax+0.4, label = label))
      
    }  else if (outputstring == "impactnum"){
      p_foi <- p_foi + geom_text(aes(y = ymax+5, label = label))
      
    } else{
      p_foi <- p_foi + geom_text(aes(y = ymax+10, label = label))
    }
    
  }
  
  
  if (colsorrows == "cols"){
    p_foi <- p_foi + facet_grid(cols = vars(sexorientation))
  } else if (colsorrows == "rows"){
    p_foi <- p_foi + facet_grid(rows = vars(sexorientation), switch = "both")
    
  }
  
  p_foi
}




figplot_vertical_nntQALY <- function(df, scaler,outputstring,  ymin, ymax, title, ylabel, colsorrows = "cols", textlabels = NULL){
  #figplot_vertical sometimes renders 'k's in png as % - workaround
  colvals = df$colval[df$sexorientation == "MSM"]
  fillvals = df$fillval[df$sexorientation == "MSM"]
  
  
  df[[outputstring]] = scaler*df[[outputstring]]
  
  df_summary <- df %>%
    group_by(strategy, sexorientation) %>%
    summarize(
      ymin = quantile(!!sym(outputstring), 0.025),
      lower = quantile(!!sym(outputstring), 0.25),
      middle = median(!!sym(outputstring)),
      upper = quantile(!!sym(outputstring), 0.75),
      ymax = quantile(!!sym(outputstring), 0.975)
    )
    
  if (!is.null(textlabels)) {
    
    df_summary2 <- df %>%
      group_by(strategy, sexorientation) %>%
      summarize(
        middle2 = median(!!sym(textlabels)),
        .groups = "drop"
      )
    
    df_summary$label <- sprintf("%.1fk", as.numeric(df_summary2$middle2))

  }

  
  p_foi <- ggplot(df_summary, aes(x = strategy, fill = strategy, color = strategy)) +
    geom_boxplot(aes(
      ymin = ymin, lower = lower, middle = middle, upper = upper, ymax = ymax
    ), stat = "identity", outlier.shape = NA) +
    coord_cartesian(ylim = c(ymin, ymax)) +
    scale_fill_manual(values = fillvals) +
    scale_color_manual(values = colvals) +
    theme_light() +   
    labs(title = title,
         x = "",
         y = ylabel,
         color = "Label") +
    theme(
      text = element_text(size = 14),  
      plot.title = element_text(face = "bold", size = 16),  
      axis.title = element_text(face = "bold", size = 14),  
      axis.text.y = element_text(size = 12),
      axis.text.x = element_blank(),  
      legend.title = element_blank(),  
      legend.text = element_text(size = 10),
      legend.position = "bottom"
    ) + guides(
      color = guide_legend(nrow = 3, override.aes = list(fill = unique(fillvals), label = "")),
      fill = "none"
    )
  
  
  if (is.null(textlabels) == FALSE){
      p_foi <- p_foi + geom_text(aes(y = ymax+2, label = label))
  }
  
  if (colsorrows == "cols"){
    p_foi <- p_foi + facet_grid(cols = vars(sexorientation))
  } else if (colsorrows == "rows"){
    p_foi <- p_foi + facet_grid(rows = vars(sexorientation), switch = "both")
    
  }
  
  p_foi
}


figplot_vertical_vaccinatednum <- function(df, scaler,outputstring,  ymin, ymax, title, ylabel, colsorrows = "cols", textlabels = NULL){
  #figplot_vertical sometimes renders 'k's in png as % - workaround
  
  colvals = df$colval[df$sexorientation == "MSM"]
  fillvals = df$fillval[df$sexorientation == "MSM"]
  
  
  df[[outputstring]] = scaler*df[[outputstring]]
  
  
  df_summary <- df %>%
    group_by(strategy, sexorientation) %>%
    summarize(
      ymin = quantile(!!sym(outputstring), 0.025),
      lower = quantile(!!sym(outputstring), 0.25),
      middle = median(!!sym(outputstring)),
      upper = quantile(!!sym(outputstring), 0.75),
      ymax = quantile(!!sym(outputstring), 0.975)
    )
  
  if (is.null(textlabels) == FALSE){
    
    df_summary2 <- df %>%
      group_by(strategy, sexorientation) %>%
      summarize(
        middle2 = median(!!sym(textlabels))
      )

      df_summary$label = paste0(round(df_summary2$middle2,0), "%")
    
  } else{
    df_summary$label = 0
  }

  
  p_foi <- ggplot(df_summary, aes(x = strategy, fill = strategy, color = strategy)) +
    geom_boxplot(aes(
      ymin = ymin, lower = lower, middle = middle, upper = upper, ymax = ymax
    ), stat = "identity", outlier.shape = NA) +
    coord_cartesian(ylim = c(ymin, ymax)) +
    scale_fill_manual(values = fillvals) +
    scale_color_manual(values = colvals) +
    theme_light() +   
    labs(title = title,
         x = "",
         y = ylabel,
         color = "Label") +
    theme(
      text = element_text(size = 14),  
      plot.title = element_text(face = "bold", size = 16),  
      axis.title = element_text(face = "bold", size = 14),  
      axis.text.y = element_text(size = 12),
      axis.text.x = element_blank(),  
      legend.title = element_blank(),  
      legend.text = element_text(size = 10),
      legend.position = "bottom"
    ) + guides(
      color = guide_legend(nrow = 3, override.aes = list(fill = unique(fillvals), label = "")),
      fill = "none"
    )
  
  
  if (is.null(textlabels) == FALSE){

      p_foi <- p_foi + geom_text(aes(y = ymax+180, label = label))

  }
  
  
  if (colsorrows == "cols"){
    p_foi <- p_foi + facet_grid(cols = vars(sexorientation))
  } else if (colsorrows == "rows"){
    p_foi <- p_foi + facet_grid(rows = vars(sexorientation), switch = "both")
    
  }
  
  p_foi
}




efficiencyplot <- function(df, sexorientation, title, xlab, ylab, xmin, xmax, palette){

dftemp <- df[df$sexorientation == sexorientation, ]


wide_eff <- data.frame(novaccination = 0*(1:max(df$paramnum)), offer_hivstatus = dftemp$QALYsgained[dftemp$strategy == "2. by HIV status"], offer_prepuse = dftemp$QALYsgained[dftemp$strategy == "3. by PrEP use"], offer_history = dftemp$QALYsgained[dftemp$strategy == "4. by history"], offer_hivstatus_prepuse = dftemp$QALYsgained[dftemp$strategy == "5. by HIV status + PrEP use"], offer_history_hivstatus = dftemp$QALYsgained[dftemp$strategy == "6. by history + HIV status"], offer_history_prepuse = dftemp$QALYsgained[dftemp$strategy == "7. by history + PrEP use"],  offer_history_hivstatus_prepuse = dftemp$QALYsgained[dftemp$strategy == "8. by history + HIV status + PrEP use"],   offer_all = dftemp$QALYsgained[dftemp$strategy == "9. to all"])
wide_eff <- as.matrix(wide_eff)

wide_costs <- data.frame(novaccination = 0*(1:max(df$paramnum)), offer_hivstatus = dftemp$vaccinatednum[dftemp$strategy == "2. by HIV status"], offer_prepuse = dftemp$vaccinatednum[dftemp$strategy == "3. by PrEP use"], offer_history = dftemp$vaccinatednum[dftemp$strategy == "4. by history"], offer_hivstatus_prepuse = dftemp$vaccinatednum[dftemp$strategy == "5. by HIV status + PrEP use"], offer_history_hivstatus = dftemp$vaccinatednum[dftemp$strategy == "6. by history + HIV status"], offer_history_prepuse = dftemp$vaccinatednum[dftemp$strategy == "7. by history + PrEP use"],  offer_history_hivstatus_prepuse = dftemp$vaccinatednum[dftemp$strategy == "8. by history + HIV status + PrEP use"],   offer_all = dftemp$vaccinatednum[dftemp$strategy == "9. to all"])
wide_costs <- as.matrix(wide_costs)


treats <- unique(dftemp$strategy)

bcea_temp <- bcea(wide_eff, wide_costs, ref = 9, interventions = treats, Kmax = 20000)
bceacols <- palette

p <- ceef.plot(bcea_temp, graph = "ggplot2", dominance = FALSE, title = "my title") + theme_light()  +  scale_color_manual(values = bceacols, labels = treats) +
  labs(title = title,
       x = xlab,
       y = ylab,
       color = "Strategy")+
  theme(
    text = element_text(size = 14),
    plot.title = element_text(face = "bold", size = 16),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    
    legend.position = "bottom"
  )

p <- p +  guides(colour = guide_legend(nrow = 3)) + coord_cartesian( xlim = c(xmin, xmax))

return(p)

}


efficiencyplot2 <- function(df, sexorientation, title, xlab, ylab, xmin, xmax, palette, frontierstrats, ymin, ymax){
  
  colvals = df$colval[df$sexorientation == "MSM"]
  fillvals = df$fillval[df$sexorientation == "MSM"]
  
  dftemp <- df[df$sexorientation == sexorientation, ]
  
  dftemp$frontier <- 0
  dftemp$frontier[dftemp$strategy %in% frontierstrats] <- 1
  
  bceacols <- palette
  
  mean_df <- dftemp %>%
    group_by(strategy) %>%
    summarise_at(vars(QALYsgained, impact, vaccinatednum, frontier), funs(mean(., na.rm=TRUE)))
  
  dftemp$QALYsgained <- dftemp$QALYsgained + runif(nrow(dftemp),min = 0, max = 1e-12)
  
  dftemp$vaccinatednum <- dftemp$vaccinatednum + runif(nrow(dftemp),min = 0, max = 1e-12)
  
  
  mean_df_frontier <- mean_df[mean_df$frontier == 1, ]
  mean_df_frontier$strategy = "Frontier"

  mean_df$frontier[mean_df$strategy == "1. No vaccination"] = 0 # by convention not on frontier
  
  shapevals <- c(4, 16)
  
  shapetemp <- mean_df$frontier
  
  
  
  shapetemp[shapetemp == 0] = 4
  shapetemp[shapetemp == 1] = 16

  p <- ggplot(data  = dftemp, aes(x = QALYsgained, y = vaccinatednum, colour = strategy, fill = strategy, shape = as.factor(frontier))) + 
    geom_point(data  = mean_df, aes(x = QALYsgained, y=vaccinatednum, colour = strategy, fill = strategy), size = 3) +
    geom_line(data  = mean_df_frontier, aes(x = QALYsgained, y=vaccinatednum), colour = "darkgrey") + 
    scale_fill_manual(values = fillvals) +
    scale_color_manual(values = colvals) +
    scale_shape_manual(values = shapevals) + 
    guides(shape = "none") + 
    theme_light()  +   
    labs(title = title,
         x = xlab,
         y = ylab,
         color = "Label") + 
    theme(
      text = element_text(size = 14),
      plot.title = element_text(face = "bold", size = 16),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 12),
      legend.title = element_blank(),
      legend.text = element_text(size = 8),
      
      legend.position = "right"
    )
  
  p <- p +  guides(
    color = guide_legend(nrow = 9, override.aes = list(fill = unique(fillvals), shape = shapetemp, label = "")),
    fill = "none"
  ) + coord_cartesian( xlim = c(xmin, xmax), ylim = c(ymin, ymax)) 
  
  return(p)
  
}
