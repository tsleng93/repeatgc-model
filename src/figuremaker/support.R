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
    } else if (textlabels == "numberneededtotreat2yr" ){
      df_summary$label = paste0(round(df_summary2$middle2,0))
    }else if (textlabels == "numberneededtotreat_QALY"){
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
    } else if(outputstring == "numberneededtotreat2yr"){
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
      
    }  else if (outputstring == "caseaverted"){
      p_foi <- p_foi + geom_text(aes(y = ymax+80, label = label))
      
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

barplot_vertical <- function(df, scaler,outputstring,  ymin, ymax, title, ylabel, colsorrows = "cols", textlabels = NULL){
  
  colvals = df$colval[df$sexorientation == "MSM"]
  fillvals = df$fillval[df$sexorientation == "MSM"]
  
  
  df[[outputstring]] = scaler*df[[outputstring]]
  
  df$label = paste0(round(df[[outputstring]],0), "%")
  

  
  p_foi <- ggplot(df, aes(x = strategy, y = .data[[outputstring]], fill = strategy, color = strategy)) +
    geom_col() +
    coord_cartesian(ylim = c(ymin, ymax)) +
    scale_fill_manual(values = colvals) +
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
      color = guide_legend(nrow = 3, override.aes = list(fill = unique(colvals), label = "")),
      fill = "none"
    )
  
  
  if (is.null(textlabels) == FALSE){
    
    if (outputstring == "numberneededtotreat"){
      
    } else{
      p_foi <- p_foi + geom_text(aes(y = .data[[outputstring]]+5, label = label))
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

create_incremental_table <- function(data, sex, frontier, notonfrontier) {
  
  # Helper function to format mean and 95% UI
  format_ui <- function(x, digits = 2) {
    sprintf(
      paste0("%.", digits, "f (95%% UI %.", digits, "f–%.", digits, "f)"),
      mean(x, na.rm = TRUE),
      quantile(x, 0.025, na.rm = TRUE),
      quantile(x, 0.975, na.rm = TRUE)
    )
  }
  
  # Initialise tables
  frontier_table <- data.frame(
    strategy = frontier,
    impact = NA_character_,
    vacc = NA_character_,
    NNV = NA_character_
  )
  
  notfrontier_table <- data.frame(
    strategy = notonfrontier,
    impact = NA_character_,
    vacc = NA_character_,
    NNV = NA_character_
  )
  
  # Filter data by sex
  sex_data <- data |>
    filter(sexorientation == sex)
  
  # Calculate frontier strategies
  for (i in seq_along(frontier)) {
    
    current <- sex_data |>
      filter(strategy == frontier[i])
    
    if (i == 1) {
      
      vaccnum <- current$vaccinatednum
      QALYsgained <- current$QALYsgained
      
    } else {
      
      previous <- sex_data |>
        filter(strategy == frontier[i - 1])
      
      vaccnum <- current$vaccinatednum - previous$vaccinatednum
      QALYsgained <- current$QALYsgained - previous$QALYsgained
    }
    
    NNV <- vaccnum / QALYsgained
    
    frontier_table$vacc[i] <- format_ui(vaccnum, 0)
    frontier_table$impact[i] <- format_ui(QALYsgained, 2)
    frontier_table$NNV[i] <- format_ui(NNV, 0)
  }
  
  # Calculate non-frontier strategies
  for (i in seq_along(notonfrontier)) {
    
    current <- sex_data |>
      filter(strategy == notonfrontier[i])
    
    vaccnum <- current$vaccinatednum
    QALYsgained <- current$QALYsgained
    
    notfrontier_table$vacc[i] <- format_ui(vaccnum, 0)
    notfrontier_table$impact[i] <- format_ui(QALYsgained, 2)
    notfrontier_table$NNV[i] <- "dominance"
  }
  
  # Combine tables
  incremental_table <- rbind(
    frontier_table,
    notfrontier_table
  )
  
  return(incremental_table)
}

prob_compare <- function(df, strategy){
  dftemp <- df[df$sexorientation != "MSW", ]
  dftemp <- dftemp[dftemp$strategy == strategy, ]
  
  
  prob_impact =0
  prob_efficiency = 0
  for (i in 1:max(df$paramnum)){
    dftemp2 <- dftemp[dftemp$paramnum == i, ]
    if (dftemp2[dftemp2$sexorientation == "Women",]$QALY > dftemp2[dftemp2$sexorientation == "MSM",]$QALY){
      prob_impact = prob_impact+1
    }
    if (dftemp2[dftemp2$sexorientation == "Women",]$numberneededtotreat_QALY < dftemp2[dftemp2$sexorientation == "MSM",]$numberneededtotreat_QALY){
      prob_efficiency = prob_efficiency+1
    }
  }
  
  prob_impact = prob_impact/max(df$paramnum)
  prob_efficiency = prob_efficiency/max(df$paramnum)
  
  c(prob_impact, prob_efficiency)
}

prob_compare(df_strategy, "to all")


prob_compare2 <- function(df, strategy){
  dftemp <- df[df$sexorientation != "MSM", ]
  dftemp <- dftemp[dftemp$strategy == strategy, ]
  
  
  prob_impact =0
  prob_efficiency = 0
  for (i in 1:max(df$paramnum)){
    dftemp2 <- dftemp[dftemp$paramnum == i, ]
    if (dftemp2[dftemp2$sexorientation == "Women",]$QALY > dftemp2[dftemp2$sexorientation == "MSW",]$QALY){
      prob_impact = prob_impact+1
    }
    if (dftemp2[dftemp2$sexorientation == "Women",]$numberneededtotreat_QALY < dftemp2[dftemp2$sexorientation == "MSW",]$numberneededtotreat_QALY){
      prob_efficiency = prob_efficiency+1
    }
  }
  
  prob_impact = prob_impact/max(df$paramnum)
  prob_efficiency = prob_efficiency/max(df$paramnum)
  
  c(prob_impact, prob_efficiency)
}

prob_optimal <- function(df, orientation, strategy){
  dftemp <- df[df$sexorientation == orientation, ]
  dftemp <- dftemp[dftemp$strategy != "No vaccination", ]


  prob =0
  for (i in 1:max(df$paramnum)){
    dftemp2 <- dftemp[dftemp$paramnum == i, ]
    if (dftemp2[dftemp2$strategy == strategy,]$numberneededtotreat_QALY == min(dftemp2$numberneededtotreat_QALY)){
      prob = prob+1
    }
  }
  prob = prob/max(df$paramnum)
}

onerun_efficiencyfrontier = function(df_strategy, sexorientation, paramnum){
  
  dftemp <- df_strategy[df_strategy$sexorientation == sexorientation & df_strategy$paramnum == paramnum, ]
  dftemp <- dftemp[dftemp$strategy != "No vaccination",]
  
  strategies <- levels(dftemp$strategy)
  frontier_counts <- setNames(
    integer(length(strategies)),
    strategies
  )
  optimal_counts <- setNames(
    integer(length(strategies)),
    strategies
  )  

counter = 1
  
while(nrow(dftemp) > 1){

if (counter > 1){
  dftemp$vaccinatednum = dftemp$vaccinatednum - dftemp$vaccinatednum[dftemp$strategy == optimalstrat]
  dftemp$QALYsgained = dftemp$QALYsgained - dftemp$QALYsgained[dftemp$strategy == optimalstrat]
  dftemp$numberneededtotreat_QALY = dftemp$vaccinatednum/dftemp$QALYsgained
  
  dftemp <- dftemp |> filter(vaccinatednum > 0) |> filter(QALYsgained > 0)
  
}

optimalstrat = dftemp$strategy[which.min(dftemp$numberneededtotreat_QALY)]
frontier_counts[optimalstrat] = frontier_counts[optimalstrat] + 1
if (counter == 1){
  optimal_counts[optimalstrat] = optimal_counts[optimalstrat] + 1
}

counter = counter+1
}

return(list(optimal_counts = optimal_counts, frontier_counts = frontier_counts))
}


manyruns_efficiencyfrontier <- function(df_strategy, sexorientation){

strategies <- levels(df_strategy$strategy)

frontier_results <- matrix(
  0,
  nrow = numpar,
  ncol = length(strategies),
  dimnames = list(NULL, strategies)
)

optimal_results <- matrix(
  0,
  nrow = numpar,
  ncol = length(strategies),
  dimnames = list(NULL, strategies)
)


for (i in 1:numpar){
  if (i %% 1000 == 0) print(i)
  results <- onerun_efficiencyfrontier(
    df_strategy = df_strategy,
    sexorientation = sexorientation,
    paramnum = i
  )
  
  frontier_results[i, names(results$frontier_counts)] <- results$frontier_counts
  
  optimal_results[i, names(results$optimal_counts)] <- results$optimal_counts
  
  
}

frontier_probabilities <- colMeans(frontier_results)

optimal_probabilities <- colMeans(optimal_results)

df_probs <- data.frame(
  strategy = strategies,
  P_frontier = colMeans(frontier_results),
  P_optimal = colMeans(optimal_results),
  sexorientation = sexorientation
)

}

plot_convergence <- function(data, sex, subsample_sizes, ylim = c(0,12)) {
  
  # Full sample and subsamples
  datasets <- list()
  
  param_values <- sort(unique(data$paramnum))
  
  for (n in subsample_sizes) {
    
    selected_params <- param_values[
      unique(round(seq(1, length(param_values), length.out = n)))
    ]
    
    datasets[[paste0("n = ", n)]] <-
      data |>
      filter(paramnum %in% selected_params)
  }
  
  datasets[["Full sample"]] <- data
  
  # Calculate median and 95% quantile intervals
  summary_data <- imap_dfr(datasets, function(df, sample_name) {
    
    df |>
      filter(sexorientation == sex) |>
      group_by(strategy) |>
      summarise(
        median = quantile(QALYsgained, 0.5, na.rm = TRUE),
        lower = quantile(QALYsgained, 0.025, na.rm = TRUE),
        upper = quantile(QALYsgained, 0.975, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(sample = sample_name)
  })
  
  summary_data <- summary_data |>
    mutate(sample = forcats::fct_relevel(sample, "Full sample", after = Inf))
  
  # Plot
  ggplot(summary_data,
         aes(x = strategy, y = median,
             colour = sample, group = sample)) +
    
    geom_linerange(
      aes(ymin = lower, ymax = upper),
      position = position_dodge(width = 0.6),
      linewidth = 0.8
    ) +
    
    geom_point(
      position = position_dodge(width = 0.6),
      size = 2.5
    ) +
    
    coord_flip(ylim = ylim) +
    
    labs(
      x = NULL,
      y = "QALYs gained",
      colour = "Sample",
      title = paste("", sex)
    ) +
    
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text = element_text(colour = "black")
    )
}