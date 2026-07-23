sample_values_df_lognormal <- function(n_times, min_values, max_values,mean_values) {
  
  sampled_df <- data.frame(matrix(nrow = n_times, ncol = length(min_values)))  # Create an empty dataframe

  # Loop through each pair of min and max values
  for (i in seq_along(min_values)) {
    # Sample n_times values from the range between min and max
    
    if (max_values[i] > 0){
      s <- (log(max_values[i]) - log(min_values[i]))/(2*1.96)
      sampled_df[, i] <- rlnorm(n_times, meanlog = log(mean_values[i]) - (s^2/2), sdlog = s)
    } else if (mean_values[i] == 0){
      sampled_df[, i] <- 0
    } else {
      sampled_df[, i] <- rlnorm(n_times, meanlog = (log(mean_values[i]) + log(mean_values[i])) / 2, sdlog = (log(mean_values[i]) - log(mean_values[i]))/(2*1.96))
    }
  }
  
  return(sampled_df)
}



sample_values_df_normal <- function(n_times, min_values, max_values,mean_values) {
  
  sampled_df <- data.frame(matrix(nrow = n_times, ncol = length(min_values)))  # Create an empty dataframe
  
  # Loop through each pair of min and max values
  for (i in seq_along(min_values)) {
    # Sample n_times values from the range between min and max
    
    if (max_values[i] > 0){
      s <- ((max_values[i]) - (min_values[i]))/(2*1.96)
      sampled_df[, i] <- rnorm(n_times, mean = mean_values[i], sd = s)
    } else if (mean_values[i] == 0){
      sampled_df[, i] <- 0
    } else {
      sampled_df[, i] < mean_values[i]
    }
  }
  
  return(sampled_df)
}


makelong <- function(df_sample, df_main, npar){
  
  df_long <- df_sample |>
    pivot_longer(
      cols = starts_with("X"),  # Columns to pivot
      names_to = "subgroup",            # New column to store names
      values_to = "value"                # New column to store values
    )
  
  df_long <- cbind(df_main[rep(1:nrow(df_main), npar), 1:5], df_long[,2])
  df_long$paramnum = rep(1:npar, each = nrow(df_main))
  df_long
}