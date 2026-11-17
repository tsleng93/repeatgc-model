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


samplevalues_degdist <- function(degdist){
  
  sampled_degdist <- list()
  
  
  for (i in 1:nrow(degdist)){
    
    #no history non user
    if (degdist$nohistorynonusermax[i] == 0){
      tempvals_nohistorynonuser = rep(0, npar)
    } else{
      tempvals_nohistorynonuser =  sample_values_df_normal(n_times = npar, 
                                                           min_values = degdist$nohistorynonusermin[i], 
                                                           max_values = degdist$nohistorynonusermax[i], 
                                                           mean_values= degdist$nohistorynonuser[i])
      tempvals_nohistorynonuser[tempvals_nohistorynonuser < 0] = 0
      
    }
    
    #no history PrEP user
    if (degdist$nohistoryPrEPusermax[i] == 0){
      tempvals_nohistoryPrEPuser = rep(0, npar)
    } else{
      tempvals_nohistoryPrEPuser =  sample_values_df_normal(n_times = npar, 
                                                            min_values = degdist$nohistoryPrEPusermin[i], 
                                                            max_values = degdist$nohistoryPrEPusermax[i], 
                                                            mean_values= degdist$nohistoryPrEPuser[i])
      tempvals_nohistoryPrEPuser[tempvals_nohistoryPrEPuser < 0] = 0
      
    }
    
    #no history PLHIV
    if (degdist$nohistoryHIVmax[i] == 0){
      tempvals_nohistoryHIV = rep(0, npar)
    } else{
      tempvals_nohistoryHIV =  sample_values_df_normal(n_times = npar, 
                                                       min_values = degdist$nohistoryHIVmin[i], 
                                                       max_values = degdist$nohistoryHIVmax[i], 
                                                       mean_values= degdist$nohistoryHIV[i])
      tempvals_nohistoryHIV[tempvals_nohistoryHIV < 0] = 0
      
    }
    
    #yes history non user
    if (degdist$nohistorynonusermax[i] == 0){
      tempvals_yeshistorynonuser = rep(0, npar)
    } else{
      tempvals_yeshistorynonuser =  sample_values_df_normal(n_times = npar, 
                                                            min_values = degdist$yeshistorynonusermin[i], 
                                                            max_values = degdist$yeshistorynonusermax[i], 
                                                            mean_values= degdist$yeshistorynonuser[i])
      tempvals_yeshistorynonuser[tempvals_nohistorynonuser < 0] = 0
      
    }
    
    #yes history PrEP user
    if (degdist$yeshistoryPrEPusermax[i] == 0){
      tempvals_yeshistoryPrEPuser = rep(0, npar)
    } else{
      tempvals_yeshistoryPrEPuser =  sample_values_df_normal(n_times = npar, 
                                                             min_values = degdist$yeshistoryPrEPusermin[i], 
                                                             max_values = degdist$yeshistoryPrEPusermax[i], 
                                                             mean_values= degdist$yeshistoryPrEPuser[i])
      tempvals_yeshistoryPrEPuser[tempvals_yeshistoryPrEPuser < 0] = 0
      
    }
    
    #yes history PLHIV
    if (degdist$yeshistoryHIVmax[i] == 0){
      tempvals_yeshistoryHIV = rep(0, npar)
    } else{
      tempvals_yeshistoryHIV =  sample_values_df_normal(n_times = npar, 
                                                        min_values = degdist$yeshistoryHIVmin[i], 
                                                        max_values = degdist$yeshistoryHIVmax[i], 
                                                        mean_values= degdist$yeshistoryHIV[i])
      tempvals_yeshistoryHIV[tempvals_yeshistoryHIV < 0] = 0
      
    } 
    
    
    sampled_degdist[[i]] = data.frame(paramnum = 1:npar, 
                                          numpartners = rep(degdist$partnernum[i], npar), 
                                          nohistorynonuser_N = unname(tempvals_nohistorynonuser),
                                          nohistoryPrEPuser_N = unname(tempvals_nohistoryPrEPuser),
                                          nohistoryHIV_N = unname(tempvals_nohistoryHIV),
                                          yeshistorynonuser_N = unname(tempvals_yeshistorynonuser),
                                          yeshistoryPrEPuser_N = unname(tempvals_yeshistoryPrEPuser),
                                          yeshistoryHIV_N = unname(tempvals_yeshistoryHIV))
    
  }
  
  
  sampled_degdist = bind_rows(sampled_degdist)
  
  sampled_degdist$nohistorynonuser_prob = 0
  sampled_degdist$nohistoryPrEPuser_prob = 0
  sampled_degdist$nohistoryHIV_prob = 0
  sampled_degdist$yeshistorynonuser_prob = 0
  sampled_degdist$yeshistoryPrEPuser_prob = 0
  sampled_degdist$yeshistoryHIV_prob = 0
  
  for (i in 1:npar){
    
    tempN = sampled_degdist$nohistorynonuser_N[sampled_degdist$paramnum == i] 
    sampled_degdist$nohistorynonuser_prob[sampled_degdist$paramnum == i] = tempN/sum(tempN)
    
    tempN = sampled_degdist$nohistoryPrEPuser_N[sampled_degdist$paramnum == i] 
    sampled_degdist$nohistoryPrEPuser_prob[sampled_degdist$paramnum == i] = tempN/sum(tempN)
    
    tempN = sampled_degdist$nohistoryHIV_N[sampled_degdist$paramnum == i] 
    sampled_degdist$nohistoryHIV_prob[sampled_degdist$paramnum == i] = tempN/sum(tempN)
    
    tempN = sampled_degdist$yeshistorynonuser_N[sampled_degdist$paramnum == i] 
    sampled_degdist$yeshistorynonuser_prob[sampled_degdist$paramnum == i] = tempN/sum(tempN)
    
    tempN = sampled_degdist$yeshistoryPrEPuser_N[sampled_degdist$paramnum == i] 
    sampled_degdist$yeshistoryPrEPuser_prob[sampled_degdist$paramnum == i] = tempN/sum(tempN)
    
    tempN = sampled_degdist$yeshistoryHIV_N[sampled_degdist$paramnum == i] 
    sampled_degdist$yeshistoryHIV_prob[sampled_degdist$paramnum == i] = tempN/sum(tempN)
    
  }
  
  sampled_degdist
  
}
