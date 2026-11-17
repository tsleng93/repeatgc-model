model <- function(prev, alpha, numpartners, phi, omega, sexactscasual, sexactsstable, probdiagstable, probdiagcasual, dur){
  
  
  ## 11 Oct Edit
  omega <- c(0, omega[numpartners])
  beta <- probdiagcasual*omega*(1 - (1-alpha)^sexactscasual) + probdiagstable*(1-omega)*(1 - (1-alpha)^sexactsstable)
  
  
  # edit - adding in 'true' infection rate
  ##
  beta_inf <- omega*(1 - (1-alpha)^sexactscasual) + (1-omega)*(1 - (1-alpha)^sexactsstable)
  
  temp <- 1 - (1-beta*prev)^numpartners
  temp_inf <- 1 - (1 - beta_inf*prev)^numpartners
  
  lambda_0 <- 4*temp
  lambda_0_inf <- 4*temp_inf
  
  Lifeyears = sum(phi[lambda_0 > 0]*(1-exp(-dur*lambda_0[lambda_0 > 0]))*(1/(lambda_0[lambda_0>0]))) + sum(dur*phi[lambda_0 == 0])
  FirstDiagnoses = sum(phi[lambda_0 > 0]*(1-exp(-dur*lambda_0[lambda_0 > 0])))
  
  if (Lifeyears > 0){
    lambda = FirstDiagnoses/Lifeyears}
  else{
    lambda = 0}
  
  
  Repeat_lambda <- sum(phi*lambda_0)
  Repeat_lambda_inf <- sum(phi*lambda_0_inf)
  
  return(c(FirstDiagnoses,Lifeyears, lambda, Repeat_lambda, Repeat_lambda_inf))
  
  
}


FindPrev_fun <- function (alpha, TargNX, TargLambda, numpartners, phi, omega, sexactscasual, sexactsstable, probdiagstable, probdiagcasual, dur, N){
  
  TargLY = TargNX/TargLambda
  
  helper <- function(prev){


    if ((prev < 0) || (prev > 1)){
      output = 1e6
    } else{


      temp = model(prev, alpha, numpartners, phi, omega, sexactscasual, sexactsstable, probdiagstable, probdiagcasual, dur)


      if (temp[3] > 1){
        output = 1e6
      } else{

        FirstDiagnoses = TargNX
        LifeYears = TargLY

        output <- -1* (FirstDiagnoses*log(temp[3]) + (LifeYears - FirstDiagnoses)*log(1 - temp[3]) )
      }
    }

  }

  f <- function(x) helper(x[1])
  st = c(prev = 0.005)
  
  #1 partner prevalence
  a <- optim(st, f, , method = "Nelder-Mead")
  pars = a$par
  
  return(c(pars,helper(pars))) 
  
  
}





strategy_df <- function(df, sex_orientation, palettefill, palettecol){
  
  
  dftemp = df[df$sex_orientation == sex_orientation,] 
  
  df_strategy = data.frame(paramnum = rep(1:max(df$paramnum), each = 9), strategy = 0, impact = 0, impactnum = 0, vaccinated = 0, vaccinatednum = 0, caseavertedperdose = 0, FOI = 0, numrepeat = 0, firstinfections = 0, lifeyears = 0, colval = rep(palettecol, max(df$paramnum)), fillval = rep(palettefill, max(df$paramnum)))
  
  
  counter = 1
  
  for (i in 1:max(df$paramnum)){

    vaccinated = c()
    unvaccinated = dftemp$paramnum == i
    
    df_strategy$paramnumber[counter] = i
    df_strategy$strategy[counter] = "1. No vaccination"
    df_strategy$QALYsgained[counter] =  (sum(dftemp$QALYlosswithoutvaccine[dftemp$paramnum == i])) -  ( (sum(dftemp$QALYlosswithvaccine[vaccinated])) + (sum(dftemp$QALYlosswithoutvaccine[unvaccinated])))
    df_strategy$vaccinated[counter] =   100*sum(dftemp$N[vaccinated])/(sum(dftemp$N[vaccinated]) + sum(dftemp$N[unvaccinated]))
    df_strategy$vaccinatednum[counter] =   sum(dftemp$N_scale[vaccinated]*dftemp$N[vaccinated])
    df_strategy$caseaverted[counter] = (sum(dftemp$Overallwithoutvaccine[dftemp$paramnum == i])) -  ( (sum(dftemp$Overallwithvaccine[vaccinated])) + (sum(dftemp$Overallwithoutvaccine[unvaccinated])))
    #df_strategy$caseavertedperdose[counter] = ((sum(dftemp$Overallwithoutvaccine[dftemp$paramnum == i])) -  ( (sum(dftemp$Overallwithvaccine[vaccinated])) + (sum(dftemp$Overallwithoutvaccine[unvaccinated])))) / df_strategy$vaccinatednum[counter]
    df_strategy$caseavertedperdose[counter] = -1000  # is NaN because 0/0, but setting -1 helps with plotting later
    df_strategy$FOI[counter] = (sum(dftemp$Firstinfectionswithvaccine[vaccinated]) + sum(dftemp$Firstinfectionswithoutvaccine[unvaccinated]))/( sum(dftemp$Lifeyearswithoutvaccine[unvaccinated]) + sum(dftemp$Lifeyearswithvaccine[vaccinated]))
    df_strategy$numrepeat[counter] = (sum(dftemp$withoutvaccine[dftemp$paramnum == i]))
    df_strategy$firstinfections[counter] = (sum( dftemp$N_scale[vaccinated]*dftemp$Firstinfectionswithvaccine[vaccinated]) + sum(dftemp$N_scale[unvaccinated]*dftemp$Firstinfectionswithoutvaccine[unvaccinated]))
    df_strategy$lifeyears[counter] =  sum(dftemp$N_scale[unvaccinated]*dftemp$Lifeyearswithoutvaccine[unvaccinated]) + sum(dftemp$N_scale[vaccinated]*dftemp$Lifeyearswithvaccine[vaccinated])
    
    
    firstinfectionswithoutvaccine = df_strategy$firstinfections[counter]
    lifeyearswithoutvaccine = df_strategy$lifeyears[counter]
    FOIwithoutvaccine = firstinfectionswithoutvaccine/lifeyearswithoutvaccine

    df_strategy$impactnum[counter] =  1000*(FOIwithoutvaccine - df_strategy$firstinfections[counter]/df_strategy$lifeyears[counter])
    df_strategy$impact[counter] = 100*(FOIwithoutvaccine - df_strategy$firstinfections[counter]/df_strategy$lifeyears[counter])/FOIwithoutvaccine
    
    #df_strategy$efficacy[counter] = 100 * (1 - sum(dftemp$withvaccine[vaccinated])/ sum(dftemp$withoutvaccine[vaccinated]))
    df_strategy$efficacy[counter] = -100  # is NaN because 0/0, but setting -1 helps with plotting later
    
    
    
    counter = counter+1
    
    vaccinated = dftemp$HIV_prepuse == "PLHIV" & dftemp$paramnum == i
    unvaccinated = dftemp$HIV_prepuse != "PLHIV" & dftemp$paramnum == i
    
    df_strategy$paramnumber[counter] = i
    df_strategy$strategy[counter] = "2. by HIV status"
    df_strategy$QALYsgained[counter] =  (sum(dftemp$QALYlosswithoutvaccine[dftemp$paramnum == i])) -  ( (sum(dftemp$QALYlosswithvaccine[vaccinated])) + (sum(dftemp$QALYlosswithoutvaccine[unvaccinated])))
    df_strategy$vaccinated[counter] =   100*sum(dftemp$N[vaccinated])/(sum(dftemp$N[vaccinated]) + sum(dftemp$N[unvaccinated]))
    df_strategy$vaccinatednum[counter] =   sum(dftemp$N_scale[vaccinated]*dftemp$N[vaccinated])
    df_strategy$caseaverted[counter] = (sum(dftemp$Overallwithoutvaccine[dftemp$paramnum == i])) -  ( (sum(dftemp$Overallwithvaccine[vaccinated])) + (sum(dftemp$Overallwithoutvaccine[unvaccinated])))
    df_strategy$caseavertedperdose[counter] = ((sum(dftemp$Overallwithoutvaccine[dftemp$paramnum == i])) -  ( (sum(dftemp$Overallwithvaccine[vaccinated])) + (sum(dftemp$Overallwithoutvaccine[unvaccinated])))) / df_strategy$vaccinatednum[counter]
    df_strategy$FOI[counter] = (sum(dftemp$Firstinfectionswithvaccine[vaccinated]) + sum(dftemp$Firstinfectionswithoutvaccine[unvaccinated]))/( sum(dftemp$Lifeyearswithoutvaccine[unvaccinated]) + sum(dftemp$Lifeyearswithvaccine[vaccinated]))
    df_strategy$efficacy[counter] = 100 * (1 - sum(dftemp$withvaccine[vaccinated])/ sum(dftemp$withoutvaccine[vaccinated]))
    df_strategy$numrepeat[counter] = (sum(dftemp$withoutvaccine[dftemp$paramnum == i]))
    df_strategy$firstinfections[counter] = (sum( dftemp$N_scale[vaccinated]*dftemp$Firstinfectionswithvaccine[vaccinated]) + sum(dftemp$N_scale[unvaccinated]*dftemp$Firstinfectionswithoutvaccine[unvaccinated]))
    df_strategy$lifeyears[counter] =  sum(dftemp$N_scale[unvaccinated]*dftemp$Lifeyearswithoutvaccine[unvaccinated]) + sum(dftemp$N_scale[vaccinated]*dftemp$Lifeyearswithvaccine[vaccinated])
    df_strategy$impactnum[counter] =  1000*(FOIwithoutvaccine - df_strategy$firstinfections[counter]/df_strategy$lifeyears[counter])
    df_strategy$impact[counter] = 100*(FOIwithoutvaccine - df_strategy$firstinfections[counter]/df_strategy$lifeyears[counter])/FOIwithoutvaccine
    
    counter = counter+1
    
    vaccinated = dftemp$HIV_prepuse == "HIV- PrEP user" & dftemp$paramnum == i
    unvaccinated = dftemp$HIV_prepuse != "HIV- PrEP user" & dftemp$paramnum == i
    
    df_strategy$paramnumber[counter] = i
    df_strategy$strategy[counter] = "3. by PrEP use"
    df_strategy$QALYsgained[counter] =  (sum(dftemp$QALYlosswithoutvaccine[dftemp$paramnum == i])) -  ( (sum(dftemp$QALYlosswithvaccine[vaccinated])) + (sum(dftemp$QALYlosswithoutvaccine[unvaccinated])))
    df_strategy$vaccinated[counter] =   100*sum(dftemp$N[vaccinated])/(sum(dftemp$N[vaccinated]) + sum(dftemp$N[unvaccinated]))
    df_strategy$vaccinatednum[counter] =   sum(dftemp$N_scale[vaccinated]*dftemp$N[vaccinated])
    df_strategy$caseaverted[counter] = (sum(dftemp$Overallwithoutvaccine[dftemp$paramnum == i])) -  ( (sum(dftemp$Overallwithvaccine[vaccinated])) + (sum(dftemp$Overallwithoutvaccine[unvaccinated])))
    df_strategy$caseavertedperdose[counter] = ((sum(dftemp$Overallwithoutvaccine[dftemp$paramnum == i])) -  ( (sum(dftemp$Overallwithvaccine[vaccinated])) + (sum(dftemp$Overallwithoutvaccine[unvaccinated])))) / df_strategy$vaccinatednum[counter]
    df_strategy$FOI[counter] = (sum(dftemp$Firstinfectionswithvaccine[vaccinated]) + sum(dftemp$Firstinfectionswithoutvaccine[unvaccinated]))/( sum(dftemp$Lifeyearswithoutvaccine[unvaccinated]) + sum(dftemp$Lifeyearswithvaccine[vaccinated]))
    df_strategy$efficacy[counter] = 100 * (1 - sum(dftemp$withvaccine[vaccinated])/ sum(dftemp$withoutvaccine[vaccinated]))
    df_strategy$numrepeat[counter] = (sum(dftemp$withoutvaccine[dftemp$paramnum == i]))
    df_strategy$firstinfections[counter] = (sum( dftemp$N_scale[vaccinated]*dftemp$Firstinfectionswithvaccine[vaccinated]) + sum(dftemp$N_scale[unvaccinated]*dftemp$Firstinfectionswithoutvaccine[unvaccinated]))
    df_strategy$lifeyears[counter] =  sum(dftemp$N_scale[unvaccinated]*dftemp$Lifeyearswithoutvaccine[unvaccinated]) + sum(dftemp$N_scale[vaccinated]*dftemp$Lifeyearswithvaccine[vaccinated])
    df_strategy$impactnum[counter] =  1000*(FOIwithoutvaccine - df_strategy$firstinfections[counter]/df_strategy$lifeyears[counter])
    df_strategy$impact[counter] = 100*(FOIwithoutvaccine - df_strategy$firstinfections[counter]/df_strategy$lifeyears[counter])/FOIwithoutvaccine
    
    counter = counter+1
    
    vaccinated = dftemp$gc_history == "with history" & dftemp$paramnum == i
    unvaccinated = dftemp$gc_history != "with history" & dftemp$paramnum == i
    
    df_strategy$paramnumber[counter] = i
    df_strategy$strategy[counter] = "4. by history"
    df_strategy$QALYsgained[counter] =  (sum(dftemp$QALYlosswithoutvaccine[dftemp$paramnum == i])) -  ( (sum(dftemp$QALYlosswithvaccine[vaccinated])) + (sum(dftemp$QALYlosswithoutvaccine[unvaccinated])))
    df_strategy$vaccinated[counter] =   100*sum(dftemp$N[vaccinated])/(sum(dftemp$N[vaccinated]) + sum(dftemp$N[unvaccinated]))
    df_strategy$vaccinatednum[counter] =   sum(dftemp$N_scale[vaccinated]*dftemp$N[vaccinated])
    df_strategy$caseaverted[counter] = (sum(dftemp$Overallwithoutvaccine[dftemp$paramnum == i])) -  ( (sum(dftemp$Overallwithvaccine[vaccinated])) + (sum(dftemp$Overallwithoutvaccine[unvaccinated])))
    df_strategy$caseavertedperdose[counter] = ((sum(dftemp$Overallwithoutvaccine[dftemp$paramnum == i])) -  ( (sum(dftemp$Overallwithvaccine[vaccinated])) + (sum(dftemp$Overallwithoutvaccine[unvaccinated])))) / df_strategy$vaccinatednum[counter]
    df_strategy$FOI[counter] = (sum(dftemp$Firstinfectionswithvaccine[vaccinated]) + sum(dftemp$Firstinfectionswithoutvaccine[unvaccinated]))/( sum(dftemp$Lifeyearswithoutvaccine[unvaccinated]) + sum(dftemp$Lifeyearswithvaccine[vaccinated]))
    df_strategy$efficacy[counter] = 100 * (1 - sum(dftemp$withvaccine[vaccinated])/ sum(dftemp$withoutvaccine[vaccinated]))
    df_strategy$numrepeat[counter] = (sum(dftemp$withoutvaccine[dftemp$paramnum == i]))
    df_strategy$firstinfections[counter] = (sum( dftemp$N_scale[vaccinated]*dftemp$Firstinfectionswithvaccine[vaccinated]) + sum(dftemp$N_scale[unvaccinated]*dftemp$Firstinfectionswithoutvaccine[unvaccinated]))
    df_strategy$lifeyears[counter] =  sum(dftemp$N_scale[unvaccinated]*dftemp$Lifeyearswithoutvaccine[unvaccinated]) + sum(dftemp$N_scale[vaccinated]*dftemp$Lifeyearswithvaccine[vaccinated])
    df_strategy$impactnum[counter] =  1000*(FOIwithoutvaccine - df_strategy$firstinfections[counter]/df_strategy$lifeyears[counter])
    df_strategy$impact[counter] = 100*(FOIwithoutvaccine - df_strategy$firstinfections[counter]/df_strategy$lifeyears[counter])/FOIwithoutvaccine
    
    counter = counter + 1
    
    vaccinated = dftemp$HIV_prepuse != "HIV- non-PrEP user" & dftemp$paramnum == i
    unvaccinated = dftemp$HIV_prepuse == "HIV- non-PrEP user" & dftemp$paramnum == i
    
    
    df_strategy$paramnumber[counter] = i
    df_strategy$strategy[counter] = "5. by HIV status + PrEP use"
    df_strategy$QALYsgained[counter] =  (sum(dftemp$QALYlosswithoutvaccine[dftemp$paramnum == i])) -  ( (sum(dftemp$QALYlosswithvaccine[vaccinated])) + (sum(dftemp$QALYlosswithoutvaccine[unvaccinated])))
    df_strategy$vaccinated[counter] =   100*sum(dftemp$N[vaccinated])/(sum(dftemp$N[vaccinated]) + sum(dftemp$N[unvaccinated]))
    df_strategy$vaccinatednum[counter] =   sum(dftemp$N_scale[vaccinated]*dftemp$N[vaccinated])
    df_strategy$caseaverted[counter] = (sum(dftemp$Overallwithoutvaccine[dftemp$paramnum == i])) -  ( (sum(dftemp$Overallwithvaccine[vaccinated])) + (sum(dftemp$Overallwithoutvaccine[unvaccinated])))
    df_strategy$caseavertedperdose[counter] = ((sum(dftemp$Overallwithoutvaccine[dftemp$paramnum == i])) -  ( (sum(dftemp$Overallwithvaccine[vaccinated])) + (sum(dftemp$Overallwithoutvaccine[unvaccinated])))) / df_strategy$vaccinatednum[counter]
    df_strategy$FOI[counter] = (sum(dftemp$Firstinfectionswithvaccine[vaccinated]) + sum(dftemp$Firstinfectionswithoutvaccine[unvaccinated]))/( sum(dftemp$Lifeyearswithoutvaccine[unvaccinated]) + sum(dftemp$Lifeyearswithvaccine[vaccinated]))
    df_strategy$efficacy[counter] = 100 * (1 - sum(dftemp$withvaccine[vaccinated])/ sum(dftemp$withoutvaccine[vaccinated]))
    df_strategy$numrepeat[counter] = (sum(dftemp$withoutvaccine[dftemp$paramnum == i]))
    df_strategy$firstinfections[counter] = (sum( dftemp$N_scale[vaccinated]*dftemp$Firstinfectionswithvaccine[vaccinated]) + sum(dftemp$N_scale[unvaccinated]*dftemp$Firstinfectionswithoutvaccine[unvaccinated]))
    df_strategy$lifeyears[counter] =  sum(dftemp$N_scale[unvaccinated]*dftemp$Lifeyearswithoutvaccine[unvaccinated]) + sum(dftemp$N_scale[vaccinated]*dftemp$Lifeyearswithvaccine[vaccinated])
    df_strategy$impactnum[counter] =  1000*(FOIwithoutvaccine - df_strategy$firstinfections[counter]/df_strategy$lifeyears[counter])
    df_strategy$impact[counter] = 100*(FOIwithoutvaccine - df_strategy$firstinfections[counter]/df_strategy$lifeyears[counter])/FOIwithoutvaccine
    
    counter = counter+1
    
    vaccinated = (dftemp$gc_history == "with history" | dftemp$HIV_prepuse == "PLHIV") & dftemp$paramnum == i
    unvaccinated = (dftemp$gc_history != "with history" & dftemp$HIV_prepuse != "PLHIV") & dftemp$paramnum == i
    
    
    df_strategy$paramnumber[counter] = i
    df_strategy$strategy[counter] = "6. by history + HIV status"
    df_strategy$QALYsgained[counter] =  (sum(dftemp$QALYlosswithoutvaccine[dftemp$paramnum == i])) -  ( (sum(dftemp$QALYlosswithvaccine[vaccinated])) + (sum(dftemp$QALYlosswithoutvaccine[unvaccinated])))
    df_strategy$vaccinated[counter] =   100*sum(dftemp$N[vaccinated])/(sum(dftemp$N[vaccinated]) + sum(dftemp$N[unvaccinated]))
    df_strategy$vaccinatednum[counter] =   sum(dftemp$N_scale[vaccinated]*dftemp$N[vaccinated])
    df_strategy$caseaverted[counter] = (sum(dftemp$Overallwithoutvaccine[dftemp$paramnum == i])) -  ( (sum(dftemp$Overallwithvaccine[vaccinated])) + (sum(dftemp$Overallwithoutvaccine[unvaccinated])))
    df_strategy$caseavertedperdose[counter] = ((sum(dftemp$Overallwithoutvaccine[dftemp$paramnum == i])) -  ( (sum(dftemp$Overallwithvaccine[vaccinated])) + (sum(dftemp$Overallwithoutvaccine[unvaccinated])))) / df_strategy$vaccinatednum[counter]
    df_strategy$FOI[counter] = (sum(dftemp$Firstinfectionswithvaccine[vaccinated]) + sum(dftemp$Firstinfectionswithoutvaccine[unvaccinated]))/( sum(dftemp$Lifeyearswithoutvaccine[unvaccinated]) + sum(dftemp$Lifeyearswithvaccine[vaccinated]))
    df_strategy$efficacy[counter] = 100 * (1 - sum(dftemp$withvaccine[vaccinated])/ sum(dftemp$withoutvaccine[vaccinated]))
    df_strategy$numrepeat[counter] = (sum(dftemp$withoutvaccine[dftemp$paramnum == i]))
    df_strategy$firstinfections[counter] = (sum( dftemp$N_scale[vaccinated]*dftemp$Firstinfectionswithvaccine[vaccinated]) + sum(dftemp$N_scale[unvaccinated]*dftemp$Firstinfectionswithoutvaccine[unvaccinated]))
    df_strategy$lifeyears[counter] =  sum(dftemp$N_scale[unvaccinated]*dftemp$Lifeyearswithoutvaccine[unvaccinated]) + sum(dftemp$N_scale[vaccinated]*dftemp$Lifeyearswithvaccine[vaccinated])
    df_strategy$impactnum[counter] =  1000*(FOIwithoutvaccine - df_strategy$firstinfections[counter]/df_strategy$lifeyears[counter])
    df_strategy$impact[counter] = 100*(FOIwithoutvaccine - df_strategy$firstinfections[counter]/df_strategy$lifeyears[counter])/FOIwithoutvaccine

    counter = counter+1
    
    vaccinated = (dftemp$gc_history == "with history" | dftemp$HIV_prepuse == "HIV- PrEP user") & dftemp$paramnum == i
    unvaccinated = (dftemp$gc_history != "with history" & dftemp$HIV_prepuse != "HIV- PrEP user") & dftemp$paramnum == i
    
    
    df_strategy$paramnumber[counter] = i
    df_strategy$strategy[counter] = "7. by history + PrEP use"
    df_strategy$QALYsgained[counter] =  (sum(dftemp$QALYlosswithoutvaccine[dftemp$paramnum == i])) -  ( (sum(dftemp$QALYlosswithvaccine[vaccinated])) + (sum(dftemp$QALYlosswithoutvaccine[unvaccinated])))
    df_strategy$vaccinated[counter] =   100*sum(dftemp$N[vaccinated])/(sum(dftemp$N[vaccinated]) + sum(dftemp$N[unvaccinated]))
    df_strategy$vaccinatednum[counter] =   sum(dftemp$N_scale[vaccinated]*dftemp$N[vaccinated])
    df_strategy$caseaverted[counter] = (sum(dftemp$Overallwithoutvaccine[dftemp$paramnum == i])) -  ( (sum(dftemp$Overallwithvaccine[vaccinated])) + (sum(dftemp$Overallwithoutvaccine[unvaccinated])))
    df_strategy$caseavertedperdose[counter] = ((sum(dftemp$Overallwithoutvaccine[dftemp$paramnum == i])) -  ( (sum(dftemp$Overallwithvaccine[vaccinated])) + (sum(dftemp$Overallwithoutvaccine[unvaccinated])))) / df_strategy$vaccinatednum[counter]
    df_strategy$FOI[counter] = (sum(dftemp$Firstinfectionswithvaccine[vaccinated]) + sum(dftemp$Firstinfectionswithoutvaccine[unvaccinated]))/( sum(dftemp$Lifeyearswithoutvaccine[unvaccinated]) + sum(dftemp$Lifeyearswithvaccine[vaccinated]))
    df_strategy$efficacy[counter] = 100 * (1 - sum(dftemp$withvaccine[vaccinated])/ sum(dftemp$withoutvaccine[vaccinated]))
    df_strategy$numrepeat[counter] = (sum(dftemp$withoutvaccine[dftemp$paramnum == i]))
    df_strategy$firstinfections[counter] = (sum( dftemp$N_scale[vaccinated]*dftemp$Firstinfectionswithvaccine[vaccinated]) + sum(dftemp$N_scale[unvaccinated]*dftemp$Firstinfectionswithoutvaccine[unvaccinated]))
    df_strategy$lifeyears[counter] =  sum(dftemp$N_scale[unvaccinated]*dftemp$Lifeyearswithoutvaccine[unvaccinated]) + sum(dftemp$N_scale[vaccinated]*dftemp$Lifeyearswithvaccine[vaccinated])
    df_strategy$impactnum[counter] =  1000*(FOIwithoutvaccine - df_strategy$firstinfections[counter]/df_strategy$lifeyears[counter])
    df_strategy$impact[counter] = 100*(FOIwithoutvaccine - df_strategy$firstinfections[counter]/df_strategy$lifeyears[counter])/FOIwithoutvaccine
    
    counter = counter+1
    
    vaccinated = (dftemp$gc_history == "with history" | dftemp$HIV_prepuse == "HIV- PrEP user" | dftemp$HIV_prepuse == "PLHIV") & dftemp$paramnum == i
    unvaccinated = (dftemp$gc_history != "with history" & dftemp$HIV_prepuse != "HIV- PrEP user" & dftemp$HIV_prepuse != "PLHIV") & dftemp$paramnum == i
    
    df_strategy$paramnumber[counter] = i
    df_strategy$strategy[counter] = "8. by history + HIV status + PrEP use"
    df_strategy$QALYsgained[counter] =  (sum(dftemp$QALYlosswithoutvaccine[dftemp$paramnum == i])) -  ( (sum(dftemp$QALYlosswithvaccine[vaccinated])) + (sum(dftemp$QALYlosswithoutvaccine[unvaccinated])))
    df_strategy$vaccinated[counter] =   100*sum(dftemp$N[vaccinated])/(sum(dftemp$N[vaccinated]) + sum(dftemp$N[unvaccinated]))
    df_strategy$vaccinatednum[counter] =   sum(dftemp$N_scale[vaccinated]*dftemp$N[vaccinated])
    df_strategy$caseaverted[counter] = (sum(dftemp$Overallwithoutvaccine[dftemp$paramnum == i])) -  ( (sum(dftemp$Overallwithvaccine[vaccinated])) + (sum(dftemp$Overallwithoutvaccine[unvaccinated])))
    df_strategy$caseavertedperdose[counter] = ((sum(dftemp$Overallwithoutvaccine[dftemp$paramnum == i])) -  ( (sum(dftemp$Overallwithvaccine[vaccinated])) + (sum(dftemp$Overallwithoutvaccine[unvaccinated])))) / df_strategy$vaccinatednum[counter]
    df_strategy$FOI[counter] = (sum(dftemp$Firstinfectionswithvaccine[vaccinated]) + sum(dftemp$Firstinfectionswithoutvaccine[unvaccinated]))/( sum(dftemp$Lifeyearswithoutvaccine[unvaccinated]) + sum(dftemp$Lifeyearswithvaccine[vaccinated]))
    df_strategy$efficacy[counter] = 100 * (1 - sum(dftemp$withvaccine[vaccinated])/ sum(dftemp$withoutvaccine[vaccinated]))
    df_strategy$numrepeat[counter] = (sum(dftemp$withoutvaccine[dftemp$paramnum == i]))
    df_strategy$firstinfections[counter] = (sum( dftemp$N_scale[vaccinated]*dftemp$Firstinfectionswithvaccine[vaccinated]) + sum(dftemp$N_scale[unvaccinated]*dftemp$Firstinfectionswithoutvaccine[unvaccinated]))
    df_strategy$lifeyears[counter] =  sum(dftemp$N_scale[unvaccinated]*dftemp$Lifeyearswithoutvaccine[unvaccinated]) + sum(dftemp$N_scale[vaccinated]*dftemp$Lifeyearswithvaccine[vaccinated])
    df_strategy$impactnum[counter] =  1000*(FOIwithoutvaccine - df_strategy$firstinfections[counter]/df_strategy$lifeyears[counter])
    df_strategy$impact[counter] = 100*(FOIwithoutvaccine - df_strategy$firstinfections[counter]/df_strategy$lifeyears[counter])/FOIwithoutvaccine
    
    counter = counter+1
    
    vaccinated = dftemp$paramnum == i
    unvaccinated = c()
    
    df_strategy$paramnumber[counter] = i
    df_strategy$strategy[counter] = "9. to all"
    df_strategy$QALYsgained[counter] =  (sum(dftemp$QALYlosswithoutvaccine[dftemp$paramnum == i])) -  ( (sum(dftemp$QALYlosswithvaccine[vaccinated])) + (sum(dftemp$QALYlosswithoutvaccine[unvaccinated])))
    df_strategy$vaccinated[counter] =   100*sum(dftemp$N[vaccinated])/(sum(dftemp$N[vaccinated]) + sum(dftemp$N[unvaccinated]))
    df_strategy$vaccinatednum[counter] =   sum(dftemp$N_scale[vaccinated]*dftemp$N[vaccinated])
    df_strategy$caseaverted[counter] = (sum(dftemp$Overallwithoutvaccine[dftemp$paramnum == i])) -  ( (sum(dftemp$Overallwithvaccine[vaccinated])) + (sum(dftemp$Overallwithoutvaccine[unvaccinated])))
    df_strategy$caseavertedperdose[counter] = ((sum(dftemp$Overallwithoutvaccine[dftemp$paramnum == i])) -  ( (sum(dftemp$Overallwithvaccine[vaccinated])) + (sum(dftemp$Overallwithoutvaccine[unvaccinated])))) / df_strategy$vaccinatednum[counter]
    df_strategy$FOI[counter] = (sum(dftemp$Firstinfectionswithvaccine[vaccinated]) + sum(dftemp$Firstinfectionswithoutvaccine[unvaccinated]))/( sum(dftemp$Lifeyearswithoutvaccine[unvaccinated]) + sum(dftemp$Lifeyearswithvaccine[vaccinated]))
    df_strategy$efficacy[counter] = 100 * (1 - sum(dftemp$withvaccine[vaccinated])/ sum(dftemp$withoutvaccine[vaccinated]))
    df_strategy$numrepeat[counter] = (sum(dftemp$withoutvaccine[dftemp$paramnum == i]))
    df_strategy$firstinfections[counter] = (sum( dftemp$N_scale[vaccinated]*dftemp$Firstinfectionswithvaccine[vaccinated]) + sum(dftemp$N_scale[unvaccinated]*dftemp$Firstinfectionswithoutvaccine[unvaccinated]))
    df_strategy$lifeyears[counter] =  sum(dftemp$N_scale[unvaccinated]*dftemp$Lifeyearswithoutvaccine[unvaccinated]) + sum(dftemp$N_scale[vaccinated]*dftemp$Lifeyearswithvaccine[vaccinated])
    df_strategy$impactnum[counter] =  1000*(FOIwithoutvaccine - df_strategy$firstinfections[counter]/df_strategy$lifeyears[counter])
    df_strategy$impact[counter] = 100*(FOIwithoutvaccine - df_strategy$firstinfections[counter]/df_strategy$lifeyears[counter])/FOIwithoutvaccine
    
    counter = counter+1
    
    
  }
  
  df_strategy
  
}



model_data_comparison <- function(df_data,
                              df_model,
                              middle,
                              lower,
                              upper,
                              modeloutputs,
                              labels) {
  
  stopifnot(length(modeloutputs) == length(labels))
  
  df_foi <- df_data[c(
    "Gender.Sexual.orientation.group",
    "GC.History",
    "HIV.Status.PrEP.user",
    "Race.group",
    middle,
    lower,
    upper
  )]
  
  ngroup <- nrow(df_foi)
  
  # Duplicate once for each dataset (SSuN + each model output)
  df_foi <- do.call(rbind, replicate(length(modeloutputs) + 1,
                                     df_foi,
                                     simplify = FALSE))
  
  # Labels
  df_foi$type <- rep(c("SSuN data", labels), each = ngroup)
  
  # Combined grouping variable
  df_foi$combined <- paste0(
    df_foi$HIV.Status.PrEP.user, ", ",
    df_foi$GC.History, ", ",
    df_foi$Race.group
  )
  
  # Fill in each model output
  for (j in seq_along(modeloutputs)) {
    
    output <- modeloutputs[j]
    rows <- (j * ngroup + 1):((j + 1) * ngroup)
    
    for (i in seq_len(ngroup)) {
      
      temp <- df_model$sex_orientation == df_foi$Gender.Sexual.orientation.group[i] &
        df_model$gc_history == df_foi$GC.History[i] &
        df_model$HIV_prepuse == df_foi$HIV.Status.PrEP.user[i] &
        df_model$ethnicity == df_foi$Race.group[i]
      
      values <- df_model[[output]][temp]
      
      df_foi[[middle]][rows[i]] <- mean(values)
      df_foi[[lower]][rows[i]] <- quantile(values, 0.025)
      df_foi[[upper]][rows[i]] <- quantile(values, 0.975)
    }
  }
  
  df_foi
}
