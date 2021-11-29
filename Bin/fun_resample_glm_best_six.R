######################################## FUN ###################################

find_resample <- function(indata, boot_period, best_candi, nboots = 100, navd_weights = 1, avd_weights = 1, maxit = 25, epsilon = 1e-8){
  
  library(data.table)
  library(readr)
  library(ggplot2)
  library(lubridate)
  library(writexl)
  library(dplyr)  
  library(zoo)
  library(caret)

  
  dta <- data.table(indata)
  
  dta_sample <- dta
  # a <- dta_sample[year(Date) %in% c(2004, 2001, 1986, 2010, 1997, 2015) & month(Date) %in% c(10, 11, 12), ]
  # b <- dta_sample[year(Date) %in% c(2005, 2002, 1987, 2011, 1998, 2016) & month(Date) %in% c(1, 2, 3, 4, 5), ]
  # 
  # dta_sample <- dta_sample[!Date %in% c(a[,Date],b[,Date])]
  list_imp2 <- list()
  
  predictors <- best_candi[1]
  for(i in 2:length(best_candi)){
    predictors <- paste(predictors, "+", best_candi[i])
  }
  
  formula <- as.formula(paste("event", "~", predictors))
  
  for (k in 1:nboots){
      sample_yr <- sample(boot_period, replace = TRUE)
      dta_fin <- dta_sample[year(Date) %in% sample_yr[1]]
      for(i in 2:length(sample_yr)){
        dta_tmp <- dta_sample[year(Date) %in% sample_yr[i]]
        dta_fin <- rbind(dta_fin, dta_tmp)
      }
      
      dta_fin[event == 0, weights := navd_weights]
      dta_fin[event == 1, weights := avd_weights]
      
      M <- glm(formula, data = dta_fin,
               family = binomial, weights = dta_fin$weights, control = list(maxit = maxit, epsilon = epsilon)) 
      
      if(!M$converged){
        M <- glm(formula, data = dta_fin,
                 family = binomial, weights = dta_fin$weights, control = list(maxit = maxit, epsilon = epsilon, trace = T)) 
      }
      aa <- varImp(M, scale = TRUE)
      aa <- data.table(aa, boot = k, variable = best_candi)
      list_imp2[[k]] <- aa
      
  }

  imp_fin <- rbindlist(list_imp2)
  all <- imp_fin
  return(all)
  
}



