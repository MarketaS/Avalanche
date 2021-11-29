######################################## FUN ###################################

find_best_vars <- function(indata, period, bootstrap, boot_period, new_candi){
   
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
  
  best <- list()
  list_imp2 <- list()
  M <- list()
  
if (bootstrap == "yes"){ 
    
    ######################################## 1 bootstrap ############################ 
    for (k in 1:100){
      sample_yr <- sample(boot_period, replace = TRUE)
      dta_fin <- dta_sample[year(Date) %in% sample_yr[1]]
      for(i in 2:length(sample_yr)){
        dta_tmp <- dta_sample[year(Date) %in% sample_yr[i]]
        dta_fin <- rbind(dta_fin, dta_tmp)
      }
      
    
    for (i in 1:length(new_candi)){
      M[[i]] = glm(dta_fin$event ~ dta_fin[[new_candi[i]]], family = "binomial")
    }
    
    # if (M$converged == TRUE) {n = n+1} else {next}  
    # if (n == 101) {break}
    sM <- data.table(new_candi, aic = lapply(M, AIC), lapply(M, function(x)summary(x)$coe[2,3]))
    sM[, aic := unlist(aic)]
    sM[, V3 := unlist(V3)]
    
    best[[k]] <- data.frame(best = sM[ which.min(aic), new_candi], run = k)
    print(k)

     list_imp <- list()
     for (i in 1:length(new_candi)){
       aa <- varImp(M[[i]], scale = TRUE)
       aa <- data.table(tibble::rownames_to_column(aa, "candi"))
       aa[candi == "dta_fin[[new_candi[i]]]", candi:= new_candi[i]]
       list_imp[[i]] <- aa
    }
    kk <- rbindlist(list_imp)
    kk$run <- k
    
    list_imp2[[k]] <- kk
    
    }
    
    best_dta <- rbindlist(best)
    
    imp_fin <- rbindlist(list_imp2)
    
    candi1 <- names(which.max(table(best_dta$best)))
    
    all1 <- list(best_dta, imp_fin, candi1)
    message("Candi 1 found")
    
    ######################################## 2 bootstrap ############################ 
    new_candi = new_candi[new_candi != candi1]
    M <- list()
    list_imp2 <- list()
    
    for (k in 1:100){
  
      sample_yr <- sample(boot_period, replace = TRUE)
      dta_fin <- dta_sample[year(Date) %in% sample_yr[1]]
      for(i in 2:length(sample_yr)){
        dta_tmp <- dta_sample[year(Date) %in% sample_yr[i]]
        dta_fin <- rbind(dta_fin, dta_tmp)
      }
      
      
      for (i in 1:length(new_candi)){
        M[[i]] = glm(dta_fin$event ~ dta_fin[, get(candi1)] + dta_fin[[new_candi[i]]], family = "binomial")
      }
      
      sM <- data.table(new_candi, aic = lapply(M, AIC), lapply(M, function(x)summary(x)$coe[2,3]))
      sM[, aic := unlist(aic)]
      sM[, V3 := unlist(V3)]
      
      best[[k]] <- data.frame(best = sM[ which.min(aic), new_candi], run = k)
      print(k)
      
      list_imp <- list()
      for (i in 1:length(new_candi)){
        aa <- varImp(M[[i]], scale = TRUE)
        aa <- data.table(tibble::rownames_to_column(aa, "candi"))
        aa[candi == "dta_fin[[new_candi[i]]]", candi:= new_candi[i]]
        aa[candi == "dta_fin[, get(candi1)]", candi:= candi1] 
        list_imp[[i]] <- aa 
      }
      kk <- rbindlist(list_imp)
      kk$run <- k
      
      list_imp2[[k]] <- kk
      
    }
    
    best_dta <- rbindlist(best)
    
    imp_fin <- rbindlist(list_imp2)
    
    candi2 <- names(which.max(table(best_dta$best)))
    
    all2 <- list(best_dta, imp_fin, candi2)
    message("Candi 2 found")
    
    ######################################## 3 bootstrap ############################
    new_candi = new_candi[new_candi != candi2]
    M <- list()
    list_imp2 <- list()
    
    for (k in 1:100){
      
      sample_yr <- sample(boot_period, replace = TRUE)
      dta_fin <- dta_sample[year(Date) %in% sample_yr[1]]
      for(i in 2:length(sample_yr)){
        dta_tmp <- dta_sample[year(Date) %in% sample_yr[i]]
        dta_fin <- rbind(dta_fin, dta_tmp)
      }
      
      
      for (i in 1:length(new_candi)){
        M[[i]] = glm(dta_fin$event ~ dta_fin[, get(candi1)] + dta_fin[, get(candi2)] + dta_fin[[new_candi[i]]], family = "binomial")
      }
      
      sM <- data.table(new_candi, aic = lapply(M, AIC), lapply(M, function(x)summary(x)$coe[2,3]))
      sM[, aic := unlist(aic)]
      sM[, V3 := unlist(V3)]
      
      best[[k]] <- data.frame(best = sM[ which.min(aic), new_candi], run = k)
      print(k)
      
      list_imp <- list()
      for (i in 1:length(new_candi)){
        aa <- varImp(M[[i]], scale = TRUE)
        aa <- data.table(tibble::rownames_to_column(aa, "candi"))
        aa[candi == "dta_fin[[new_candi[i]]]", candi:= new_candi[i]]
        aa[candi == "dta_fin[, get(candi1)]", candi:= candi1] 
        aa[candi == "dta_fin[, get(candi2)]", candi:= candi2] 
        list_imp[[i]] <- aa 
      }
      kk <- rbindlist(list_imp)
      kk$run <- k
      
      list_imp2[[k]] <- kk
      
    }
    
    best_dta <- rbindlist(best)
    
    imp_fin <- rbindlist(list_imp2)
    
    candi3 <- names(which.max(table(best_dta$best)))
    
    all3 <- list(best_dta, imp_fin, candi3)
    message("Candi 3 found")
    
    ######################################## 4 bootstrap ############################
    new_candi = new_candi[new_candi != candi3]
    M <- list()
    list_imp2 <- list()
    
    for (k in 1:100){
      
      sample_yr <- sample(boot_period, replace = TRUE)
      dta_fin <- dta_sample[year(Date) %in% sample_yr[1]]
      for(i in 2:length(sample_yr)){
        dta_tmp <- dta_sample[year(Date) %in% sample_yr[i]]
        dta_fin <- rbind(dta_fin, dta_tmp)
      }
      
      for (i in 1:length(new_candi)){
        M[[i]] = glm(dta_fin$event ~ dta_fin[, get(candi1)] + dta_fin[, get(candi2)] + dta_fin[, get(candi3)] + dta_fin[[new_candi[i]]], family = "binomial")
      }
      
      sM <- data.table(new_candi, aic = lapply(M, AIC), lapply(M, function(x)summary(x)$coe[2,3]))
      sM[, aic := unlist(aic)]
      sM[, V3 := unlist(V3)]
      
      best[[k]] <- data.frame(best = sM[ which.min(aic), new_candi], run = k)
      print(k)
      
      list_imp <- list()
      for (i in 1:length(new_candi)){
        aa <- varImp(M[[i]], scale = TRUE)
        aa <- data.table(tibble::rownames_to_column(aa, "candi"))
        aa[candi == "dta_fin[[new_candi[i]]]", candi:= new_candi[i]]
        aa[candi == "dta_fin[, get(candi1)]", candi:= candi1] 
        aa[candi == "dta_fin[, get(candi2)]", candi:= candi2] 
        aa[candi == "dta_fin[, get(candi3)]", candi:= candi3] 
        list_imp[[i]] <- aa 
      }
      kk <- rbindlist(list_imp)
      kk$run <- k
      
      list_imp2[[k]] <- kk
      
    }
    
    best_dta <- rbindlist(best)
    
    imp_fin <- rbindlist(list_imp2)
    
    candi4 <- names(which.max(table(best_dta$best)))
    
    all4 <- list(best_dta, imp_fin, candi4)
    message("Candi 4 found")
    
    ######################################## 5 bootstrap ############################
    new_candi = new_candi[new_candi != candi4]
    M <- list()
    list_imp2 <- list()
    
    for (k in 1:100){
      
      sample_yr <- sample(boot_period, replace = TRUE)
      dta_fin <- dta_sample[year(Date) %in% sample_yr[1]]
      for(i in 2:length(sample_yr)){
        dta_tmp <- dta_sample[year(Date) %in% sample_yr[i]]
        dta_fin <- rbind(dta_fin, dta_tmp)
      }
      
      
      
      for (i in 1:length(new_candi)){
        M[[i]] = glm(dta_fin$event ~ dta_fin[, get(candi1)] + dta_fin[, get(candi2)] + dta_fin[, get(candi3)] + dta_fin[, get(candi4)] + dta_fin[[new_candi[i]]], family = "binomial")
      }
      
      sM <- data.table(new_candi, aic = lapply(M, AIC), lapply(M, function(x)summary(x)$coe[2,3]))
      sM[, aic := unlist(aic)]
      sM[, V3 := unlist(V3)]
      
      best[[k]] <- data.frame(best = sM[ which.min(aic), new_candi], run = k)
      print(k)
      
      list_imp <- list()
      for (i in 1:length(new_candi)){
        aa <- varImp(M[[i]], scale = TRUE)
        aa <- data.table(tibble::rownames_to_column(aa, "candi"))
        aa[candi == "dta_fin[[new_candi[i]]]", candi:= new_candi[i]]
        aa[candi == "dta_fin[, get(candi1)]", candi:= candi1] 
        aa[candi == "dta_fin[, get(candi2)]", candi:= candi2] 
        aa[candi == "dta_fin[, get(candi3)]", candi:= candi3] 
        aa[candi == "dta_fin[, get(candi4)]", candi:= candi4] 
        list_imp[[i]] <- aa 
      }
      kk <- rbindlist(list_imp)
      kk$run <- k
      
      list_imp2[[k]] <- kk
      
    }
    
    best_dta <- rbindlist(best)
    
    imp_fin <- rbindlist(list_imp2)
    
    candi5 <- names(which.max(table(best_dta$best)))
    
    all5 <- list(best_dta, imp_fin, candi5)
    message("Candi 5 found")
    
    ######################################## 6 bootstrap ############################
    new_candi = new_candi[new_candi != candi5]
    M <- list()
    list_imp2 <- list()
    
    for (k in 1:100){
      sample_yr <- sample(boot_period, replace = TRUE)
      dta_fin <- dta_sample[year(Date) %in% sample_yr[1]]
      for(i in 2:length(sample_yr)){
        dta_tmp <- dta_sample[year(Date) %in% sample_yr[i]]
        dta_fin <- rbind(dta_fin, dta_tmp)
      }
      
      
      for (i in 1:length(new_candi)){
        M[[i]] = glm(dta_fin$event ~ dta_fin[, get(candi1)] + dta_fin[, get(candi2)] + dta_fin[, get(candi3)] + dta_fin[, get(candi4)] + dta_fin[, get(candi5)] + dta_fin[[new_candi[i]]], family = "binomial")
      }
      
      sM <- data.table(new_candi, aic = lapply(M, AIC), lapply(M, function(x)summary(x)$coe[2,3]))
      sM[, aic := unlist(aic)]
      sM[, V3 := unlist(V3)]
      
      best[[k]] <- data.frame(best = sM[ which.min(aic), new_candi], run = k)
      print(k)
      
      list_imp <- list()
      for (i in 1:length(new_candi)){
        aa <- varImp(M[[i]], scale = TRUE)
        aa <- data.table(tibble::rownames_to_column(aa, "candi"))
        aa[candi == "dta_fin[[new_candi[i]]]", candi:= new_candi[i]]
        aa[candi == "dta_fin[, get(candi1)]", candi:= candi1] 
        aa[candi == "dta_fin[, get(candi2)]", candi:= candi2] 
        aa[candi == "dta_fin[, get(candi3)]", candi:= candi3] 
        aa[candi == "dta_fin[, get(candi4)]", candi:= candi4] 
        aa[candi == "dta_fin[, get(candi5)]", candi:= candi5] 
        list_imp[[i]] <- aa 
      }
      kk <- rbindlist(list_imp)
      kk$run <- k
      
      list_imp2[[k]] <- kk
      
    }
    
    best_dta <- rbindlist(best)
    
    imp_fin <- rbindlist(list_imp2)
    
    candi6 <- names(which.max(table(best_dta$best)))
    
    all6 <- list(best_dta, imp_fin, candi6)
    message("Candi 6 found")
    
    all <- list(all1, all2, all3, all4, all5, all6)
    all <- c(candi1, candi2,candi3, candi4, candi5, candi6)
    return(all)
    
    } else {
      
      dta1 <- dta[year(Date) %in% period]
      dta_fin <- dta1
      
      ################################# 1 #########################################
      
      for (i in 1:length(new_candi)){
        M[[i]] = glm(dta_fin$event ~ dta_fin[[new_candi[i]]], family = "binomial")
      }
      
      sM <- data.table(new_candi, aic = lapply(M, AIC), lapply(M, function(x)summary(x)$coe[2,3]))
      sM[, aic := unlist(aic)]
      sM[, V3 := unlist(V3)]
      
      best <- data.frame(best = sM[ which.min(aic), new_candi])
      
      list_imp <- list()
      for (i in 1:length(new_candi)){
        aa <- varImp(M[[i]], scale = TRUE)
        aa <- data.table(tibble::rownames_to_column(aa, "candi"))
        aa[candi == "dta_fin[[new_candi[i]]]", candi:= new_candi[i]] 
        list_imp[[i]] <- aa 
      }
      kk <- rbindlist(list_imp)
      
      candi1 <- names(which.max(table(best$best)))
      
      all1 <- list(best, kk, candi1)
      message("Candi 1 found")
      
      ################################# 2 #########################################
      new_candi = new_candi[new_candi != candi1]
      M <- list()
      
      for (i in 1:length(new_candi)){
        M[[i]] = glm(dta_fin$event ~ dta_fin[, get(candi1)] + dta_fin[[new_candi[i]]], family = "binomial")
      }
      
      sM <- data.table(new_candi, aic = lapply(M, AIC), lapply(M, function(x)summary(x)$coe[2,3]))
      sM[, aic := unlist(aic)]
      sM[, V3 := unlist(V3)]
      
      best <- data.frame(best = sM[ which.min(aic), new_candi])
      
      list_imp <- list()
      for (i in 1:length(new_candi)){
        aa <- varImp(M[[i]], scale = TRUE)
        aa <- data.table(tibble::rownames_to_column(aa, "candi"))
        aa[candi == "dta_fin[[new_candi[i]]]", candi:= new_candi[i]] 
        aa[candi == "dta_fin[, get(candi1)]", candi:= candi1] 
        list_imp[[i]] <- aa 
      }
      kk <- rbindlist(list_imp)
      
      candi2 <- names(which.max(table(best$best)))
      
      all2 <- list(best, kk, candi2)
      message("Candi 2 found")
      
      ################################# 3 #########################################
      new_candi = new_candi[new_candi != candi2]
      M <- list()
      
      for (i in 1:length(new_candi)){
        M[[i]] = glm(dta_fin$event ~ dta_fin[, get(candi1)] + dta_fin[, get(candi2)] + dta_fin[[new_candi[i]]], family = "binomial")
      }
      
      sM <- data.table(new_candi, aic = lapply(M, AIC), lapply(M, function(x)summary(x)$coe[2,3]))
      sM[, aic := unlist(aic)]
      sM[, V3 := unlist(V3)]
      
      best <- data.frame(best = sM[ which.min(aic), new_candi])
      
      list_imp <- list()
      for (i in 1:length(new_candi)){
        aa <- varImp(M[[i]], scale = TRUE)
        aa <- data.table(tibble::rownames_to_column(aa, "candi"))
        aa[candi == "dta_fin[[new_candi[i]]]", candi:= new_candi[i]] 
        aa[candi == "dta_fin[, get(candi1)]", candi:= candi1] 
        aa[candi == "dta_fin[, get(candi2)]", candi:= candi2] 
        list_imp[[i]] <- aa 
      }
      kk <- rbindlist(list_imp)
      
      candi3 <- names(which.max(table(best$best)))
      
      all3 <- list(best, kk, candi3)
      message("Candi 3 found")
      
      ################################# 4 #########################################
      new_candi = new_candi[new_candi != candi3]
      M <- list()
      
      for (i in 1:length(new_candi)){
        M[[i]] = glm(dta_fin$event ~ dta_fin[, get(candi1)] + dta_fin[, get(candi2)] + dta_fin[, get(candi3)] + dta_fin[[new_candi[i]]], family = "binomial")
      }
      
      sM <- data.table(new_candi, aic = lapply(M, AIC), lapply(M, function(x)summary(x)$coe[2,3]))
      sM[, aic := unlist(aic)]
      sM[, V3 := unlist(V3)]
      
      best <- data.frame(best = sM[ which.min(aic), new_candi])
      
      list_imp <- list()
      for (i in 1:length(new_candi)){
        aa <- varImp(M[[i]], scale = TRUE)
        aa <- data.table(tibble::rownames_to_column(aa, "candi"))
        aa[candi == "dta_fin[[new_candi[i]]]", candi:= new_candi[i]] 
        aa[candi == "dta_fin[, get(candi1)]", candi:= candi1] 
        aa[candi == "dta_fin[, get(candi2)]", candi:= candi2] 
        aa[candi == "dta_fin[, get(candi3)]", candi:= candi3] 
        list_imp[[i]] <- aa 
      }
      kk <- rbindlist(list_imp)
      
      candi4 <- names(which.max(table(best$best)))
      
      all4 <- list(best, kk, candi4)
      message("Candi 4 found")
      
      ################################# 5 #########################################
      new_candi = new_candi[new_candi != candi4]
      M <- list()
      
      for (i in 1:length(new_candi)){
        M[[i]] = glm(dta_fin$event ~ dta_fin[, get(candi1)] + dta_fin[, get(candi2)] + dta_fin[, get(candi3)] + dta_fin[, get(candi4)] + dta_fin[[new_candi[i]]], family = "binomial")
      }
      
      sM <- data.table(new_candi, aic = lapply(M, AIC), lapply(M, function(x)summary(x)$coe[2,3]))
      sM[, aic := unlist(aic)]
      sM[, V3 := unlist(V3)]
      
      best <- data.frame(best = sM[ which.min(aic), new_candi])
      
      list_imp <- list()
      for (i in 1:length(new_candi)){
        aa <- varImp(M[[i]], scale = TRUE)
        aa <- data.table(tibble::rownames_to_column(aa, "candi"))
        aa[candi == "dta_fin[[new_candi[i]]]", candi:= new_candi[i]] 
        aa[candi == "dta_fin[, get(candi1)]", candi:= candi1] 
        aa[candi == "dta_fin[, get(candi2)]", candi:= candi2] 
        aa[candi == "dta_fin[, get(candi3)]", candi:= candi3] 
        aa[candi == "dta_fin[, get(candi4)]", candi:= candi4] 
        list_imp[[i]] <- aa 
      }
      kk <- rbindlist(list_imp)
      
      candi5 <- names(which.max(table(best$best)))
      
      all5 <- list(best, kk, candi5)
      message("Candi 5 found")
      
      ################################# 6 #########################################
      new_candi = new_candi[new_candi != candi5]
      M <- list()
      
      for (i in 1:length(new_candi)){
        M[[i]] = glm(dta_fin$event ~ dta_fin[, get(candi1)] + dta_fin[, get(candi2)] + dta_fin[, get(candi3)] + dta_fin[, get(candi4)] + dta_fin[, get(candi5)] + dta_fin[[new_candi[i]]], family = "binomial")
      }
      
      sM <- data.table(new_candi, aic = lapply(M, AIC), lapply(M, function(x)summary(x)$coe[2,3]))
      sM[, aic := unlist(aic)]
      sM[, V3 := unlist(V3)]
      
      best <- data.frame(best = sM[ which.min(aic), new_candi])
      
      list_imp <- list()
      for (i in 1:length(new_candi)){
        aa <- varImp(M[[i]], scale = TRUE)
        aa <- data.table(tibble::rownames_to_column(aa, "candi"))
        aa[candi == "dta_fin[[new_candi[i]]]", candi:= new_candi[i]] 
        aa[candi == "dta_fin[, get(candi1)]", candi:= candi1] 
        aa[candi == "dta_fin[, get(candi2)]", candi:= candi2] 
        aa[candi == "dta_fin[, get(candi3)]", candi:= candi3] 
        aa[candi == "dta_fin[, get(candi4)]", candi:= candi4] 
        aa[candi == "dta_fin[, get(candi5)]", candi:= candi5] 
        list_imp[[i]] <- aa 
      }
      kk <- rbindlist(list_imp)
      
      candi6 <- names(which.max(table(best$best)))
      
      all6 <- list(best, kk, candi6)
      message("Candi 6 found")
      
      #all <- list(all1, all2, all3, all4, all5, all6)
      all <- c(candi1, candi2,candi3, candi4, candi5, candi6)
      return(all)
       
    }
}



