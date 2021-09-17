
library(data.table)
library(readr)
library(ggplot2)
library(lubridate)
library(writexl)
library(dplyr)  
library(zoo)

setwd ("c:/Users/marketa.souckova/Documents/laviny/")

glm_data_lbou_d <- adcast_all[stat == "LBOU"]
glm_data_lbou_W_d <- glm_data_lbou_d[CAW == "warm"]
glm_data_lbou_C_d <- glm_data_lbou_d[CAW == "cold"]

dta = data.table(glm_data_lbou_W_d)
dta = dta[year(Date) %in% 1968:2020]

candi = names(dta)[-1]
nn = length(candi)
new_candi = candi[6:nn]
M = list()
for (i in 1:length(new_candi)){
  print(i)
  M[[i]] = glm(dta$event ~ dta[[new_candi[i]]])# the  predictor
}

sM = data.table(new_candi, aic = lapply(M, AIC), lapply(M, function(x)summary(x)$coe[2,3]))
sM[, aic := unlist(aic)]
sM[, V3 := unlist(V3)]
sM[,abs := abs(aic)]

M2 = list()
candi = candi[candi!='SWE_value']
nn = length(candi)
new_candi = candi[6:nn]
for (i in 1:length(new_candi)){
  print(i)
  M2[[i]] = glm(dta$event ~ dta$SWE_value + dta[[new_candi[i]]] )
}

sM2 = data.table(new_candi, aic = lapply(M2, AIC), lapply(M2, function(x)summary(x)$coe[2,3]))
sM2[, aic := unlist(aic)]
sM2[, V3 := unlist(V3)]
sM2[,abs := abs(aic)]

M3 = list()
# exclude already selected variable
candi = candi[candi!='SLd_value']

nn = length(candi)
new_candi = candi[6:nn]
for (i in 1:length(new_candi)){
  print(i)
  M3[[i]] = glm(dta$event ~ dta$SWE_value + dta$SLd_value + dta[[new_candi[i]]] )#building of already selected variables and others
}

sM3 = data.table(new_candi, aic = lapply(M3, AIC), lapply(M3, function(x)summary(x)$coe[2,3]))# i am not sure what position is coe [3,4]
sM3[, aic := unlist(aic)]
sM3[, V3 := unlist(V3)]
sM3[,abs := abs(aic)]


M4 = list()
candi = candi[candi!='WSavg_value'] # eliminate already chosen predictor

nn = length(candi)
new_candi = candi[6:nn]
for (i in 1:length(new_candi)){
  print(i)
  M4[[i]] = glm(dta$event ~ dta$SWE_value + dta$SLd_value +dta$WSavg_value + dta[[new_candi[i]]] )
}

sM4 = data.table(new_candi, aic = lapply(M4, AIC), lapply(M4, function(x)summary(x)$coe[2,3]))
sM4[, aic := unlist(aic)]
sM4[, V3 := unlist(V3)]
sM4[,abs := abs(aic)]

M5 = list()

candi = candi[candi!='Tair_value'] #eliminate already chosen predictor
#Rain_Ta_value
nn = length(candi)
new_candi = candi[6:nn]
for (i in 1:length(new_candi)){
  print(i)
  M5[[i]] = glm(dta$event ~  dta$SWE_value + dta$SLd_value +dta$WSavg_value + dta$Tair_value + dta[[new_candi[i]]] )
}

sM5 = data.table(new_candi, aic = lapply(M5, AIC), lapply(M5, function(x)summary(x)$coe[2,3]))
sM5[, aic := unlist(aic)]
sM5[, V3 := unlist(V3)]
sM5[,abs := abs(aic)]

M6 = list()
candi = candi[candi!='WSavg_value3'] #eliminate already chosen predictor

#Rain_Ta_value5
nn = length(candi)
new_candi = candi[6:nn]
for (i in 1:length(new_candi)){
  print(i)
  M6[[i]] = glm(dta$event ~ dta$SWE_value + dta$SLd_value +dta$WSavg_value + dta$Tair_value  +dta$WSavg_value3 + dta[[new_candi[i]]] )
}
# best predictors for LOWEST AIC right now 

sM6 = data.table(new_candi, aic = lapply(M6, AIC), lapply(M6, function(x)summary(x)$coe[2,3]))
sM6[, aic := unlist(aic)]
sM6[, V3 := unlist(V3)]
sM6[,abs := abs(aic)]
g1bboot <- glm(formula = event ~ SWE_value + SLd_value +WSavg_value + Tair_value + WSavg_value3 + P_value, data =glm_data_lbou_W_d, family = binomial)
summary(g1bboot)
lbou_w_d <- varImp(g1bboot, scale = TRUE)

tempVar <- rownames(lbou_w_d)

lbou_w_d <- data.table (lbou_w_d)
lbou_w_d$variable <- tempVar
lbou_w_d[variable == "P_value3", variable:= "P"]
lbou_w_d[variable == "Tair_value", variable:= "Tair"]
lbou_w_d[variable == "WSavg_value", variable:= "WSavg"]
lbou_w_d[variable == "SLd_value", variable:= "SLd"]
lbou_w_d[variable == "SWE_value", variable:= "SWE"]
lbou_w_d[, type := "lbou_w_d"]

saveRDS(lbou_w_d, "./data/varImp_W_LBOU_all.rds")
readRDS ("./data/varImp_W_LBOU_all.rds")