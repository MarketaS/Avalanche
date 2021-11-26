library(data.table)
library(readr)
library(ggplot2)
library(lubridate)
library(writexl)
library(dplyr)  
library(zoo)

setwd ("c:/Users/marketa.souckova/Documents/laviny/")

adcast$Tdiff_value <-  adcast$Tdiff_value24 <- adcast$Tdiff_value48 <- adcast$Tdiff_value72 <- adcast$Tdiff_value96  <- adcast$Tdiff_value120 <- adcast$Tdiff_value144 <- NULL
glm_data_lbou <- adcast[stat == "LBOU"]
glm_data_lbou_W <- glm_data_lbou[CAW == "warm"]
glm_data_lbou_C <- glm_data_lbou[CAW == "cold"]


dta = data.table(glm_data_lbou_W)
dta = dta[year(DATE2) %in% 2004:2015]

candi = names(dta)[-1]
nn = length(candi)
new_candi = candi[7:nn]
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
candi = candi[candi!='SVH_value72']
nn = length(candi)
new_candi = candi[7:nn]
for (i in 1:length(new_candi)){
  print(i)
  M2[[i]] = glm(dta$event ~ dta$SVH_value72 + dta[[new_candi[i]]] )
}

sM2 = data.table(new_candi, aic = lapply(M2, AIC), lapply(M2, function(x)summary(x)$coe[2,3]))
sM2[, aic := unlist(aic)]
sM2[, V3 := unlist(V3)]
sM2[,abs := abs(aic)]


M3 = list()
candi = candi[candi!='Fmax_value144']# exclude already selected variable


nn = length(candi)
new_candi = candi[7:nn]
for (i in 1:length(new_candi)){
  print(i)
  M3[[i]] = glm(dta$event ~ dta$SVH_value72 + dta$Fmax_value144 + dta[[new_candi[i]]] )#building of already selected variables and others
}

sM3 = data.table(new_candi, aic = lapply(M3, AIC), lapply(M3, function(x)summary(x)$coe[2,3]))# i am not sure what position is coe [3,4]
sM3[, aic := unlist(aic)]
sM3[, V3 := unlist(V3)]
sM3[,abs := abs(aic)]


M4 = list()
candi = candi[candi!='SRA1H_value48'] # eliminate already chosen predictor

nn = length(candi)
new_candi = candi[7:nn]
for (i in 1:length(new_candi)){
  print(i)
  M4[[i]] = glm(dta$event ~ dta$SVH_value72 + dta$Fmax_value144 + dta$SRA1H_value48 + dta[[new_candi[i]]] )
}

sM4 = data.table(new_candi, aic = lapply(M4, AIC), lapply(M4, function(x)summary(x)$coe[2,3]))
sM4[, aic := unlist(aic)]
sM4[, V3 := unlist(V3)]
sM4[,abs := abs(aic)]


M5 = list()
candi = candi[candi!='SRA1H_value24']


nn = length(candi)
new_candi = candi[7:nn]
for (i in 1:length(new_candi)){
  print(i)
  M5[[i]] = glm(dta$event ~ dta$SVH_value72 + dta$Fmax_value144 + dta$SRA1H_value48 + dta$SRA1H_value24 + dta[[new_candi[i]]] )
}

sM5 = data.table(new_candi, aic = lapply(M5, AIC), lapply(M5, function(x)summary(x)$coe[2,3]))
sM5[, aic := unlist(aic)]
sM5[, V3 := unlist(V3)]
sM5[,abs := abs(aic)]


M6 = list()

candi = candi[candi!='T_value24'] #eliminate already chosen predictor

nn = length(candi)
new_candi = candi[7:nn]
for (i in 1:length(new_candi)){
  print(i)
  M6[[i]] = glm(dta$event ~ dta$SVH_value72 + dta$Fmax_value144 + dta$SRA1H_value48 + dta$SRA1H_value24  + dta$T_value24+ dta[[new_candi[i]]] )
}# best predictors for LOWEST AIC right now - dont know how to incorporate significant p value - right now I have mostly 0 p value

sM6 = data.table(new_candi, aic = lapply(M6, AIC), lapply(M6, function(x)summary(x)$coe[2,3]))
sM6[, aic := unlist(aic)]
sM6[, V3 := unlist(V3)]
sM6[,abs := abs(aic)]

#T_value

all_aic <- list(sm = sM, sm2 = sM2, sm3 = sM3, sm4 = sM4, sm5 = sM5, sm6 = sM6)
saveRDS(all_aic, "./data/all_aic_boot.rds")
all_aic <- readRDS(file = "data/all_aic_boot.rds")

g5 <- glm(formula = event ~ SVH_value72 + Fmax_value144 + SRA1H_value48 + SRA1H_value24  + T_value24, data =glm_data_lbou_W, family = binomial)
summary (g5)

varImp(g5, scale = TRUE)

lbou_w_boot <- varImp(g5, scale = TRUE)

tempVar <- rownames(lbou_w_boot)

lbou_w_boot <- data.table (lbou_w_boot)
lbou_w_boot$variable <- tempVar

lbou_w_boot[variable == "Fmax_value144", variable:= "Fmax 144"]
lbou_w_boot[variable == "T_value24", variable:= "T 24"]
lbou_w_boot[variable == "SRA1H_value24", variable:= "P 24"]
lbou_w_boot[variable == "SVH_value72", variable:= "SWE 72"]
lbou_w_boot[variable == "SRA1H_value48", variable:= "SWE 48"]
lbou_w_boot[, type := "lbou_w_boot"]

saveRDS(lbou_w_boot, "./data/varImp_W_LBOU_boot.rds")
readRDS("./data/varImp_W_LBOU_boot.rds")
# deletion correlated values 
#adcast$SCE_value <- adcast$SCE_value24 <-NULL