library(data.table)
library(readr)
library(zoo)
library (reshape2)
library (caret)


#WET AVALANCHES glm
adcast_C$SD_value <-  adcast_C$SD_value3 <- adcast_C$SD_value5 <- adcast_C$SD_value10 <- NULL
adcast_C$Rain_Tw_value  <- adcast_C$Rain_Tw_value3  <- adcast_C$Rain_Tw_value5  <- adcast_C$Rain_Tw_value10 <-NULL
adcast_C$Rain_Ta_value <- adcast_C$Rain_Ta_value3 <- adcast_C$Rain_Ta_value5 <- adcast_C$Rain_Ta_value10 <-NULL
adcast_C$H_value <- adcast_C$H_value3 <- adcast_C$H_value5 <- adcast_C$H_value10 <-NULL
glm_data_lbou_C2 <- adcast_C[stat == "LBOU"]
glm_data_lbou_W_C2 <- glm_data_lbou_d[CAW == "warm"]
glm_data_lbou_C_C2 <- glm_data_lbou_d[CAW == "cold"]

dta = data.table(glm_data_lbou_W_C2)
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
#Rain_Ta_value3
#	P_value

#g1 <- glm(formula = event ~ SWE_value + SLd_value +WSavg_value+ Rain_Ta_value + Rain_Ta_value5, data =glm_data_lbou_W_d, family = binomial)
summary (g1)
#g1a <- glm(formula = event ~ SWE_value + SLd_value +WSavg_value+ H_value + Tair_value5, data =glm_data_lbou_W_d, family = binomial)
summary (g1a)
g2b <- glm(formula = event ~ SWE_value + SLd_value +WSavg_value + Tair_value + WSavg_value3 + P_value, data =glm_data_lbou_W_C2, family = binomial)
#summary (g1a)
summary (g2b)
install.packages("caret")
library (caret)
require(graphics)
p1<- predict(g1, newdata = NULL, type="response", se.fit = FALSE, na.action = na.pass)

lbou_w_C2 <- varImp(g2b, scale = TRUE)

tempVar <- rownames(lbou_w_C2)

lbou_w_C2 <- data.table (lbou_w_C2)
lbou_w_C2$variable <- tempVar
lbou_w_C2[variable == "P_value", variable:= "P"]
lbou_w_C2[variable == "Tair_value", variable:= "Tair"]
lbou_w_C2[variable == "WSavg_value3", variable:= "WSavg 3"]
lbou_w_C2[variable == "WSavg_value", variable:= "WSavg"]
lbou_w_C2[variable == "SLd_value", variable:= "SLd"]
lbou_w_C2[variable == "SWE_value", variable:= "SWE"]
lbou_w_C2[, type := "lbou_w_C2"]

saveRDS(lbou_w_C2, "./data/varImp_W_LBOU_C2_daily.rds")
readRDS ("./data/varImp_W_LBOU_C2_daily.rds")

#COLD WET EVENTS
dta = data.table(glm_data_lbou_C_C2)
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
candi = candi[candi!='SWE_value10']
nn = length(candi)
new_candi = candi[6:nn]
for (i in 1:length(new_candi)){
  print(i)
  M2[[i]] = glm(dta$event ~ dta$SWE_value10 + dta[[new_candi[i]]] )
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
  M3[[i]] = glm(dta$event ~ dta$SWE_value10 + dta$SLd_value + dta[[new_candi[i]]] )#building of already selected variables and others
}

sM3 = data.table(new_candi, aic = lapply(M3, AIC), lapply(M3, function(x)summary(x)$coe[2,3]))# i am not sure what position is coe [3,4]
sM3[, aic := unlist(aic)]
sM3[, V3 := unlist(V3)]
sM3[,abs := abs(aic)]


M4 = list()
candi = candi[candi!='Tair_value'] # eliminate already chosen predictor

nn = length(candi)
new_candi = candi[6:nn]
for (i in 1:length(new_candi)){
  print(i)
  M4[[i]] = glm(dta$event ~ dta$SWE_value10 + dta$SLd_value +dta$Tair_value + dta[[new_candi[i]]] )
}

sM4 = data.table(new_candi, aic = lapply(M4, AIC), lapply(M4, function(x)summary(x)$coe[2,3]))
sM4[, aic := unlist(aic)]
sM4[, V3 := unlist(V3)]
sM4[,abs := abs(aic)]

M5 = list()

candi = candi[candi!='SLd_value3'] #eliminate already chosen predictor
#Rain_Ta_value
nn = length(candi)
new_candi = candi[6:nn]
for (i in 1:length(new_candi)){
  print(i)
  M5[[i]] = glm(dta$event ~  dta$SWE_value10 + dta$SLd_value + dta$Tair_value+dta$SLd_value3 + dta[[new_candi[i]]] )
}

sM5 = data.table(new_candi, aic = lapply(M5, AIC), lapply(M5, function(x)summary(x)$coe[2,3]))
sM5[, aic := unlist(aic)]
sM5[, V3 := unlist(V3)]
sM5[,abs := abs(aic)]

M6 = list()
candi = candi[candi!='SLd_value5'] #eliminate already chosen predictor

#Rain_Ta_value5
nn = length(candi)
new_candi = candi[6:nn]
for (i in 1:length(new_candi)){
  print(i)
  M6[[i]] = glm(dta$event ~ dta$SWE_value10 + dta$SLd_value + dta$Tair_value+dta$SLd_value3  +dta$SLd_value5 + dta[[new_candi[i]]] )
}
# best predictors for LOWEST AIC right now 

sM6 = data.table(new_candi, aic = lapply(M6, AIC), lapply(M6, function(x)summary(x)$coe[2,3]))
sM6[, aic := unlist(aic)]
sM6[, V3 := unlist(V3)]
sM6[,abs := abs(aic)]
#SLd_value5

g2 <- glm(formula = event ~ SWE_value10 + SLd_value + Tair_value + SLd_value3 + SLd_value5, data =glm_data_lbou_C_C2, family = binomial)
summary (g2)
lbou_C_C2 <- varImp(g2, scale = TRUE)

tempVar <- rownames(lbou_C_C2)

lbou_C_C2 <- data.table (lbou_C_C2)
lbou_C_C2$variable <- tempVar
lbou_C_C2[variable == "SLd_value10", variable:= "SLd 10"]
lbou_C_C2[variable == "Tair_value", variable:= "Tair"]
lbou_C_C2[variable == "SLd_value5", variable:= "SLd 5"]
lbou_C_C2[variable == "SLd_value3", variable:= "SLd 3"]
lbou_C_C2[variable == "SWE_value10", variable:= "SWE 10"]
lbou_C_C2[, type := "lbou_C_C2"]

saveRDS(lbou_C_C2, "./data/varImp_C_LBOU_C2.rds")
readRDS ("./data/varImp_C_LBOU_C2.rds")
library(label.r)
library (ggplot2)
glm_lbou_w_c_C2_daily <- rbind(lbou_w_C2, lbou_C_C2)
glm_lbou_w_c_C2_daily <- data.table(glm_lbou_w_c_C2_daily)
ggplot(glm_lbou_w_c_C2_daily)+
  geom_point(aes(x = Overall, y = variable, color = variable))+
  facet_wrap(~ type, scales = "free")+
label.r(title = paste0(y = "variable", x = "percentage %"))
          
          