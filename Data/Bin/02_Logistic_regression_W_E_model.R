library(data.table)
library(readr)
library(zoo)
library (reshape2)
library (caret)

aval_melt_total <- readRDS(file = "./my_data/lbou_lucb_data_new.rds")

adcast <- dcast.data.table(aval_melt_total, DATE2 + ID + PLOT + event + CAW + stat + event  ~ var_name, value.var = 'value')
#adcast <- readRDS("C:/Users/marketa.souckova/OneDrive - CZU v Praze/R/Krkonose/Rcode/RDS/adcast.rds")# nahr?t na One Drive
#adcast <- readRDS("~/laviny/data/adcast.rds")

setwd ("c:/Users/marketa.souckova/Documents/laviny/")
########### GLM LBOU #########################

adcast$Tdiff_value <-  adcast$Tdiff_value24 <- adcast$Tdiff_value48 <- adcast$Tdiff_value72 <- adcast$Tdiff_value96  <- adcast$Tdiff_value120 <- adcast$Tdiff_value144 <- NULL
glm_data_lbou <- adcast[stat == "LBOU"]
glm_data_lbou_W <- glm_data_lbou[CAW == "warm"]
glm_data_lbou_C <- glm_data_lbou[CAW == "cold"]

# selection from all available variables the best predictors for warm events
dta = data.table(glm_data_lbou_W)
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
candi = candi[candi!='SVH_value']
nn = length(candi)
new_candi = candi[7:nn]
for (i in 1:length(new_candi)){
  print(i)
  M2[[i]] = glm(dta$event ~ dta$SVH_value + dta[[new_candi[i]]] )
}

sM2 = data.table(new_candi, aic = lapply(M2, AIC), lapply(M2, function(x)summary(x)$coe[2,3]))
sM2[, aic := unlist(aic)]
sM2[, V3 := unlist(V3)]
sM2[,abs := abs(aic)]

M3 = list()
# exclude already selected variable
candi = candi[candi!='H_value144']

nn = length(candi)
new_candi = candi[7:nn]
for (i in 1:length(new_candi)){
  print(i)
  M3[[i]] = glm(dta$event ~ dta$SVH_value + dta$H_value144 + dta[[new_candi[i]]] )#building of already selected variables and others
}

sM3 = data.table(new_candi, aic = lapply(M3, AIC), lapply(M3, function(x)summary(x)$coe[2,3]))# i am not sure what position is coe [3,4]
sM3[, aic := unlist(aic)]
sM3[, V3 := unlist(V3)]
sM3[,abs := abs(aic)]
#write_xlsx(sM3, 'sM3.xlsx')

M4 = list()
candi = candi[candi!='T_value48'] # eliminate already chosen predictor

nn = length(candi)
new_candi = candi[7:nn]
for (i in 1:length(new_candi)){
  print(i)
  M4[[i]] = glm(dta$event ~ dta$SVH_value + dta$H_value144 +dta$T_value48 + dta[[new_candi[i]]] )
}

sM4 = data.table(new_candi, aic = lapply(M4, AIC), lapply(M4, function(x)summary(x)$coe[2,3]))
sM4[, aic := unlist(aic)]
sM4[, V3 := unlist(V3)]
sM4[,abs := abs(aic)]
#write_xlsx(sM4, 'sM4.xlsx')

M5 = list()

candi = candi[candi!='SRA1H_value48'] #eliminate already chosen predictor

nn = length(candi)
new_candi = candi[7:nn]
for (i in 1:length(new_candi)){
  print(i)
  M5[[i]] = glm(dta$event ~  dta$SVH_value + dta$H_value144 +dta$T_value48 + dta$SRA1H_value48 + dta[[new_candi[i]]] )
}

sM5 = data.table(new_candi, aic = lapply(M5, AIC), lapply(M5, function(x)summary(x)$coe[2,3]))
sM5[, aic := unlist(aic)]
sM5[, V3 := unlist(V3)]
sM5[,abs := abs(aic)]
#write_xlsx(sM5, 'sM5.xlsx')

M6 = list()
candi = candi[candi!='SVH_value120'] #eliminate already chosen predictor

nn = length(candi)
new_candi = candi[7:nn]
for (i in 1:length(new_candi)){
  print(i)
  M6[[i]] = glm(dta$event ~  dta$SVH_value + dta$H_value144 +dta$T_value48 + dta$SRA1H_value48 +dta$SVH_value120 + dta[[new_candi[i]]] )
}
# best predictors for LOWEST AIC right now 

sM6 = data.table(new_candi, aic = lapply(M6, AIC), lapply(M6, function(x)summary(x)$coe[2,3]))
sM6[, aic := unlist(aic)]
sM6[, V3 := unlist(V3)]
sM6[,abs := abs(aic)]
#write_xlsx(sM6, 'sM6.xlsx') T_value72 is the 6th best predictor


all_aic <- list(sm = sM, sm2 = sM2, sm3 = sM3, sm4 = sM4, sm5 = sM5, sm6 = sM6)
saveRDS(all_aic, "./data/all_aic_W.rds")
all_aic <- readRDS(file = "data/all_aic_W.rds")

g1 <- glm(formula = event ~ SVH_value + H_value144 +T_value48 + SRA1H_value48 +SVH_value120, data =glm_data_lbou_W, family = binomial)
summary (g1)

install.packages("caret")
library (caret)
require(graphics)
p1<- predict(g1, newdata = NULL, type="response", se.fit = FALSE, na.action = na.pass)

lbou_w <- varImp(g1, scale = TRUE)

tempVar <- rownames(lbou_w)

lbou_w <- data.table (lbou_w)
lbou_w$variable <- tempVar

lbou_w[variable == "H_value144", variable:= "H 144"]
lbou_w[variable == "T_value48", variable:= "T 48"]
lbou_w[variable == "SRA1H_value48", variable:= "P 48"]
lbou_w[variable == "SVH_value120", variable:= "SWE 120"]
lbou_w[variable == "SVH_value", variable:= "SWE"]
lbou_w[, type := "lbou_w"]

saveRDS(lbou_w, "./data/varImp_W_LBOU.rds")
readRDS ("./data/varImp_W_LBOU.rds")

library("randomForest")
library("DALEX")

install.packages("ResourceSelection")
library(ResourceSelection)

hoslem.test(g1$y, fitted(g1))

# selection from all available variables the best predictors for cold events
dta = data.table (glm_data_lbou_C)
candi = names(dta)[-1]
nn = length(candi)
new_candi = candi[7:nn]
M = list()
for (i in 1:length(new_candi)){
  print(i)
  M[[i]] = glm(dta$event ~ dta[[new_candi[i]]])# the  predictor
}

sM_C = data.table(new_candi, aic = lapply(M, AIC), lapply(M, function(x)summary(x)$coe[2,3] ))
sM_C[, aic := unlist(aic)]
sM_C[, V3 := unlist(V3)]
sM_C[,abs := abs(aic)]

M2 = list()
candi = candi[candi!='SVH_value']

nn = length(candi)
new_candi = candi[7:nn]
for (i in 1:length(new_candi)){
  M2[[i]] = glm(dta$event ~ dta$SVH_value + dta[[new_candi[i]]] )
}

sM2_C = data.table(new_candi, aic = lapply(M2, AIC), lapply(M2, function(x)summary(x)$coe[2,3] ))
sM2_C[, aic := unlist(aic)]
sM2_C[, V3 := unlist(V3)]
sM2_C[,abs := abs(aic)]


M3 = list()
candi = candi[candi!='H_value120']
nn = length(candi)
new_candi = candi[7:nn]
for (i in 1:length(new_candi)){
  M3[[i]] = glm(dta$event ~ dta$SVH_value + dta$H_value120 + dta[[new_candi[i]]] )
}

sM3_C = data.table(new_candi, aic = lapply(M3, AIC), lapply(M3, function(x)summary(x)$coe[2,3] ))
sM3_C[, aic := unlist(aic)]
sM3_C[, V3 := unlist(V3)]
sM3_C[,abs := abs(aic)]

M4 = list()
candi = candi[candi!='SSV1H_value']
nn = length(candi)
new_candi = candi[7:nn]
for (i in 1:length(new_candi)){
  M4[[i]] = lm(dta$event ~ dta$SVH_value + dta$H_value120 + dta$SSV1H_value + dta[[new_candi[i]]] )
}
sM4_C = data.table(new_candi, aic = lapply(M4, AIC), lapply(M4, function(x)summary(x)$coe[2,4]))
sM4_C[, aic := unlist(aic)]
sM4_C[, V3 := unlist(V3)]
sM4_C[,abs := abs(aic)]


M5 = list()
candi = candi[candi!='H_value24']
nn = length(candi)
new_candi = candi[7:nn]
for (i in 1:length(new_candi)){
  M5[[i]] = lm(dta$event ~ dta$SVH_value + dta$H_value120 + dta$SSV1H_value + dta$H_value24 + dta[[new_candi[i]]] )
}
sM5_C = data.table(new_candi, aic = lapply(M5, AIC), lapply(M5, function(x)summary(x)$coe[2,4]))
sM5_C[, aic := unlist(aic)]
sM5_C[, V3 := unlist(V3)]
sM5_C[,abs := abs(aic)]


M6 = list()
candi = candi[candi!='SNO_value']
nn = length(candi)
new_candi = candi[7:nn]
for (i in 1:length(new_candi)){
  M6[[i]] = lm(dta$event ~ dta$SVH_value + dta$H_value120 + dta$SSV1H_value + dta$H_value24 + dta$SNO_value + dta[[new_candi[i]]] )
}
sM6_C = data.table(new_candi, aic = lapply(M6, AIC), lapply(M6, function(x)summary(x)$coe[2,4]))
sM6_C[, aic := unlist(aic)]
sM6_C[, V3 := unlist(V3)]
sM6_C[,abs := abs(aic)]
#T_value
all_aic_C <- list(sm = sM_C, sm2 = sM2_C, sm3 = sM3_C, sm4 = sM4_C, sm5 = sM5_C)
saveRDS(all_aic_C, "./data/all_aic_C_LBOU.rds")
all_aic_C <- readRDS(file = "data/all_aic_C_LBOU.rds")
#  glm model of COLD lbou

g2 <- glm(formula = event ~ SVH_value + H_value120 + SSV1H_value + H_value24 + SNO_value , data =glm_data_lbou_C, family = binomial)
p2<- predict(g2, newdata = NULL, type="response", se.fit = FALSE, na.action = na.pass)
summary (g2)
lbou_c <-varImp(g2, scale = TRUE)

tempVar <- rownames(lbou_c)
lbou_c <- data.table (lbou_c)
lbou_c$variable <- tempVar

lbou_c[variable == "H_value120", variable:= "H 120"]
lbou_c[variable == "H_value24", variable:= "H 24"]
lbou_c[variable == "SSV1H_value", variable:= "SLd "]
lbou_c[variable == "SNO_value", variable:= "NSS "]
lbou_c[variable == "SVH_value", variable:= "SWE"]

lbou_c[, type := "lbou_c"]

saveRDS(lbou_c, "./data/varImp_C_LBOU.rds")
hoslem.test(g2$y, fitted(g2))

########### GLM LUCB #########################
#adcast$Tdiff_value <-  adcast$Tdiff_value24 <- adcast$Tdiff_value48 <- adcast$Tdiff_value72 <- adcast$Tdiff_value96  <- adcast$Tdiff_value120 <- adcast$Tdiff_value144 <- NULL


glm_data_lucb <- adcast[stat == "LUCB"]
glm_data_lucb_W <- glm_data_lucb[CAW == "warm"]
glm_data_lucb_C <- glm_data_lucb[CAW == "cold"]

dta = data.table(glm_data_lucb_W)
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
candi = candi[candi!='SVH_value']
nn = length(candi)
new_candi = candi[7:nn]
for (i in 1:length(new_candi)){
  print(i)
  M2[[i]] = glm(dta$event ~ dta$SVH_value + dta[[new_candi[i]]] )
}

sM2 = data.table(new_candi, aic = lapply(M2, AIC), lapply(M2, function(x)summary(x)$coe[2,3]))
sM2[, aic := unlist(aic)]
sM2[, V3 := unlist(V3)]
sM2[,abs := abs(aic)]

M3 = list()
# exclude already selected variable
candi = candi[candi!='H_value']

nn = length(candi)
new_candi = candi[7:nn]
for (i in 1:length(new_candi)){
  print(i)
  M3[[i]] = glm(dta$event ~ dta$SVH_value + dta$H_value + dta[[new_candi[i]]] )#building of already selected variables and others
}

sM3 = data.table(new_candi, aic = lapply(M3, AIC), lapply(M3, function(x)summary(x)$coe[2,3]))# i am not sure what position is coe [3,4]
sM3[, aic := unlist(aic)]
sM3[, V3 := unlist(V3)]
sM3[,abs := abs(aic)]
#write_xlsx(sM3, 'sM3.xlsx')

M4 = list()
candi = candi[candi!='SSV1H_value'] # eliminate already chosen predictor

nn = length(candi)
new_candi = candi[7:nn]
for (i in 1:length(new_candi)){
  print(i)
  M4[[i]] = glm(dta$event ~ dta$SVH_value +  dta$H_value + dta$SSV1H_value + dta[[new_candi[i]]] )
}

sM4 = data.table(new_candi, aic = lapply(M4, AIC), lapply(M4, function(x)summary(x)$coe[2,3]))
sM4[, aic := unlist(aic)]
sM4[, V3 := unlist(V3)]
sM4[,abs := abs(aic)]
#write_xlsx(sM4, 'sM4.xlsx')

M5 = list()

candi = candi[candi!='SRA1H_value'] #eliminate already chosen predictor

nn = length(candi)
new_candi = candi[7:nn]
for (i in 1:length(new_candi)){
  print(i)
  M5[[i]] = glm(dta$event ~ dta$SVH_value +  dta$H_value + dta$SSV1H_value  + dta$SRA1H_value + dta[[new_candi[i]]] )
}

sM5 = data.table(new_candi, aic = lapply(M5, AIC), lapply(M5, function(x)summary(x)$coe[2,3]))
sM5[, aic := unlist(aic)]
sM5[, V3 := unlist(V3)]
sM5[,abs := abs(aic)]

M6 = list()
candi = candi[candi!='T_value'] #eliminate already chosen predictor

nn = length(candi)
new_candi = candi[7:nn]
for (i in 1:length(new_candi)){
  print(i)
  M6[[i]] = glm(dta$event ~ dta$SVH_value +  dta$H_value + dta$SSV1H_value  + dta$SRA1H_value + dta$T_value + dta[[new_candi[i]]] )
}
# best predictors for LOWEST AIC right now 

sM6 = data.table(new_candi, aic = lapply(M6, AIC), lapply(M6, function(x)summary(x)$coe[2,3]))
sM6[, aic := unlist(aic)]
sM6[, V3 := unlist(V3)]
sM6[,abs := abs(aic)]
#SRA1H_value48
all_aic <- list(sm = sM, sm2 = sM2, sm3 = sM3, sm4 = sM4, sm5 = sM5, sm6 = sM6)
saveRDS(all_aic, "./data/all_aic_LUCB_W.rds")
all_aic_LUCB <- readRDS(file = "data/all_aic_LUCB_W.rds")

g3 <- glm(formula = event ~ SVH_value+ H_value+ SSV1H_value + SRA1H_value + T_value, data =glm_data_lucb_W, family = binomial)
summary (g3)

p3<- predict(g3, newdata = NULL, type="response", se.fit = FALSE, na.action = na.pass)

lucb_w <- varImp(g3, scale = TRUE)

tempVar <- rownames(lucb_w)
lucb_w <- data.table(lucb_w)
lucb_w$variable <- tempVar

#lucb_w[var == "SCE", var:= "SD"] 
lucb_w[variable == "SVH_value", variable:= "SWE "] 
#lucb_w[var == "SNO", var:= "NSS"] 
#lucb_w[var == "D", var:= "WD"] 
#lucb_w[var == "Fprum", var:= "WSavg"]
#lucb_w[var == "Fmax", var:= "WSmax"]
lucb_w[variable == "SRA1H_value", variable:= "P "]
lucb_w[variable == "SSV1H_value", variable:= "SLd "]
lucb_w[variable == "T_value", variable:= "T "]
lucb_w[variable == "H_value", variable:= "H "]
lucb_w[, type := "lucb_w"]
saveRDS(lucb_w, "./data/varImp_W_LUCB.rds")


library(ResourceSelection)

hoslem.test(g3$y, fitted(g3))

# selection from all available variables the best predictors for cold events
dta = data.table (glm_data_lucb_C)
candi = names(dta)[-1]
nn = length(candi)
new_candi = candi[7:nn]
M = list()
for (i in 1:length(new_candi)){
  print(i)
  M[[i]] = glm(dta$event ~ dta[[new_candi[i]]])# the  predictor
}

sM_C = data.table(new_candi, aic = lapply(M, AIC), lapply(M, function(x)summary(x)$coe[2,3] ))
sM_C[, aic := unlist(aic)]
sM_C[, V3 := unlist(V3)]
sM_C[,abs := abs(aic)]

M2 = list()
candi = candi[candi!='SVH_value']

nn = length(candi)
new_candi = candi[7:nn]
for (i in 1:length(new_candi)){
  M2[[i]] = glm(dta$event ~ dta$SVH_value + dta[[new_candi[i]]] )
}

sM2_C = data.table(new_candi, aic = lapply(M2, AIC), lapply(M2, function(x)summary(x)$coe[2,3] ))
sM2_C[, aic := unlist(aic)]
sM2_C[, V3 := unlist(V3)]
sM2_C[,abs := abs(aic)]


M3 = list()
candi = candi[candi!='SSV1H_value']
nn = length(candi)
new_candi = candi[7:nn]
for (i in 1:length(new_candi)){
  M3[[i]] = glm(dta$event ~ dta$SVH_value + dta$SSV1H_value + dta[[new_candi[i]]] )
}

sM3_C = data.table(new_candi, aic = lapply(M3, AIC), lapply(M3, function(x)summary(x)$coe[2,3] ))
sM3_C[, aic := unlist(aic)]
sM3_C[, V3 := unlist(V3)]
sM3_C[,abs := abs(aic)]

M4 = list()
candi = candi[candi!='SRA1H_value']
nn = length(candi)
new_candi = candi[7:nn]
for (i in 1:length(new_candi)){
  M4[[i]] = lm(dta$event ~  dta$SVH_value + dta$SSV1H_value + dta$SRA1H_value + dta[[new_candi[i]]] )
}
sM4_C = data.table(new_candi, aic = lapply(M4, AIC), lapply(M4, function(x)summary(x)$coe[2,4]))
sM4_C[, aic := unlist(aic)]
sM4_C[, V3 := unlist(V3)]
sM4_C[,abs := abs(aic)]


M5 = list()
candi = candi[candi!='D_value']
nn = length(candi)
new_candi = candi[7:nn]
for (i in 1:length(new_candi)){
  M5[[i]] = lm(dta$event ~ dta$SVH_value + dta$SSV1H_value + dta$SRA1H_value + dta$D_value + dta[[new_candi[i]]] )
}
sM5_C = data.table(new_candi, aic = lapply(M5, AIC), lapply(M5, function(x)summary(x)$coe[2,4]))
sM5_C[, aic := unlist(aic)]
sM5_C[, V3 := unlist(V3)]
sM5_C[,abs := abs(aic)]

M6 = list()
candi = candi[candi!='SVH_value144']
nn = length(candi)
new_candi = candi[7:nn]
for (i in 1:length(new_candi)){
  M6[[i]] = lm(dta$event ~ dta$SVH_value + dta$SSV1H_value + dta$SRA1H_value + dta$D_value + dta$SVH_value144 +dta[[new_candi[i]]] )
}
sM6_C = data.table(new_candi, aic = lapply(M6, AIC), lapply(M6, function(x)summary(x)$coe[2,4]))
sM6_C[, aic := unlist(aic)]
sM6_C[, V3 := unlist(V3)]
sM6_C[,abs := abs(aic)]

#SVH_value120

all_aic_C <- list(sm = sM_C, sm2 = sM2_C, sm3 = sM3_C, sm4 = sM4_C, sm5 = sM5_C, sm6 = sM6_C )
saveRDS(all_aic_C, "./data/all_aic_C_LUCB.rds")
all_aic_C_LUCB <- readRDS(file = "data/all_aic_C_LUCB.rds")

g4 <- glm(formula = event ~ SVH_value + SSV1H_value + SRA1H_value + D_value + SVH_value144, data =glm_data_lucb_C, family = binomial)
summary (g4)

p4<- predict(g4, newdata = NULL, type="response", se.fit = FALSE, na.action = na.pass)

lucb_c <- varImp(g4, scale = TRUE)

tempVar <- rownames(lucb_c)
lucb_c <- data.table(lucb_c)

lucb_c$variable <- tempVar

lucb_c[variable == "SVH_value", variable:= "SWE "]
lucb_c[variable == "SRA1H_value", variable:= "P "]
lucb_c[variable == "SSV1H_value", variable:= "SLd "]
lucb_c[variable == "D_value", variable:= "WD "]
lucb_c[variable == "SVH_value144", variable:= "SWE 144"]


lucb_c[, type := "lucb_c"]

saveRDS(lucb_c, "./data/varImp_C_LUCB.rds")

library (ggplot2)

glm_lbou_lucb_w_c <- rbind(lbou_w, lbou_c,lucb_w, lucb_c)
glm_lbou_lucb_w_c <- data.table(glm_lbou_lucb_w_c)
ggplot(glm_lbou_lucb_w_c)+
  geom_point(aes(x = Overall, y = variable, color = variable))+
  label.r(title = paste0(y = "variable", x = "percentage %")+
  facet_wrap(~ type, scales = "free_y")
  
saveRDS(object = aval_melt_total, file = "C:/Users/marketa.souckova/Documents/laviny/glm_W_E_C_W_plot.rds")  
 

g_SCE_lbou <- glm(event ~ SCE_value + SCE_value24 + SCE_value48 + SCE_value72 + SCE_value96 + SCE_value120 + SCE_value144, data = glm_data_lbou, family = 'binomial')

g_T_lbou <- glm(event ~  T_value + T_value24 + T_value48 + T_value72 + T_value96 + T_value120 + T_value144, data = glm_data_lbou_W, family = 'binomial')

g_SRA1H_lbou <- glm(event ~ SRA1H_value +SRA1H_value24 + SRA1H_value48 + SRA1H_value72 + SRA1H_value96 + SRA1H_value120 + + SRA1H_value144, data = glm_data_lbou, family = 'binomial')

g_SVH_lbou <- glm(event ~ SVH_value + SVH_value24 + SVH_value48 + SVH_value72 + SVH_value96 + SVH_value120 + SVH_value144, data = glm_data_lbou_W, family = 'binomial')

g_SNO_lbou <- glm(event ~ SNO_value + SNO_value24 + SNO_value48 + SNO_value72 + SNO_value96 + SNO_value120 + SNO_value144, data = glm_data_lbou_W, family = 'binomial')

g_SSV1H_lbou <- glm(event ~ SSV1H_value + SSV1H_value24 + SSV1H_value48 + SSV1H_value72 + SSV1H_value96 + SSV1H_value120 + SSV1H_value144, data = glm_data_lbou_W, family = 'binomial')


g_SCE_lucb <- glm(event ~ SCE_value + SCE_value24 + SCE_value48 + SCE_value72 + SCE_value96 + SCE_value120 + SCE_value144, data = glm_data_lucb, family = 'binomial')

g_T_lucb <- glm(event ~  T_value + T_value24 + T_value48 + T_value72 + T_value96 + T_value120 + T_value144, data = glm_data_lucb_W, family = 'binomial')

g_SRA1H_lucb <- glm(event ~ SRA1H_value +SRA1H_value24 + SRA1H_value48 + SRA1H_value72 + SRA1H_value96 + SRA1H_value120 + + SRA1H_value144, data = glm_data_lucb, family = 'binomial')

g_SVH_lucb <- glm(event ~ SVH_value + SVH_value24 + SVH_value48 + SVH_value72 + SVH_value96 + SVH_value120 + SVH_value144, data = glm_data_lucb_W, family = 'binomial')

g_SNO_lucb <- glm(event ~ SNO_value + SNO_value24 + SNO_value48 + SNO_value72 + SNO_value96 + SNO_value120 + SNO_value144, data = glm_data_lucb_W, family = 'binomial')

g_SSV1H_lucb <- glm(event ~ SSV1H_value + SSV1H_value24 + SSV1H_value48 + SSV1H_value72 + SSV1H_value96 + SSV1H_value120 + SSV1H_value144, data = glm_data_lucb_W, family = 'binomial')