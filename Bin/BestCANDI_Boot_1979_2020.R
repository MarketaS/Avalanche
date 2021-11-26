library(data.table)
library(readr)
library(ggplot2)
library(lubridate)
library(writexl)
library(dplyr)  
library(zoo)
library(caret)
setwd ("C:/Users/souckovamarketa/OneDrive - CZU v Praze/R/Krkonose/Rcode")
source("fun_find_best_vars.R")

setwd ("C:/Users/souckovamarketa/OneDrive - CZU v Praze/R/Avalanche/Bin/daily/")
adcast_A_daily <- readRDS("C:/Users/souckovamarketa/OneDrive - CZU v Praze/R/Avalanche/Bin/daily/adcast_A_daily_upd.rds")
#adcast_A_daily <- readRDS("./adcast_A_daily_upd.rds")
adcast_C_daily <- readRDS("./adcast_C_daily_upd.rds")
adcast_A_daily <- readRDS("./adcast_A_daily.rds")
#adcast_C_daily <- readRDS("./adcast_C_daily_upd.rds")

#delete multicolinear variables
adcast_C_daily$SWE_value <- adcast_C_daily$SWE_value3 <-adcast_C_daily$SWE_value6 <-NULL
adcast_C_daily$Rain_Ta_value <-adcast_C_daily$Rain_Ta_value3  <-adcast_C_daily$Rain_Ta_value6 <-NULL
adcast_C_daily$Rain_Tw_value <-adcast_C_daily$Rain_Tw_value3  <-adcast_C_daily$Rain_Tw_value6 <-NULL
#adcast_C_daily$Rain_Ta_sum6 <-adcast_C_daily$Rain_Ta_sum3  <-NULL
adcast_C_daily$H_value <- adcast_C_daily$H_value3 <-adcast_C_daily$H_value6 <-NULL
adcast_C_daily$Tair_value<-adcast_C_daily$Tair_value6<-NULL
adcast_C_daily$WD_value<-adcast_C_daily$WD_value6<-NULL
adcast_C_daily$WSavg_value<-adcast_C_daily$WSavg_value6<-NULL
adcast_C_daily$SLd_value<-adcast_C_daily$SLd_value6<-NULL
adcast_C_daily$SD_value3<-adcast_C_daily$SD_value6<-NULL
adcast_C_daily$P_value3<-adcast_C_daily$P_value6<-NULL

adcast_C_daily$NSS_value <-adcast_C_daily$NSS_value3 <-adcast_C_daily$NSS_value6<-NULL
adcast_C_daily$Tamp3<- adcast_C_daily$Tamp6 <-NULL

#delete multicolinear variables slab aval
adcast_A_daily$SWE_value <- adcast_A_daily$SWE_value3 <-adcast_A_daily$SWE_value6 <-NULL
adcast_A_daily$Rain_Ta_value <-adcast_A_daily$Rain_Ta_value3  <-adcast_A_daily$Rain_Ta_value6 <-NULL
adcast_A_daily$Rain_Tw_value <-adcast_A_daily$Rain_Tw_value3  <-adcast_A_daily$Rain_Tw_value6 <-NULL
#adcast_A_daily$Rain_Ta_sum6 <-adcast_A_daily$Rain_Ta_sum3  <-NULL
adcast_A_daily$H_value <- adcast_A_daily$H_value3 <-adcast_A_daily$H_value6 <-NULL
adcast_A_daily$Tair_value <- adcast_A_daily$Tair_value3<-adcast_A_daily$Tair_value6<-NULL
adcast_A_daily$NSS_value <-adcast_A_daily$NSS_value3 <-adcast_A_daily$NSS_value6<-NULL
adcast_A_daily$WD_value<- adcast_A_daily$WSavg_value <- adcast_A_daily$SLd_value <-adcast_A_daily$SD_value <-adcast_A_daily$P_value <-NULL
adcast_A_daily$Tamp3<- adcast_A_daily$Tamp6 <-NULL

glm_data_lbou_A <- adcast_A_daily#[stat == "LBOU"]
glm_data_lbou_C <- adcast_C_daily[stat == "LBOU"]
# Nad 500 for slab Aval
# Slab_EQ_0 <- adcast_A_daily[event == 0 & ID %in% sample(x = adcast_A_daily$ID, size = 830)]
# 
# Slab_EQ_1 <- adcast_A_daily[event == 1]
# glm_lbou_S_d <- rbind(Slab_EQ_0,Slab_EQ_1)
# 
# # Nad 500 for slab Aval
# Wet_EQ_0 <- adcast_C_daily[event == 0 & ID %in% sample(x = adcast_C_daily$ID, size = 186)]
# 
# Wet_EQ_1 <- adcast_C_daily[event == 1]
# glm_lbou_W_d <- rbind(Wet_EQ_0,Wet_EQ_1)

# glm_data_lbou_W_A <- glm_data_lbou_A[CAW == "warm"]
# saveRDS(glm_data_lbou_W_A, "./glm_data_lbou_W_A.rds")
# glm_data_lbou_C_A <- glm_data_lbou_A[CAW == "cold"]
# saveRDS(glm_data_lbou_C_A, "./glm_data_lbou_C_A.rds")
# 
# glm_data_lbou_W_C <- glm_data_lbou_C[CAW == "warm"]
# saveRDS(glm_data_lbou_W_C, "./glm_data_lbou_W_C.rds")
# glm_data_lbou_C_C <- glm_data_lbou_C[CAW == "cold"]
# saveRDS(glm_data_lbou_C_C, "./glm_data_lbou_C_C.rds")

period <- c(1979:1999, 2002:2020)
boot_period <- c(1979:1999, 2002:2020)
period <- c(2004:2020)
boot_period <- c(2010:2020)


new_candi <- names(glm_data_lbou_A)[-1:-3]
new_candi <- names(glm_data_lbou_C)[-1:-4]
# new_candi <- names(glm_lbou_S_d)[-1:-6]
# new_candi <- names(glm_lbou_W_d)[-1:-6]

# compare one variable
glm_data_lbou_C[, plot(SDdif6, event)]
# model
run_S <- find_best_vars(indata = glm_data_lbou_A, period = period, bootstrap = "no", boot_period = boot_period, new_candi = new_candi)

predictors <- run_S[1]
predict <- run_S[1]
for(i in 2:length(run_S) ){
  predictors <- paste(predictors, "+", run_S[i])
  predict<- paste0(predict, ",", run_S[i])
}
formula <- as.formula(paste("event", "~", predictors))

gS <- glm(formula = formula , data = glm_data_lbou_A, family = binomial)
gS <- glm(event ~ SD_value3 + NSSsum6 + WD_value3 + Tmax6, data = glm_data_lbou_A, family = binomial)
gSPRE <- predict(gS,glm_data_lbou_A[,.(SD_value3 , NSSsum6 , WD_value3 , Tmax6 )] , type="response")
plot(gSPRE)
plot(glm_data_lbou_A$event,gSPRE)
library (ModelMetrics)
gini(gS)[1]
summary (gS)

# Rows are correct outcome, columns are prediction with threshold 0.5
tab <- table(glm_data_lbou_A$event, gSPRE>= 0.1)
#ab <- table(gW, gWPRE>= 0.2)
tab   # Display the confusion matrix
accuracy.reg <- sum(diag(tab)) / sum(tab)
accuracy.reg  # Output accuracy

#random forest
library (randomForest)
gS <- randomForest(cevent ~ SD_value3 + NSSsum6 + WD_value3 + Tmax6, data = glm_lbou_S_Nad[month(Date)%in% c(2)& complete.cases(SD_value3 + NSSsum6 + WD_value3 + Tmax6)], family = binomial)

# DELETE Nad
glm_lbou_S_Nad =  glm_data_lbou_A
glm_lbou_S_Nad[, delete := FALSE]
nrow_nad <- nrow(glm_lbou_S_Nad[event == 0])
delete_this <- c(rep(TRUE, 6),FALSE, rep(TRUE, 6))

delete_vector <- rep(delete_this, ceiling(nrow_nad/length(delete_this)))
delete_vector <- delete_vector[1:nrow_nad]

glm_lbou_S_Nad[event == 0, delete := delete_vector]
glm_lbou_S_Nad <- glm_lbou_S_Nad[delete == FALSE]

run_S_Nad <- find_best_vars(indata = glm_lbou_S_Nad, period = period, bootstrap = "no", boot_period = boot_period, new_candi = new_candi)

predictors <- run_S_Nad[1]
predict <- run_S_Nad[1]
for(i in 2:length(run_S_Nad) ){
  predictors <- paste(predictors, "+", run_S_Nad[i])
  predict<- paste0(predict, ",", run_S_Nad[i])
}
formula <- as.formula(paste("event", "~", predictors))

glm_lbou_S_Nad[, cevent := factor(event)]
gS_Nad <- glm(formula = event ~ SD_value3 + NSSsum6 + WD_value3 + Tmax6 + SDdif2 + Tmin3 + month(Date), data = glm_lbou_S_Nad[complete.cases(SD_value3 + NSSsum6 + WD_value3 + Tmax6 + SDdif2 + Tmin3)], family = binomial)
summary(gS_Nad)
library (randomForest)
gS_Nad <- randomForest(cevent ~ SD_value3 + NSSsum6 + WD_value3 + Tmax6 + SDdif2 + Tmin3, data = glm_lbou_S_Nad[month(Date)%in% c(2)& complete.cases(SD_value3 + NSSsum6 + WD_value3 + Tmax6 + SDdif2 + Tmin3)], family = binomial)


gw_htest_out_Nad <- predict(gS_Nad, type="response")
plot(gw_htest_out_Nad)
plot(glm_lbou_S_Nad[complete.cases(SD_value3, WD_value3, NSSsum6, SDdif2, Tmin3, SLd_value6)]$event,gw_htest_out_Nad)

glm_lbou_S_Nad[Tmin6> -3, plot(P_value3, event)]

#slab Nad model
gS_Nad1<- glm(event ~  SD_value3 +  NSSsum6 +  SLd_value6,data = glm_lbou_S_Nad, family = binomial)
summary(gS_Nad)
varImp(gS_Nad, scale = TRUE)

#slab run delte Nad months

# glm_lbou_SJFM <- glm_data_lbou_A [month(Date) %in% c(1,2,3)]
# run_SJFM <- find_best_vars(indata = glm_lbou_SJFM, period = period, bootstrap = "no", boot_period = boot_period, new_candi = new_candi)
# 
# predictors <- run_SJFM[1]
# predict <- run_SJFM[1]
# for(i in 2:length(run_SJFM) ){
#   predictors <- paste(predictors, "+", run_SJFM[i])
#   predict<- paste0(predict, ",", run_SJFM[i])
# }
# formula <- as.formula(paste("event", "~", predictors))
# gS_SJFM <- glm(formula = formula , data = glm_lbou_SJFM, family = binomial)
# gw_SJFM_pred <- predict(gS_SJFM,glm_lbou_SJFM[,.(WD_value3 , SD_value3 , WSavg_value6 , SDdif2 , NSSsum3 , 
#                                                    SD_value6)] , type="response")
# plot(gw_SJFM_pred)
# plot(glm_lbou_SJFM$event,gw_SJFM_pred)
# 
# summary (gS_SJFM)

#Wet daily 1979-2020
run_W <- find_best_vars(indata = glm_data_lbou_C, period = period, bootstrap = "no", boot_period = boot_period, new_candi = new_candi)

predictors <- run_W[1]
predict <- run_W[1]
for(i in 2:length(run_W) ){
  predictors <- paste(predictors, "+", run_W[i])
  predict<- paste0(predict, ",", run_W[i])
}
formula <- as.formula(paste("event", "~", predictors))
gW <- glm(event ~ SD_value3 + SDdif6 + SLd_value3 + P_value3 + Tmin6, data = glm_data_lbou_C, family = binomial)
gW <- glm(formula = formula , data = glm_data_lbou_C, family = binomial)
gWPRE <- predict(gW,glm_data_lbou_C[,.(SD_value3 , SDdif6 , SLd_value3 , P_value3 , Tmin6 )] , type="response")
gWPRE <- predict(gW,glm_data_lbou_C, type="response")
plot(gWPRE)
plot(glm_data_lbou_C$event,gWPRE)
#gWPRE <-predict(gW, type="response")
#histogram
glm_data_lbou_C_res <- glm_data_lbou_C
glm_data_lbou_C_res[, predict := gWPRE]
ggplot(glm_data_lbou_C_res)+ geom_histogram(aes(x = predict, group = event), bins = 10)
ggplot(glm_data_lbou_C_res[event == 1])+ geom_histogram(aes(x = predict), bins = 10)
ggplot(glm_data_lbou_C_res)+ geom_histogram(aes(x = predict), bins = 5)+ facet_wrap(~event, scales = "free_y")
#lm_data_lbou_C + predict y values 

#Testing
require(caret)
gW_SD <- glm(formula = event ~  SD_value, data = glm_data_lbou_C, family = binomial)
gw_SD_pred <- predict(gW_SD,glm_data_lbou_C , type="response")
plot(glm_data_lbou_C$event,gw_SD_pred)
#confusionMatrix(gw_SD_pred < 0.1, glm_data_lbou_C$event)


gW_NSSsum6 <- glm(formula = event ~  NSSsum6, data = glm_data_lbou_C, family = binomial)
gw_NSSsum6_pred <- predict(gW_NSSsum6,glm_data_lbou_C , type="response")
plot(glm_data_lbou_C$event,gW_NSSsum6_pred)

glm_lbou_W_1 <- glm_data_lbou_C [month(Date) %in% 1]
run_W_J <- find_best_vars(indata = glm_lbou_W_1, period = period, bootstrap = "no", boot_period = boot_period, new_candi = new_candi)
predictors <- run_W_J[1]
predict <- run_W_J[1]
for(i in 2:length(run_W_J) ){
predictors <- paste(predictors, "+", run_W_J[i])
predict<- paste0(predict, ",", run_W_J[i])
}
formula <- as.formula(paste("event", "~", predictors))
gS_J <- glm(formula = formula , data = glm_lbou_W_1, family = binomial)
gw_J_pred <- predict(gS_J,glm_lbou_W_1[,.(SLd_value3 , SDdif6 , NSSsum6 , WD_value3 , P_value + 
                                            SDdif3)] , type="response")
# Rows are correct outcome, columns are prediction with threshold 0.5
tab <- table(glm_data_lbou_C$event, gw_SD_pred>= 0.1)
#ab <- table(gW, gWPRE>= 0.2)
tab   # Display the confusion matrix
accuracy.reg <- sum(diag(tab)) / sum(tab)
accuracy.reg  # Output accuracy

gS_tno_weights <- glm(formula = event ~  SD_value24, data = glm_lbou_S, family = binomial, control = list(trace=TRUE, maxit = 10, epsilon = 0.001))
gw_htest_out <- predict(gS_tno_weights,glm_lbou_S[,.(SD_value24)] , type="response")
plot(gw_htest_out)

gS_tno_weights <- glm(formula = event ~  NSS_value24, data = glm_lbou_S, family = binomial)
gw_htest_out <- predict(gS_tno_weights,glm_lbou_S[,.(NSS_value24)] , type="response")
plot(gw_htest_out)

gS_tno_weights <- glm(formula = event ~  SDdif48, data = glm_lbou_S, family = binomial)
gw_htest_out <- predict(gS_tno_weights,glm_lbou_S[,.(SDdif48)] , type="response")
plot(gw_htest_out)

gS_tno_weights <- glm(formula = event ~  NSSsum72, data = glm_lbou_S, family = binomial)
gw_htest_out <- predict(gS_tno_weights,glm_lbou_S[,.(NSSsum72)] , type="response")
plot(gw_htest_out)

gS_tno_weights <- glm(formula = event ~  Rain_Ta_sum72, data = glm_lbou_S, family = binomial)
gw_htest_out <- predict(gS_tno_weights,glm_lbou_S[,.(Rain_Ta_sum72)] , type="response")
plot(gw_htest_out)

gS_tno_weights <- glm(formula = event ~  SD_value72, data = glm_lbou_S, family = binomial)
gw_htest_out <- predict(gS_tno_weights,glm_lbou_S[,.(SD_value72)] , type="response")
plot(gw_htest_out)
library (ModelMetrics)
gini(gW)[1]
gini (gW, gWPRE)
summary (gW)

preddreg <- predict(dreg, newdata=test, type="response")

# Rows are correct outcome, columns are prediction with threshold 0.5
tab <- table(glm_data_lbou_C$event, gWPRE>= 0.1)
#ab <- table(gW, gWPRE>= 0.2)
tab   # Display the confusion matrix
accuracy.reg <- sum(diag(tab)) / sum(tab)
accuracy.reg  # Output accuracy


#Nad

glm_lbou_W_Nad =  glm_data_lbou_C
glm_lbou_W_Nad[, delete := FALSE]
nrow_nad <- nrow(glm_lbou_W_Nad[event == 0])
delete_this <- c(rep(TRUE, 6),FALSE, rep(TRUE, 6))

delete_vector <- rep(delete_this, ceiling(nrow_nad/length(delete_this)))
delete_vector <- delete_vector[1:nrow_nad]

glm_lbou_W_Nad[event == 0, delete := delete_vector]
glm_lbou_W_Nad <- glm_lbou_W_Nad[delete == FALSE]

run_W_Nad <- find_best_vars(indata = glm_lbou_W_Nad, period = period, bootstrap = "no", boot_period = boot_period, new_candi = new_candi)

predictors <- run_W_Nad[1]
predict <- run_W_Nad[1]
for(i in 2:length(run_W_Nad) ){
  predictors <- paste(predictors, "+", run_W_Nad[i])
  predict<- paste0(predict, ",", run_W_Nad[i])
}
formula <- as.formula(paste("event", "~", predictors))

gW_Nad <- glm(formula = formula , data = glm_lbou_W_Nad, family = binomial)
gw_htest_out_Nad <- predict(gW_Nad,glm_lbou_W_Nad[,.(SD_value3 , SLd_value3 , P_value6 ,SDdif6,WSavg_value6 , Tmin6)] , type="response")

plot(gw_htest_out_Nad)
plot(glm_lbou_W_Nad$event,gw_htest_out_Nad)


#wet Nad model
gw_Nad <- glm(event ~ SD_value3 + SLd_value3 + P_value6 + WSavg_value6 + Tmin6,data = glm_lbou_W_Nad, family = binomial)
summary(gw_Nad)
varImp(gw_Nad, scale = TRUE)


#Slab
run_S_d <- find_best_vars(indata = glm_lbou_S_d, period = period, bootstrap = "yes", boot_period = boot_period, new_candi = new_candi)
gS_d <- glm(formula = event ~  SD_value + NSS_value6 +SLd_value + WSavg_value + Tair_value6 + SLd_value6, data =glm_lbou_S_d, family = binomial)
summary (gS_d)
varImp(gS_d, scale = TRUE)

gS_d_04_20 <- glm(formula = event ~  SD_value + WD_value + SLd_value6 + SD_value6 + NSS_value3 +  + WSavg_value6, data =glm_lbou_S_d, family = binomial)
summary (gS_d_04_20)
varImp(gS_d_04_20, scale = TRUE)

gS_d_all <- glm(formula = event ~  SD_value + NSS_value6 + WD_value + WSavg_value + SLd_value6 + H_value6 , data =glm_lbou_S_d, family = binomial)
summary (gS_d_all)
varImp(gS_d_all, scale = TRUE)

#Wet new daily 1979-2020
run_W_d <- find_best_vars(indata = glm_data_lbou_C, period = period, bootstrap = "no", boot_period = boot_period, new_candi = new_candi)
gW_d <- glm(formula = event ~NSSsum6  +SD_value3 + P_value3  +SLd_value, data =glm_data_lbou_C, family = binomial)
summary (gW_d )
varImp(gW_d, scale = TRUE)

#Wet
run_W_d <- find_best_vars(indata = glm_lbou_W_d, period = period, bootstrap = "yes", boot_period = boot_period, new_candi = new_candi)
gW_d <- glm(formula = event ~ SD_value + WD_value6 + P_value3 + Tair_value3 + WSavg_value3 + +H_value6, data =glm_lbou_W_d, family = binomial)
summary (gW_d )
varImp(gW_d, scale = TRUE)

#best six variables for the period 1979-2020 verified by Bootstrap - 100 variation of six years
run1 <- find_best_vars(indata = glm_data_lbou_C_C, period = period, bootstrap = "yes", boot_period = boot_period, new_candi = new_candi)
g1 <- glm(formula = event ~  WSavg_value + SD_value6 + NSS_value6 + P_value6  + WD_value6 + H_value , data =glm_data_lbou_C_C, family = binomial)
summary (g1)
varImp(g1, scale = TRUE)
run2 <- find_best_vars(indata = glm_data_lbou_W_C, period = period, bootstrap = "yes", boot_period = boot_period, new_candi = new_candi)
g2 <- glm(formula = event ~ SD_value6 +  Tair_value6 + P_value6 +NSS_value6 +H_value6 +WSavg_value6 , data =glm_data_lbou_W_C, family = binomial)
summary (g2)
varImp(g2, scale = TRUE)
run3 <- readRDS("C:/Users/souckovamarketa/OneDrive - CZU v Praze/R/Avalanche/Bin/daily/run3.rds")
run3 <- find_best_vars(indata = glm_data_lbou_W_A, period = period, bootstrap = "yes", boot_period = boot_period, new_candi = new_candi)
g3 <- glm(formula = event ~ SD_value6 + P_value6 + Tair_value6+ WSavg_value6 + NSS_value6 +H_value6, data =glm_data_lbou_W_A, family = binomial)
summary (g3)
varImp(g3, scale = TRUE)
run4 <- readRDS("C:/Users/souckovamarketa/OneDrive - CZU v Praze/R/Avalanche/Bin/daily/run4.rds")
run4 <- find_best_vars(indata = glm_data_lbou_C_A, period = period, bootstrap = "yes", boot_period = boot_period, new_candi = new_candi)
g4 <- glm(formula = event ~  WSavg_value + SD_value  + WD_value3 + NSS_value3 + H_value3 + NSS_value6, data =glm_data_lbou_C_A, family = binomial)
summary (g4)
varImp(g4, scale = TRUE)
saveRDS(run1, "./run1.rds")
saveRDS(run2, "./run2.rds")
saveRDS(run3, "./run3.rds")
saveRDS(run4, "./run4.rds")
saveRDS(run3, "./run3_since2004.rds")
saveRDS(run1, "./run1_since2004.rds")
readRDS(run1_since2004)

run3
#best six variables for the period 1979-2020
# run_e_W_C <- find_best_vars(indata = glm_data_lbou_W_C, period = period, bootstrap = "no", boot_period = boot_period, new_candi = new_candi)
# run_e_C_C <- find_best_vars(indata = glm_data_lbou_C_C, period = period, bootstrap = "no", boot_period = boot_period, new_candi = new_candi)
# 
# run_e_W_A <- find_best_vars(indata = glm_data_lbou_W_A, period = period, bootstrap = "no", boot_period = boot_period, new_candi = new_candi)
# run_e_C_A <- find_best_vars(indata = glm_data_lbou_C_A, period = period, bootstrap = "no", boot_period = boot_period, new_candi = new_candi)

run_e_S <- find_best_vars(indata = glm_lbou_S_d, period = period, bootstrap = "no", boot_period = boot_period, new_candi = new_candi)

