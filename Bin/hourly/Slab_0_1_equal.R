library(data.table)
library(readr)
library(ggplot2)
library(lubridate)
library(writexl)
library(dplyr)  
library(zoo)
library(caret)
setwd ("C:/Users/souckovamarketa/OneDrive - CZU v Praze/R/Krkonose/Rcode/")
source("fun_find_best_vars.R")
setwd("C:/Users/souckovamarketa/OneDrive - CZU v Praze/R/Avalanche/Bin/hourly")
adcast_A_hourly <- readRDS("C:/Users/souckovamarketa/OneDrive - CZU v Praze/R/Avalanche/Bin/hourly/adcast_A_upd.rds")

adcast_A_hourly$event_1 <- NULL
colnames(adcast_A_hourly)[1] <- "Date"

#wet aval read
adcast_C_hourly <- readRDS("C:/Users/souckovamarketa/OneDrive - CZU v Praze/R/Avalanche/Bin/hourly/adcast_C_upd.rds")
adcast_C_hourly$event_1 <- NULL
colnames(adcast_C_hourly)[1] <- "Date"


#slab aval remove covariates
adcast_A_hourly$SWE_value <-adcast_A_hourly$SWE_value24 <- adcast_A_hourly$SWE_value72 <-adcast_A_hourly$SWE_value144 <-NULL
adcast_A_hourly$Rain_Tw_value <-adcast_A_hourly$Rain_Tw_value24 <- adcast_A_hourly$Rain_Tw_value72 <-adcast_A_hourly$Rain_Tw_value144 <-NULL
adcast_A_hourly$Rain_Ta_value <-adcast_A_hourly$Rain_Ta_value24 <- adcast_A_hourly$Rain_Ta_value72 <-adcast_A_hourly$Rain_Ta_value144 <-NULL
adcast_A_hourly$H_value <- adcast_A_hourly$H_value24 <-adcast_A_hourly$H_value72 <-adcast_A_hourly$H_value144<-NULL
adcast_A_hourly$Tair_value <- adcast_A_hourly$Tair_value24 <-adcast_A_hourly$Tair_value72 <-adcast_A_hourly$Tair_value144<-NULL
adcast_A_hourly$NSS_value <- adcast_A_hourly$NSS_value24 <-adcast_A_hourly$NSS_value72 <-adcast_A_hourly$NSS_value144<-NULL
adcast_A_hourly$WD_value<- adcast_A_hourly$WSavg_value <- adcast_A_hourly$SLd_value <-adcast_A_hourly$SD_value <-adcast_A_hourly$P_value <-adcast_A_hourly$NSS_value <-NULL
adcast_A_hourly$Tamp72 <- adcast_A_hourly$Tamp144 <-NULL

#wet aval remove covariates
adcast_C_hourly$SWE_value <-adcast_C_hourly$SWE_value24 <- adcast_C_hourly$SWE_value72 <-adcast_C_hourly$SWE_value144 <-NULL
adcast_C_hourly$Rain_Tw_value <-adcast_C_hourly$Rain_Tw_value24 <- adcast_C_hourly$Rain_Tw_value72 <-adcast_C_hourly$Rain_Tw_value144 <-NULL
adcast_C_hourly$Rain_Ta_value <-adcast_C_hourly$Rain_Ta_value24 <- adcast_C_hourly$Rain_Ta_value72 <-adcast_C_hourly$Rain_Ta_value144 <-NULL
adcast_C_hourly$H_value <- adcast_C_hourly$H_value24 <-adcast_C_hourly$H_value72 <-adcast_C_hourly$H_value144<-NULL
adcast_C_hourly$Tair_value <- adcast_C_hourly$Tair_value24 <-adcast_C_hourly$Tair_value72 <-adcast_C_hourly$Tair_value144<-NULL
adcast_C_hourly$NSS_value <- adcast_C_hourly$NSS_value24 <-adcast_C_hourly$NSS_value72 <-adcast_C_hourly$NSS_value144<-NULL
adcast_C_hourly$WD_value<- adcast_C_hourly$WSavg_value <- adcast_C_hourly$SLd_value <-adcast_C_hourly$SD_value <-adcast_C_hourly$P_value <-NULL
adcast_C_hourly$Tamp72 <- adcast_C_hourly$Tamp144 <-NULL

# # slab 0=1
# Slab_EQ_0 <- adcast_A_hourly[event == 0 & ID %in% sample(x = adcast_A_hourly$ID, size = 500)]
# Slab_EQ_1 <- adcast_A_hourly[event == 1]
# adcast_Slab <- rbind(Slab_EQ_0,Slab_EQ_1)
# # wet 0=1
# Wet_EQ_0 <- adcast_C_hourly[event == 0 & ID %in% sample(x = adcast_C_hourly$ID, size = 70)]
# Wet_EQ_1 <- adcast_C_hourly[event == 1]
# adcast_Wet <- rbind(Wet_EQ_0,Wet_EQ_1)
# 
# #SLAB 2
# adcast_Slab$WARM <- 1
# adcast_Slab$COLD <- 1.01
# # Slab aval pokus
# plot(x = adcast_Slab[event == 1 & CAW == "cold", Date], y = adcast_Slab[event == 1 & CAW == "cold", COLD], type = "p", col = "blue")
# lines(x = adcast_Slab[event == 1 & CAW == "warm", Date], y = adcast_Slab[event == 1 & CAW == "warm", WARM], type = "p", col = "red")
# 
# #Wet 2
# adcast_Wet$WARM <- 1
# adcast_Wet$COLD <- 1.01
# # Slab aval pokus
# plot(x = adcast_Wet[event == 1 & CAW == "cold", Date], y = adcast_Wet[event == 1 & CAW == "cold", COLD], type = "p", col = "blue")
# lines(x = adcast_Wet[event == 1 & CAW == "warm", Date], y = adcast_Wet[event == 1 & CAW == "warm", WARM], type = "p", col = "red")


glm_lbou_S <-adcast_A_hourly
#glm_lbou_S <- adcast_Slab
saveRDS(glm_lbou_S, "./glm_lbou_S_hourly.rds")

glm_lbou_W <-adcast_C_hourly
glm_lbou_W <- adcast_Wet
saveRDS(glm_lbou_W, "./glm_lbou_W.rds")

#separation into warm and cold temperature events 2
#glm_data_lbou_W_S <- adcast_Slab[CAW == "warm"]
#saveRDS(glm_data_lbou_W_S, "./glm_data_lbou_W_S.rds")
#glm_data_lbou_C_S <- adcast_Slab[CAW == "cold"]
#saveRDS(glm_data_lbou_C_S, "./glm_data_lbou_C_S.rds")

period <- c(2004:2020)
boot_period <- c(2004:2020)
new_candi <- names(glm_lbou_S)[-1:-3]
new_candi <- names(glm_lbou_W)[-1:-3]

glm_lbou_S_Nad =  glm_lbou_S
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

gS_Nad <- glm(formula = formula , data = glm_lbou_S_Nad, family = binomial)
gw_htest_out_Nad <- predict(gS_Nad,glm_lbou_S_Nad[,.(WSavg_value72,SLd_value24,SD_value24,WD_value24,SDdif144,SD_value144)] , type="response")
plot(gw_htest_out_Nad)
plot(glm_lbou_S_Nad$event,gw_htest_out_Nad)

gS_h <- glm(formula = event ~ SD_value24+SDdif48+NSSsum72 +P_value72, data =glm_lbou_S, family = binomial)
summary (gS_h)
gS_h <- glm(formula = event ~ WSavg_value72+SLd_value24+SD_value24+WD_value24, data =glm_lbou_S_Nad, family = binomial)
summary (gS_Nad)
varImp(gS_h, scale = TRUE)

#slab run

glm_lbou_SJFM <- glm_lbou_S[month(Date) %in% c(1,2,3)]
run_SJFM <- find_best_vars(indata = glm_lbou_SJFM, period = period, bootstrap = "no", boot_period = boot_period, new_candi = new_candi)

predictors <- run_SJFM[1]
predict <- run_SJFM[1]
for(i in 2:length(run_SJFM) ){
  predictors <- paste(predictors, "+", run_SJFM[i])
  predict<- paste0(predict, ",", run_SJFM[i])
}
formula <- as.formula(paste("event", "~", predictors))
gS_SJFM <- glm(formula = formula , data = glm_lbou_SJFM, family = binomial)
gw_SJFM_pred <- predict(gS_SJFM,glm_lbou_SJFM[,.( WSavg_value24 , SLd_value24 , WD_value144 , SD_value24 , 
                                                    SDdif144 )] , type="response")
plot(gw_SJFM_pred)
plot(glm_lbou_SJFM$event,gw_SJFM_pred)
gS_h <- glm(formula = event ~ WSavg_value24 + SLd_value24 + WD_value144 + SD_value24 + 
            SDdif144, data =glm_lbou_S, family = binomial)
summary (gS_h)
#Testing

gS_tno_weights <- glm(formula = event ~  SD_value24, data = glm_lbou_S, family = binomial)
gw_htest_out <- predict(gS_tno_weights,glm_lbou_S[,.(SD_value24)] , type="response")
plot(gw_htest_out)
varImp(gS_tno_weights)

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

#gS_tno_weights <- glm(formula = event ~  SLd_value144 + SD_value +NSSsum72, data = glm_lbou_W_test, family = binomial, control = list(trace=TRUE, maxit = 100, epsilon = 0.0000001))

gS_tno_weights <- glm(formula = event ~  SD_value24+SDdif48+NSSsum72 +P_value72, data = glm_lbou_S, family = binomial)
gw_htest_out <- predict(gS_tno_weights,glm_lbou_S[,.(SD_value24, SDdif48, NSSsum72, P_value72)] , type="response")
plot(gw_htest_out)
plot(glm_lbou_S$event,gw_htest_out)

gS_h <- glm(formula = event ~ SD_value24+SDdif48+NSSsum72 +P_value72, data =glm_lbou_S, family = binomial)
summary (gS_h)
varImp(gS_h, scale = TRUE)

#Hosmer-Lemeshow Goodness of Fit
#https://www.theanalysisfactor.com/r-glm-model-fit/
install.packages("ResourceSelection")
library(ResourceSelection)
hoslem.test(glm_lbou_S$event, fitted(gS_h))
y <- glm_lbou_S$event[which(!is.na(gw_htest_out))]
yhat <- gw_htest_out[which(!is.na(gw_htest_out))]
hoslem.test(y, yhat)
#p-value = 0.1508, 0.056
#Our model appears to fit well because we have no significant difference between the model and the observed data (i.e. the p-value is above 0.05)

#wet  run
run_W <- find_best_vars(indata = glm_lbou_W, period = period, bootstrap = "no", boot_period = boot_period, new_candi = new_candi)
gW_h <- glm(formula = event ~  SLd_value + SD_value +P_value, data =glm_lbou_W, family = binomial, maxit = 100)

weights <- glm_lbou_W$event*400000+1

#delete rows if I need Nad
glm_lbou_W_test <- glm_lbou_W

glm_lbou_W_test[, delete := FALSE]
nrow_nad <- nrow(glm_lbou_W_test[event == 0])
delete_this <- c(rep(TRUE, 6),FALSE, rep(TRUE, 6))

delete_vector <- rep(delete_this, ceiling(nrow_nad/length(delete_this)))
delete_vector <- delete_vector[1:nrow_nad]

glm_lbou_W_test[event == 0, delete := delete_vector]
glm_lbou_W_test <- glm_lbou_W_test[delete == FALSE]
#

weight <- (nrow(glm_lbou_W_test))/nrow(glm_lbou_W_test[event == 1])
weights <- glm_lbou_W_test$event*weight + 1
gW_htest <- glm(formula = event ~  SLd_value144 + SD_value +NSSsum72, data = glm_lbou_W_test, family = binomial, control = list(trace=TRUE, maxit = 100, epsilon = 0.0000001), weights = weights)
#investigate this for all variables if there is see any 0 values
# gW_htestno_weights <- glm(formula = event ~  SLd_value144 + SD_value +NSSsum72, data = glm_lbou_W_test, family = binomial, control = list(trace=TRUE, maxit = 100, epsilon = 0.0000001))
# gw_htest_out <- predict(gW_htest,glm_lbou_W_test[,.(SLd_value144,SD_value,NSSsum72 )] , type="response")
# plot(gw_htest_out)

#investigate each variable

varImp(gW_htest, weights = weights)
varImp(gW_htestno_weights, weights = weights)
summary(gW_htestno_weights)

#predict
plot(glm_lbou_S$event, gw_htest_out)
summary (gW_h)
varImp(gW_h, scale = TRUE, conditional = TRUE)
glm_lbou_W$y_pred = predict(gW_h,glm_lbou_W , type="response")



#slab 0,1 #slab 0,1 #slab 0,1 #slab 0,1
run3_h <- find_best_vars(indata = glm_data_lbou_W_S, period = period, bootstrap = "yes", boot_period = boot_period, new_candi = new_candi)

g3_h <- glm(formula = event ~  SD_value72 + WSavg_value144 + SLd_value144 + NSS_value72 +WSavg_value72 + P_value24, data =glm_data_lbou_W_S, family = binomial, maxit =200)
summary (g3_h)
varImp(g3_h, scale = TRUE)

run4_h <- find_best_vars(indata = glm_data_lbou_C_S, period = period, bootstrap = "yes", boot_period = boot_period, new_candi = new_candi)
g4_h <- glm(formula = event ~  WD_value72 + SLd_value + H_value + SD_value +Tair_value144 + Tair_value72 , data =glm_data_lbou_C_S, family = binomial)
summary (g4_h)
varImp(g4_h, scale = TRUE)
# 2004-2020, Nad=500
run_e_S <- find_best_vars(indata = glm_lbou_S, period = period, bootstrap = "no", boot_period = boot_period, new_candi = new_candi)
gS_e_h <- glm(formula = event ~  WSavg_value + SLd_value + SD_value + NSS_value72 +NSS_value + WD_value72, data =glm_lbou_S, family = binomial)
summary (gS_e_h)
varImp(gS_e_h, scale = TRUE)
