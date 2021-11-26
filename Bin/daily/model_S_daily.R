library(data.table)

# building new variables from previous dataset
dta_dir <- ("C:/Users/souckovamarketa/OneDrive - CZU v Praze/R/Avalanche/Bin/daily/")
adcast_A_daily <- readRDS(paste0(dta_dir, "adcast_A_daily.rds"))
adcast_C_daily <- readRDS(paste0(dta_dir, "adcast_C_daily.rds"))

## adcast_A_daily
adcast_A_daily_sub <- subset(adcast_A_daily, select = -c(ID, PLOT, stat))
adcast_A_daily_unique <- unique(adcast_A_daily_sub) 
adcast_A_daily_aval_days <- adcast_A_daily[event == 1, max(as.Date(Date)), .(ID)]
names(adcast_A_daily_aval_days)[2] <- "Date"

# New predictors
adcast_A_daily_unique[,NSSsum3:= frollsum(NSS_value,3, align = "right", na.rm = T)]
adcast_A_daily_unique[,NSSsum6:= frollsum(NSS_value,6, align = "right", na.rm = T)]
adcast_A_daily_unique[,Rain_Ta_sum3:= frollsum(Rain_Ta_value,3, align = "right", na.rm = T)]
adcast_A_daily_unique[,Rain_Ta_sum6:= frollsum(Rain_Ta_value,6, align = "right", na.rm = T)]
adcast_A_daily_unique[,Tmax3:= frollapply(Tair_value,3, max,  align = "right", na.rm = T, fill = NA)]
adcast_A_daily_unique[,Tmax6:= frollapply(Tair_value,6, max,  align = "right", na.rm = T)]
adcast_A_daily_unique[,Tmin3:= frollapply(Tair_value,3, min,  align = "right", na.rm = T)]
adcast_A_daily_unique[,Tmin6:= frollapply(Tair_value,6, min,  align = "right", na.rm = T)]
adcast_A_daily_unique[Tmax3 == Inf | Tmax3 == -Inf,Tmax3:= NA]
adcast_A_daily_unique[Tmax6 == Inf | Tmax6 == -Inf,Tmax6:= NA]
adcast_A_daily_unique[Tmin3 == Inf | Tmin3 == -Inf,Tmin3:= NA]
adcast_A_daily_unique[Tmin6 == Inf | Tmin6 == -Inf,Tmin6:= NA]

adcast_A_daily_unique[,Tamp3:= Tmax3-Tmin3]
adcast_A_daily_unique[,Tamp6:= Tmax6-Tmin6]
adcast_A_daily_unique[,SD2:= shift(SD_value, 2,  type = 'lag')]
adcast_A_daily_unique[,SD3:= shift(SD_value, 3,  type = 'lag')]
adcast_A_daily_unique[,SD4:= shift(SD_value, 4,  type = 'lag')]
adcast_A_daily_unique[,SD6:= shift(SD_value, 6,  type = 'lag')]
adcast_A_daily_unique[,SDdif2:= SD_value - SD2]
adcast_A_daily_unique[,SDdif3:= SD_value - SD3]
adcast_A_daily_unique[,SDdif4:= SD_value - SD4]
adcast_A_daily_unique[,SDdif6:= SD_value - SD6]

adcast_A_daily_unique[,keep:= TRUE]
adcast_A_daily_unique[,Date_check := shift(Date, 6,  type = 'lag')]
adcast_A_daily_unique[difftime(Date, Date_check, units = 'days') > 6, keep := FALSE]
adcast_A_daily_unique[event == 1 & !(Date %in% adcast_A_daily_aval_days$Date), keep := FALSE]
adcast_A_daily_unique[event == 1 & Date %in% adcast_A_daily_aval_days$Date, keep := TRUE]
adcast_A_daily_unique[, aval_before := which.min(difftime( adcast_A_daily_aval_days$Date,Date, units = 'days') < 6)-1,.(Date)]
adcast_A_daily_unique[aval_before > 0, aval_date_before := adcast_A_daily_aval_days$Date[aval_before]]
adcast_A_daily_unique[, norm_date := Date]
adcast_A_daily_unique[event == 0 & difftime( Date, aval_date_before,units = "days") < 6, keep:= FALSE]


adcast_A_daily_unique <- subset(adcast_A_daily_unique, select = -c(SD2,SD3,SD4,SD6,Date_check, aval_before, aval_date_before, norm_date))

adcast_A_daily_updated <- adcast_A_daily_unique[keep == TRUE]
adcast_A_daily_updated <- subset(adcast_A_daily_updated, select = -c(keep))

saveRDS(adcast_A_daily_updated, file = 'adcast_A_daily_upd.rds')

## adcast_C_daily
adcast_C_daily_sub <- subset(adcast_C_daily, select = -c(ID, PLOT, stat))
adcast_C_daily_unique <- unique(adcast_C_daily_sub) 
adcast_C_daily_aval_days <- adcast_C_daily[event == 1, max(as.Date(Date)), .(ID)]
names(adcast_C_daily_aval_days)[2] <- "Date"

#New predictors
adcast_C_daily_unique[,NSSsum3:= frollsum(NSS_value,3, align = "right", na.rm = T)]
adcast_C_daily_unique[,NSSsum6:= frollsum(NSS_value,6, align = "right", na.rm = T)]
adcast_C_daily_unique[,Rain_Ta_sum3:= frollsum(Rain_Ta_value,3, align = "right", na.rm = T)]
adcast_C_daily_unique[,Rain_Ta_sum6:= frollsum(Rain_Ta_value,6, align = "right", na.rm = T)]
adcast_C_daily_unique[,Tmax3:= frollapply(Tair_value,3, max,  align = "right", na.rm = T)]
adcast_C_daily_unique[,Tmax6:= frollapply(Tair_value,6, max,  align = "right", na.rm = T)]
adcast_C_daily_unique[,Tmin3:= frollapply(Tair_value,3, min,  align = "right", na.rm = T)]
adcast_C_daily_unique[,Tmin6:= frollapply(Tair_value,6, min,  align = "right", na.rm = T)]
adcast_C_daily_unique[Tmax3 == Inf | Tmax3 == -Inf,Tmax3:= NA]
adcast_C_daily_unique[Tmax6 == Inf | Tmax6 == -Inf,Tmax6:= NA]
adcast_C_daily_unique[Tmin3 == Inf | Tmin3 == -Inf,Tmin3:= NA]
adcast_C_daily_unique[Tmin6 == Inf | Tmin6 == -Inf,Tmin6:= NA]
adcast_C_daily_unique[,Tamp3:= Tmax3-Tmin3]
adcast_C_daily_unique[,Tamp6:= Tmax6-Tmin6]
adcast_C_daily_unique[,SD2:= shift(SD_value, 2,  type = 'lag')]
adcast_C_daily_unique[,SD3:= shift(SD_value, 3,  type = 'lag')]
adcast_C_daily_unique[,SD4:= shift(SD_value, 4,  type = 'lag')]
adcast_C_daily_unique[,SD6:= shift(SD_value, 6,  type = 'lag')]
adcast_C_daily_unique[,SDdif2:= SD_value - SD2]
adcast_C_daily_unique[,SDdif3:= SD_value - SD3]
adcast_C_daily_unique[,SDdif4:= SD_value - SD4]
adcast_C_daily_unique[,SDdif6:= SD_value - SD6]

adcast_C_daily_unique[,keep:= TRUE]
adcast_C_daily_unique[,Date_check := shift(Date, 6,  type = 'lag')]
adcast_C_daily_unique[difftime(Date, Date_check, units = 'days') > 6, keep := FALSE]
adcast_C_daily_unique[event == 1 & !(Date %in% adcast_C_daily_aval_days$Date), keep := FALSE]
adcast_C_daily_unique[event == 1 & Date %in% adcast_C_daily_aval_days$Date, keep := TRUE]
adcast_C_daily_unique[, aval_before := which.min(difftime( adcast_C_daily_aval_days$Date,Date, units = 'days') < 6)-1,.(Date)]
adcast_C_daily_unique[aval_before > 0, aval_date_before := adcast_C_daily_aval_days$Date[aval_before]]
adcast_C_daily_unique[, norm_date := Date]
adcast_C_daily_unique[event == 0 & difftime( Date, aval_date_before,units = "days") < 6, keep:= FALSE]
adcast_C_daily_unique <- subset(adcast_C_daily_unique, select = -c(SD2,SD3,SD4,SD6,Date_check, aval_before, aval_date_before, norm_date))
adcast_C_daily_updated <- adcast_C_daily_unique[keep == TRUE]
adcast_C_daily_updated <- subset(adcast_C_daily_updated, select = -c(keep))

saveRDS(adcast_C_daily_updated, file = 'adcast_C_daily_upd.rds')

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

#delete multicolinear variables
adcast_C_daily$SWE_value <- adcast_C_daily$SWE_value3 <-adcast_C_daily$SWE_value6 <-NULL
adcast_C_daily$Rain_Ta_value <-adcast_C_daily$Rain_Ta_value3  <-adcast_C_daily$Rain_Ta_value6 <-NULL
adcast_C_daily$Rain_Tw_value <-adcast_C_daily$Rain_Tw_value3  <-adcast_C_daily$Rain_Tw_value6 <-NULL
#adcast_C_daily$Rain_Ta_sum6 <-adcast_C_daily$Rain_Ta_sum3  <-NULL
adcast_C_daily$H_value <- adcast_C_daily$H_value3 <-adcast_C_daily$H_value6 <-NULL
adcast_C_daily$Tair_value<-adcast_C_daily$Tair_value3<-adcast_C_daily$Tair_value6<-NULL
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
glm_data_lbou_C <- adcast_C_daily#[stat == "LBOU"]

period <- c(1979:1999, 2002:2020)
boot_period <- c(1979:1999, 2002:2020)
#period <- c(2004:2020)
#boot_period <- c(2010:2020)

new_candi <- names(glm_data_lbou_A)[-1:-3]
new_candi <- names(glm_data_lbou_C)[-1:-3]

# compare one variable
glm_data_lbou_A[, plot(SDdif6, event)]
# model slab aval
run_S <- find_best_vars(indata = glm_data_lbou_A, period = period, bootstrap = "no", boot_period = boot_period, new_candi = new_candi)

predictors <- run_S[1]
predict <- run_S[1]
for(i in 2:length(run_S) ){
  predictors <- paste(predictors, "+", run_S[i])
  predict<- paste0(predict, ",", run_S[i])
}
formula <- as.formula(paste("event", "~", predictors))

gS <- glm(formula = formula , data = glm_data_lbou_A, family = binomial)
gS <- glm(event ~ SD_value + NSSsum6 + WD_value + Tmax6, data = glm_data_lbou_A, family = binomial)
gS <- glm(event ~ SD_value3 + NSSsum6 + WD_value3 + Tmax6, data = glm_data_lbou_A, family = binomial)
gSPRE <- predict(gS,glm_data_lbou_A[,.(SD_value3 , NSSsum6 , WD_value3 , Tmax6 )] , type="response")
gSPRE <- predict(gS,glm_data_lbou_A[,.(SD_value , NSSsum6 , WD_value , Tmax6 )] , type="response")
plot(gSPRE)
plot(glm_data_lbou_A$event,gSPRE)
summary (gS)
library (ModelMetrics)
gini(gS)[1]


# Rows are correct outcome, columns are prediction with threshold 0.5
tab <- table(glm_data_lbou_A$event, gSPRE>= 0.1)
#tab <- table(gW, gWPRE>= 0.2)
tab   # Display the confusion matrix
accuracy.reg <- sum(diag(tab)) / sum(tab)
accuracy.reg  # Output accuracy

#random forest
glm_data_lbou_A[, cevent := factor(event)]
library (randomForest)
gS <- randomForest(cevent ~ SD_value3 + NSSsum6 + WD_value3 + Tmax6, data = glm_data_lbou_A[month(Date)%in% c(2)& complete.cases(SD_value3 + NSSsum6 + WD_value3 + Tmax6)], family = binomial)

# glm model with seasonal factor
glm_data_lbou_A[, cevent := factor(event)]
gS1 <- glm(formula = event ~ SD_value3 + NSSsum6 + WD_value3 + Tmax6 + SDdif2 + Tmin3 + month(Date), data = glm_data_lbou_A[complete.cases(SD_value3 + NSSsum6 + WD_value3 + Tmax6 + SDdif2 + Tmin3)], family = binomial)
summary(gS1)

# DELETE Nad - if I do it then I have better results - why???
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
gS_Nad <- glm(formula = event ~ SD_value + NSSsum6 + WD_value + Tmax6 + SDdif4 + Tmin3 + month(Date), data = glm_lbou_S_Nad[complete.cases(SD_value + NSSsum6 + WD_value + Tmax6 + SDdif4 + Tmin3)], family = binomial)
summary(gS_Nad)
library (randomForest)
gS_Nad <- randomForest(cevent ~ SD_value3 + NSSsum6 + WD_value3 + Tmax6 + SDdif4, data = glm_lbou_S_Nad[month(Date)%in% c(2)& complete.cases(SD_value3 + NSSsum6 + WD_value3 + Tmax6 + SDdif4)], family = binomial)


# model wet aval
run_W <- find_best_vars(indata = glm_data_lbou_C, period = period, bootstrap = "no", boot_period = boot_period, new_candi = new_candi)

predictors <- run_W[1]
predict <- run_W[1]
for(i in 2:length(run_W) ){
  predictors <- paste(predictors, "+", run_W[i])
  predict<- paste0(predict, ",", run_W[i])
}
formula <- as.formula(paste("event", "~", predictors))

gW <- glm(formula = formula , data = glm_data_lbou_C, family = binomial)
summary (gW)
gW <- glm(event ~ SD_value3 + Tmin3 + NSSsum6 + SLd_value, data = glm_data_lbou_C, family = binomial)
gWPRE <- predict(gW,glm_data_lbou_C[,.(SD_value3 , Tmin3, NSSsum6 , SLd_value )] , type="response")
plot(gWPRE)
plot(glm_data_lbou_C$event,gWPRE)

library (ModelMetrics)
gini(gW)[1]

# Rows are correct outcome, columns are prediction with threshold 0.5
tab <- table(glm_data_lbou_C$event, gWPRE>= 0.1)
#tab <- table(gW, gWPRE>= 0.2)
tab   # Display the confusion matrix
accuracy.reg <- sum(diag(tab)) / sum(tab)
accuracy.reg  # Output accuracy

#random forest
glm_data_lbou_C[, cevent := factor(event)]
library (randomForest)
gW <- randomForest(event ~ SD_value3 + Tmin3 + NSSsum6 + SLd_value, data = glm_data_lbou_C[month(Date)%in% c(2)& complete.cases(SD_value3 + Tmin3 + NSSsum6 + SLd_value)], family = binomial)
