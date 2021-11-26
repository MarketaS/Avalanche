library(data.table)
library(readr)
library(ggplot2)
library(lubridate)
library(writexl)
library(dplyr)  
library(zoo)
library(caret)
setwd ("C:/Users/Marketa/OneDrive - CZU v Praze/R/Krkonose/Rcode/RDS/")
source("fun_find_best_vars.R")
setwd("C:/Users/Marketa/OneDrive - CZU v Praze/R/Avalanche/Bin/hourly")
adcast_A_hourly <- readRDS("C:/Users/Marketa/OneDrive - CZU v Praze/R/Avalanche/Bin/hourly/adcast_A.rds")
adcast_A_hourly$event_1 <- NULL
colnames(adcast_A_hourly)[1] <- "Date"

adcast_C_hourly <- readRDS("C:/Users/Marketa/OneDrive - CZU v Praze/R/Avalanche/Bin/hourly/adcast_C.rds")
adcast_C_hourly$event_1 <- NULL
colnames(adcast_C_hourly)[1] <- "Date"
#names(adcast_A_hourly)[names(adcast_A_hourly) == "UTC"] <- "Date"
#names(adcast_C_hourly)[names(adcast_C_hourly) == "UTC"] <- "Date"
#glm_data_lbou_A <- adcast_A[stat == "LBOU"]
#glm_data_lbou_C <- adcast_C_daily[stat == "LBOU"]
#slab aval
adcast_A_hourly$SWE_value <-adcast_A_hourly$SWE_value24 <- adcast_A_hourly$SWE_value72 <-adcast_A_hourly$SWE_value144 <-NULL

glm_data_lbou_W_A <- adcast_A_hourly[CAW == "warm"]
glm_data_lbou_C_A <- adcast_A_hourly[CAW == "cold"]
str(glm_data_lbou_W_A)

#wet aval
adcast_C_hourly$SWE_value <-adcast_C_hourly$SWE_value24 <- adcast_C_hourly$SWE_value72 <-adcast_C_hourly$SWE_value144 <-NULL
glm_data_lbou_W_C <- adcast_C_hourly[CAW == "warm"]
glm_data_lbou_C_C <- adcast_C_hourly[CAW == "cold"]

period <- c(1979:2020)
boot_period <- c(1979:1999, 2002:2020)

new_candi <- names(glm_data_lbou_W_C)[-1:-5]
#best six variables for the period 1979-2020 verified by Bootstrap - 100 variation of six years
run1 <- find_best_vars(indata = glm_data_lbou_C_C, period = period, bootstrap = "yes", boot_period = boot_period, new_candi = new_candi)

run2 <- find_best_vars(indata = glm_data_lbou_W_C, period = period, bootstrap = "yes", boot_period = boot_period, new_candi = new_candi)

run3 <- find_best_vars(indata = glm_data_lbou_W_A, period = period, bootstrap = "yes", boot_period = boot_period, new_candi = new_candi)
g1 <- glm(formula = event ~ WSavg_value144 + SLd_value144 + WSavg_value72 + SD_value72 + NSS_value72 + NSS_value144, data =glm_data_lbou_W_A, family = binomial)
summary (g1)
varImp(g1, scale = TRUE)
run4 <- find_best_vars(indata = glm_data_lbou_C_A, period = period, bootstrap = "yes", boot_period = boot_period, new_candi = new_candi)
g2 <- glm(formula = event ~ WD_value + SLd_value144  + SD_value + NSS_value72 + SD_value72 + NSS_value, data =glm_data_lbou_C_A, family = binomial)
summary (g2)
varImp(g2, scale = TRUE)
saveRDS(run3, "./run3.rds")
saveRDS(run4, "./run4.rds")
#best six variables for the period 1979-2020
run_e_W_C <- find_best_vars(indata = glm_data_lbou_W_C, period = period, bootstrap = "no", boot_period = boot_period, new_candi = new_candi)
run_e_C_C <- find_best_vars(indata = glm_data_lbou_C_C, period = period, bootstrap = "no", boot_period = boot_period, new_candi = new_candi)

run_e_W_A <- find_best_vars(indata = glm_data_lbou_W_A, period = period, bootstrap = "no", boot_period = boot_period, new_candi = new_candi)
run_e_C_A <- find_best_vars(indata = glm_data_lbou_C_A, period = period, bootstrap = "no", boot_period = boot_period, new_candi = new_candi)
saveRDS(run_e_W_C, "./run_e_W_C.rds")
saveRDS(run_e_C_C, "./run_e_C_C.rds")
