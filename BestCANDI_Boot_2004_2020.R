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
#adcast_A_daily <- readRDS("./adcast_A.rds")
adcast_C_hourly <- readRDS("C:/Users/Marketa/OneDrive - CZU v Praze/R/Avalanche/Bin/hourly/adcast_C.rds")

names(adcast_A_hourly)[names(adcast_A_hourly) == "UTC"] <- "Date"
names(adcast_C_hourly)[names(adcast_C_hourly) == "UTC"] <- "Date"
#glm_data_lbou_A <- adcast_A[stat == "LBOU"]
#glm_data_lbou_C <- adcast_C_daily[stat == "LBOU"]
#slab aval
glm_data_lbou_W_A <- adcast_A_hourly[CAW == "warm"]
glm_data_lbou_C_A <- adcast_A_hourly[CAW == "cold"]
str(glm_data_lbou_W_A)
adcast_A_hourly$SWE_value <-adcast_A_hourly$SWE_value24 <- adcast_A_hourly$SWE_value72 <-adcast_A_hourly$SWE_value144 <-NULL
adcast_C_hourly$SWE_value <-adcast_C_hourly$SWE_value24 <- adcast_C_hourly$SWE_value72 <-adcast_C_hourly$SWE_value144 <-NULL
#wet aval
glm_data_lbou_W_C <- adcast_C_hourly[CAW == "warm"]
glm_data_lbou_C_C <- adcast_C_hourly[CAW == "cold"]

period <- c(1979:2020)
boot_period <- c(1979:1999, 2002:2020)

new_candi <- names(glm_data_lbou_W_C)[-1:-6]
#best six variables for the period 1979-2020 verified by Bootstrap - 100 variation of six years
run1 <- find_best_vars(indata = glm_data_lbou_C_C, period = period, bootstrap = "yes", boot_period = boot_period, new_candi = new_candi)

run2 <- find_best_vars(indata = glm_data_lbou_W_C, period = period, bootstrap = "yes", boot_period = boot_period, new_candi = new_candi)

run3 <- find_best_vars(indata = glm_data_lbou_W_A, period = period, bootstrap = "yes", boot_period = boot_period, new_candi = new_candi)

run4 <- find_best_vars(indata = glm_data_lbou_C_A, period = period, bootstrap = "yes", boot_period = boot_period, new_candi = new_candi)

#best six variables for the period 1979-2020
run_e_W_C <- find_best_vars(indata = glm_data_lbou_W_C, period = period, bootstrap = "no", boot_period = boot_period, new_candi = new_candi)
run_e_C_C <- find_best_vars(indata = glm_data_lbou_C_C, period = period, bootstrap = "no", boot_period = boot_period, new_candi = new_candi)

run_e_W_A <- find_best_vars(indata = glm_data_lbou_W_A, period = period, bootstrap = "no", boot_period = boot_period, new_candi = new_candi)
run_e_C_A <- find_best_vars(indata = glm_data_lbou_C_A, period = period, bootstrap = "no", boot_period = boot_period, new_candi = new_candi)
saveRDS(run_e_W_C, "./run_e_W_C.rds")
saveRDS(run_e_C_C, "./run_e_C_C.rds")
