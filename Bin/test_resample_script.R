library(data.table)
library(readr)
library(ggplot2)
library(lubridate)
library(writexl)
library(dplyr)  
library(zoo)
library(caret)

adcast_A_hourly <- readRDS("C:/Users/souckovamarketa/OneDrive - CZU v Praze/R/Avalanche/Bin/hourly/adcast_A_upd.rds")
setwd ("C:/Users/souckovamarketa/OneDrive - CZU v Praze/R/Krkonose/Rcode/")
source("fun_resample_glm_best_six.R")

boot_period <- c(1979:1999, 2002:2020)
#boot_period <- c(2004:2020)

# vector of 6 best candidates identified before # do I have to find the number?
best_six <- names(adcast_A_hourly)[c(8, 6, 19)]
#SD_value24+SDdif48+ NSSsum72 +P_value72
names(adcast_A_hourly)[1] <- "Date"

# how often you want to boot strap
nboots <- 10

out <- find_resample(indata = adcast_A_hourly, boot_period = boot_period, best_candi = best_six, nboots = nboots,
                     navd_weights = 1, avd_weights = 1, maxit = 20, epsilon = 1e-8)

out[,min := min(Overall),  .(variable)]
out[,max := max(Overall), .(variable)]
out[,median:= median(Overall), .(variable)]

ggplot(out, aes(x = variable))+
  geom_segment(aes(xend = variable, yend = max, y = min), size = 5, alpha = 0.2)+
  geom_point(aes(y = median), size = 2, color = "darkorange")+
  coord_flip()+
  labs(x = "z score")+
  theme_bw()

ggplot(out, aes(x = variable))+
  geom_boxplot(aes(y = Overall), fill = "white")+
  geom_point(aes(y = median), size = 2, color = "darkorange")+
  coord_flip()+
  labs(x = "z score")+
  theme_bw()

