install.packages("corrplot")

library(corrplot)
library(data.table)
library(readr)
library(ggplot2)
library(lubridate)
library(writexl)
library(dplyr)  
library(zoo)
setwd ("c:/Users/marketa.souckova/Documents/laviny/")
setwd("C:/Users/souckovamarketa/OneDrive - CZU v Praze/R/Avalanche/Dat")

daily <- readRDS("D:/marketa2/marketa/data_daily_1962_2020.rds")
daily <- readRDS("C:/Users/souckovamarketa/OneDrive - CZU v Praze/R/Avalanche/Dat/data_daily_1962_2020.rds")
daily$UTC <- as.POSIXct(paste0(as.character(daily$Date), " 12:00:00"), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
daily <- daily[,c(3,4,13,14)]

dta_lbou <- readRDS("D:/marketa2/marketa/Hourly_new/data/hourly_data_2004_2020.RDS")
dta_lbou <- readRDS("C:/Users/souckovamarketa/OneDrive - CZU v Praze/R/Avalanche/Dat/hourly_data_2004_2020.RDS")
colnames(dta_lbou) <- c("Date", "SD", "NSS", "SWE", "H", "WD", "WSavg","WSmax", "Tair", "P", "SLd", "time", "Date_min", "RBLB", "Tsoil05", "Rain_Tw", "Rain_Ta")
dta_lbou$time <- dta_lbou$RBLB <- dta_lbou$WSmax <- dta_lbou$Tsoil05 <-NULL

dates <- gsub(pattern = " UTC", replacement = "", x = dta_lbou$Date_min)
dta_lbou$UTC <- as.POSIXct(x = dates, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
dta_lbou$Date <- dta_lbou$Date_min <- dta_lbou$SD <- dta_lbou$NSS <- dta_lbou$SWE <- NULL 

dta_lbou <- merge(x = daily, y = dta_lbou, by = "UTC", all.y = T)

NonNAindexSD <- which(!is.na(dta_lbou$SD))
NonNAindexNSS <- which(!is.na(dta_lbou$NSS))
NonNAindexSWE <- which(!is.na(dta_lbou$SWE))

dta_lbou$SD[min(NonNAindexSD):max(NonNAindexSD)] <- na.approx(object = dta_lbou$SD)
dta_lbou$NSS[min(NonNAindexNSS):max(NonNAindexNSS)] <- na.approx(object = dta_lbou$NSS)
dta_lbou$SWE[min(NonNAindexSWE):max(NonNAindexSWE)] <- na.approx(object = dta_lbou$SWE)
dta <- dta_lbou
#dta <- data.table(read_delim("./data/data_hourly_2004_2020.txt", 
                             "\t", escape_double = FALSE, 
                             col_types = cols(D = col_double(), 
                                              Fmax = col_double(), Fprum = col_double(), 
                                              H = col_double(), RGLB1H = col_double(), 
                                              SCE = col_double(), SNO = col_double(), 
                                              SRA1H = col_double(), SSV1H = col_double(), 
                                              SVH = col_double(), T = col_double(), 
                                              T05 = col_double(), date = col_character(), 
                                              datum = col_date(format = "%Y-%m-%d"), 
                                              time = col_character()), trim_ws = TRUE))
#https://exceltown.com/navody/jazyk-r/vizualizace-korelace-v-r/
# create a correlation matrix
glm_data_lbou_A [,c(-1, -2, -3, -4)]
dta <- glm_data_lbou_A
dta <- dta [,c(-1, -2, -3, -4)]
dta = data.table (dta)
str (dta)
dta$datum <- dta$date <- dta$time <- NULL
dta$UTC <-  NULL
k = cor(dta, use = "pairwise.complete.obs")
corrplot(k, method = "color", type = "upper", use = "pairwise.complete.obs", tl.cex = 0.6)
corrplot(k, method = "number", type = "upper", use = "pairwise.complete.obs", tl.cex = 0.5)


dta1 = data.table (adcast)
adcast <- readRDS(file = "data/adcast.rds")
dta1$DATE2 <- dta1$ID <- dta1$PLOT <- dta1$event <- dta1$CAW <-NULL
dta1$stat <- dta1$event_1 <-NULL
k = cor(dta1, use = "pairwise.complete.obs")

#cor.test(adcast$Tdiff_value, adcast$T_value, use = "pairwise.complete.obs" )

corrplot(k, order = "AOE", type = "upper", use = "pairwise.complete.obs", tl.cex = 0.5)

#daily data basic variables
dta_daily <- readRDS("C:/Users/marketa.souckova/OneDrive - CZU v Praze/R/Krkonose/Rcode/RDS/denni_data.rds")
dta_daily = data.table (dta_daily)
str (dta_daily)
dta_daily$Date <- NULL
k = cor(dta_daily, use = "pairwise.complete.obs")
corrplot(k, method = "color", use = "pairwise.complete.obs")
corrplot(k, method = "number", type = "upper", use = "pairwise.complete.obs")
summary (dta_daily)by = year
#daily data
dta_daily_all = data.table (adcast_all)
str (dta_daily_all)
dta_daily_all$Date <- dta_daily_all$ID <- dta_daily_all$PLOT <- dta_daily_all$event <- dta_daily_all$CAW <- dta_daily_all$stat <-NULL
k = cor(dta_daily_all, use = "pairwise.complete.obs")
corrplot(k, order = "AOE", type = "upper", use = "pairwise.complete.obs", tl.cex = 0.5)


sum(value, na.rm = T)
precip <- list()
hours <- c(24,48,72,96,120,144)
for (i in c(1:length(hours))){
  a <- aval_melt[PLOT %between% c(1, hours[i]) & var %in% c("SRA1H") & variable == "value", {sum = sum(value, na.rm = T); T0 = value[1] ; list(sum = sum, T0 = T0)}, by = .(ID, event, var, CAW)]
  a$H <- hours[i]
  precip[[i]] <- a
  print(hours[i])
}
precip <- rbindlist(precip)

library(corrplot)
library(reshape2)
library(PerformanceAnalytics)

