library(data.table)
library(zoo)
library(readr)
library(zoo)
library(circular)
library (readxl)
library (dplyr)

setwd ("C:/Users/souckovamarketa/OneDrive - CZU v Praze/R/Avalanche/Dat/")

################### AVAL DATA ######################

aval <- data.table(read.table("c:/Users/souckovamarketa/OneDrive - CZU v Praze/R/Avalanche/Dat/avalanche_event1.txt", header = T, sep = "\t"))

str(aval)
aval$date <- as.character(aval$date)
aval$date2 <- as.Date(aval$date, format = "%d.%m.%Y")

W_aval <- c(24:37)
E_aval <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,38,39)

aval[, locality := ifelse(cadastr_number %in% W_aval, 'W', 'E')]
aval_lbou <-aval[event == 1]
# aval_lbou <- aval[locality == "W",]
# aval_lucb <- aval[locality == "E",]

non_aval <- data.table(read.table("c:/Users/souckovamarketa/OneDrive - CZU v Praze/R/Avalanche/Dat/avalanche_event1_0.txt", header = T, sep = "\t"))

#non_aval <- data.table(read.table("c:/Users/Moravec/Desktop/avalanche_event1_0.txt", header = T, sep = "\t"))
non_aval$date <- as.character(non_aval$date)
non_aval$date2 <- as.Date(non_aval$date, format = "%d.%m.%Y")

#non_aval_lucb <- non_aval[!date2 %in% aval_lucb$date2 & month(date2) %in% c(10, 11, 12, 1, 2, 3, 4, 5) & event == 0]
non_aval_lbou <- non_aval[!date2 %in% aval_lbou$date2 & month(date2) %in% c(10, 11, 12, 1, 2, 3, 4, 5) & event == 0]

dates <- list()
for (i in 1:nrow(aval_lbou)){
  a <- seq.Date(from = as.Date(aval_lbou[i, date2]) - 6, to = aval_lbou[i, date2] + 6, by = "day")
  dates[[i]] <- a
}
dates_fin <- unique(as.Date(unlist(dates)))

non_aval_lbou <- non_aval_lbou[!date2 %in% dates_fin]
#non_aval_lucb <- non_aval_lucb[!date2 %in% dates_fin]

non_aval_lbou[, locality:= "W"]
non_aval_lbou <-non_aval_lbou[event == 0]
#non_aval_lucb[, locality:= "E"]

#aval_total_lucb <- rbind(aval_lucb, non_aval_lucb)
aval_total_lbou <- rbind(aval_lbou, non_aval_lbou)

#aval_total_lucb$ID <- paste0("Aval ", 1:nrow(aval_total_lucb))
aval_total_lbou$ID <- paste0("Aval ", 1:nrow(aval_total_lbou))

########### METEO DATA ###############

dta <- readRDS("C:/Users/souckovamarketa/OneDrive - CZU v Praze/R/Avalanche/Dat/Meteo_4Aval.RDS")
dta <-dta %>% select("Date", "F", "SCE", "SNO", "SSV", "SRA", "Tair", "H", "Rain_Tw", "Rain_Ta", "D_mean_ok", "SWE")
colnames(dta) <- c("Date", "WSavg", "SD", "NSS", "SLd", "P", "Tair", "H", "Rain_Tw", "Rain_Ta", "WD", "SWE")
saveRDS(dta, file = "C:/Users/souckovamarketa/OneDrive - CZU v Praze/R/Avalanche/Dat/dta.rds")

dta_all <- dta
###### adding missing dates ovlivní to mnozství nelavin? pác puvodne byly nekteré datumy vymazány?
datum <- data.frame(Date=seq.Date(from = as.Date("1962-01-01"), to = as.Date("2020-10-31"), by = "day"))
dta_all <- merge(x = dta_all, y = datum, by.x = "Date", by.y = "Date", all.y = T)

saveRDS(dta_all, file = "C:/Users/souckovamarketa/OneDrive - CZU v Praze/R/Avalanche/Dat/data_daily_1962_2020.rds")
#dta_all <- readRDS("./data_daily_1962_2020.rds")

dta_all <- dta_all[Date >= as.Date("1962-07-01")]

################# WARM COLD ##################
library(readxl)
setwd ("C:/Users/souckovamarketa/OneDrive - CZU v Praze/R/Avalanche/Dat")

brks <- read_excel("./W_C_events_dates_op.xlsx", 
                   col_types = c("numeric", "text", "numeric", 
                                 "numeric", "numeric", "text", "text", 
                                 "text"))

brks <- as.Date(brks$toR, format = "%Y-%m-%d")
brks <- na.omit(brks)

end.brks <- seq.Date(from = as.Date("1962-06-30"), to = as.Date("2020-06-30"), by = "year")

fin.brks <- sort(c(brks, end.brks))


dta_all[, CAW:= cut(x = Date, breaks = fin.brks, labels = c(1:(length(fin.brks)-1)))]

dta_all$CAW <- as.numeric(dta_all$CAW)

dta_all[is.na(CAW), CAW:= 113]

dta_all[,.(min(Date), max(Date)), by= CAW]

labs <- rep(c("cold", "warm"), 57)[c(-114,-113)]

dta_all[, CAW2:= cut(x = Date, breaks = fin.brks, labels = labs)]

dta_all[is.na(CAW2), CAW2:= "warm"]

dta_all$CAW <- NULL
colnames(dta_all)[ncol(dta_all)] <- "CAW"

dta_melt <- melt(dta_all, id.vars = c("Date", "CAW")) 
setwd ("C:/Users/souckovamarketa/OneDrive - CZU v Praze/R/Avalanche/Dat")


#mooving averadge 3,6 days
library (circular)
circ <- function(x){
  a <- mean(circular(x, units = 'degrees'), na.rm = T)[[1]]
  a[which(a < 0)] <- a[which(a < 0)] +360
  return(a)
}

days <-  c(3,6)
for (i in c(1:length(days))){
  sum_colname <- paste0("value", days[i], "_sum")
  mean_colname <- paste0("value", days[i], "_mean")
  circmean_colname <- paste0("value", days[i], "_circmean")
  colname <- paste0("value", days[i])
  dta_melt[, c(sum_colname) := rollsum(value, days[i], na.rm = T, align = "right", fill = NA), by = .(variable)]
  dta_melt[, c(mean_colname) := rollmean(value, days[i], na.rm = T, align = "right", fill = NA), by = .(variable)]
  dta_melt[, c(circmean_colname) := rollapply(data = value, width = days[i], FUN = circ, align = "right", fill = NA), by = .(variable)]
  #dta_melt[, c(colname) := ifelse(variable %in% c("P", "SLd"), get(sum_colname), get(mean_colname))]#what about wind?
  dta_melt[, c(colname) := if (variable %in% c("P", "SLd")) { get(sum_colname) }
                           else if (variable %in% c("WD")) { get(circmean_colname)}
                           else { get(mean_colname) } ]#what about wind?
  dta_melt[, c(sum_colname):= NULL ]
  dta_melt[, c(mean_colname):= NULL ]
  dta_melt[, c(circmean_colname):= NULL ]
  gc()
  print(days[i])
}
saveRDS(dta_all, file = "C:/Users/souckovamarketa/OneDrive - CZU v Praze/R/Avalanche/Dat/dta_melt_daily.rds")
#dta_melt <- readRDS(file = "./dta_melt_daily.rds")

############## SIX DAYS LBOU #########

aval_total_lbou <- aval_total_lbou[date2 >= dta_melt[, min(Date)] + 6 & date2 <= as.Date("2020-10-31"), ]

aval_total_C_lbou <- aval_total_lbou[C == 2 | event == 0, ]
aval_total_A_lbou <- aval_total_lbou[A %in% c(2, 3, 4) | event == 0, ]

## Wet avalanches lbou, C = 2
aval_lbou_C_list <- list()

for (i in 1:nrow(aval_total_C_lbou)){
  id <- aval_total_C_lbou[i,ID]
  start <- aval_total_C_lbou[i,date2]
  stop <- aval_total_C_lbou[i,date2] - 6
  dta_aval <- dta_melt[Date %between% c(stop[1], start[1]) ]
  dta_aval[, PLOT:= c(1:.N), by = variable]
  dta_aval$ID <- id
  aval_lbou_C_list[[i]] <- dta_aval
  print(i)
  
}
#10207 records
aval_lbou_C_dtafr <- rbindlist(aval_lbou_C_list)
aval_lbou_C_dtafr <- merge(x = aval_lbou_C_dtafr, y = aval_total_C_lbou[,.(event,ID)], by = "ID")

#############

## Slab avalanches lbou, A = 2,3,4
aval_lbou_A_list <- list()

for (i in 1:nrow(aval_total_A_lbou)){
  id <- aval_total_A_lbou[i,ID]
  start <- aval_total_A_lbou[i,date2]
  stop <- aval_total_A_lbou[i,date2] - 6
  dta_aval <- dta_melt[Date %between% c(stop[1], start[1]) ]
  dta_aval[, PLOT:= c(1:.N), by = variable]
  dta_aval$ID <- id
  aval_lbou_A_list[[i]] <- dta_aval
  print(i)
  
}
#10856
aval_lbou_A_dtafr <- rbindlist(aval_lbou_A_list)
aval_lbou_A_dtafr <- merge(x = aval_lbou_A_dtafr, y = aval_total_A_lbou[,.(event,ID)], by = "ID")

#############

#### All avalanches together LBOU
aval_lbou_list <- list() # upravit
#change colum number to Aval Id

for (i in 1:nrow(aval_total_lbou)){
  id <- aval_total_lbou[i,ID]
  start <- aval_total_lbou[i,date2] # sloupec
  stop <- aval_total_lbou[i,date2] - 6
  dta_aval <- dta_melt[Date %between% c(stop[1], start[1]) , ]
  dta_aval[, PLOT:= c(1:.N), by = variable]
  dta_aval$ID <- id 
  aval_lbou_list[[i]] <- dta_aval
  print(i)
}
#11230
aval_lbou_dtafr <- rbindlist(aval_lbou_list)
aval_lbou_dtafr <- merge(x = aval_lbou_dtafr, y = aval_total_lbou[,.(event,ID)], by = "ID")

colnames(aval_lbou_A_dtafr)[4] <- "var"
colnames(aval_lbou_C_dtafr)[4] <- "var"
colnames(aval_lbou_dtafr)[4] <- "var"

aval_lbou_melt <- melt(data = aval_lbou_dtafr, id.vars = c("ID", "Date", "PLOT", "var", "CAW", "event"))
aval_lbou_A_melt <- melt(data = aval_lbou_A_dtafr, id.vars = c("ID", "Date", "PLOT", "var", "CAW", "event"))
aval_lbou_C_melt <- melt(data = aval_lbou_C_dtafr, id.vars = c("ID", "Date", "PLOT", "var", "CAW", "event"))

aval_lbou_melt$type <- "all" 
aval_lbou_A_melt$type <- "slab" 
aval_lbou_C_melt$type <- "wet"

aval_melt_lbou_total <- rbind(aval_lbou_melt, aval_lbou_A_melt, aval_lbou_C_melt)

aval_melt_lbou_total$stat <- "LBOU"

aval_melt_total <- aval_melt_lbou_total
aval_melt_total[, var_name:= paste0(var, "_", variable)]

setwd ("C:/Users/souckovamarketa/OneDrive - CZU v Praze/R/Avalanche/Bin/daily")

adcast_A <- dcast.data.table(aval_melt_total[type == "slab"], Date + ID + PLOT + event + CAW + stat  ~ var_name, value.var = 'value')
adcast_C <- dcast.data.table(aval_melt_total[type == "wet"], Date + ID + PLOT + event + CAW + stat  ~ var_name, value.var = 'value')
adcast_all <- dcast.data.table(aval_melt_total[type == "all"], Date + ID + PLOT + event + CAW + stat  ~ var_name, value.var = 'value')

saveRDS(adcast_A, "./adcast_A_daily.rds")
saveRDS(adcast_C, "./adcast_C_daily.rds")
saveRDS(adcast_all, ".adcast_all_daily.rds")

#readRDS("./adcast_A_daily.rds")
#readRDS("./adcast_C_daily.rds")
#adcast_C_daily <- readRDS("~/laviny/data/adcast_C_daily.rds")
#readRDS("./data/adcast_all_daily.rds")


