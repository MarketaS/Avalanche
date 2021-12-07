library(data.table)
library(zoo)
#setwd ("c:/Users/marketa.souckova/Documents/laviny/daily/")
setwd ("C:/Users/souckovamarketa/OneDrive - CZU v Praze/R/Krkonose/Rcode/RDS/")

################### AVAL DATA ######################

aval <- data.table(read.table("c:/Users/souckovamarketa/OneDrive - CZU v Praze/R/Krkonose/laviny/daily/avalanche_event1.txt", header = T, sep = "\t"))

str(aval)
aval$date <- as.character(aval$date)
aval$date2 <- as.Date(aval$date, format = "%d.%m.%Y")

W_aval <- c(24:37)
E_aval <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,38,39)

aval[, locality := ifelse(cadastr_number %in% W_aval, 'W', 'E')]

aval_lbou <- aval[locality == "W",]
aval_lucb <- aval[locality == "E",]

non_aval <- data.table(read.table("c:/Users/souckovamarketa/OneDrive - CZU v Praze/R/Krkonose/laviny/daily/avalanche_event1_0.txt", header = T, sep = "\t"))

#non_aval <- data.table(read.table("c:/Users/Moravec/Desktop/avalanche_event1_0.txt", header = T, sep = "\t"))
non_aval$date <- as.character(non_aval$date)
non_aval$date2 <- as.Date(non_aval$date, format = "%d.%m.%Y")

non_aval_lucb <- non_aval[!date2 %in% aval_lucb$date2 & month(date2) %in% c(10, 11, 12, 1, 2, 3, 4, 5) & event == 0]
non_aval_lbou <- non_aval[!date2 %in% aval_lbou$date2 & month(date2) %in% c(10, 11, 12, 1, 2, 3, 4, 5) & event == 0]

dates <- list()
for (i in 1:nrow(aval_lbou)){
  a <- seq.Date(from = as.Date(aval_lbou[i, date2]) - 6, to = aval_lbou[i, date2] + 6, by = "day")
  dates[[i]] <- a
}
dates_fin <- unique(as.Date(unlist(dates)))

dates <- list()
for (i in 1:nrow(aval_lucb)){
  a <- seq.Date(from = as.Date(aval_lucb[i, date2]) - 6, to = aval_lucb[i, date2] + 6, by = "day")
  dates[[i]] <- a
}
dates_fin <- unique(as.Date(unlist(dates)))

non_aval_lbou <- non_aval_lbou[!date2 %in% dates_fin]
non_aval_lucb <- non_aval_lucb[!date2 %in% dates_fin]

non_aval_lbou[, locality:= "W"]
non_aval_lucb[, locality:= "E"]

aval_total_lucb <- rbind(aval_lucb, non_aval_lucb)
aval_total_lbou <- rbind(aval_lbou, non_aval_lbou)

aval_total_lucb$ID <- paste0("Aval ", 1:nrow(aval_total_lucb))
aval_total_lbou$ID <- paste0("Aval ", 1:nrow(aval_total_lbou))

########### METEO DATA ###############

#dta <- dataRDS(readRDS("c:/Users/souckovamarketa/Documents/laviny/daily/Meteo_4Aval.RDS"))
dta <- readRDS("C:/Users/souckovamarketa/OneDrive - CZU v Praze/R/Avalanche/Dat/Meteo_4Aval.RDS")

dta <- dta[,-which(grepl(pattern = "Station", colnames(dta)))]
dta <- dta[,-which(grepl(pattern = "note", colnames(dta)))]
dta <- dta[,-which(grepl(pattern = "grad", colnames(dta)))]
colnames(dta) <- c("Date", "WSavg", "SD", "NSS", "SLd", "P", "Tair", "H", "Rain_Tw", "Rain_Ta", "WD", "SWE")
saveRDS(dta, file = "C:/Users/souckovamarketa/OneDrive - CZU v Praze/R/Avalanche/Dat/dta.rds")


# dta2 <- data.table(read.table("c:/Users/souckovamarketa/OneDrive - CZU v Praze/R/Avalanche/Dat/LBOU_daily_1961_2020.txt", header = T, sep = ","))
# dta2 <- dta2[, .(date, SVH)]
# colnames(dta2) <- c("date", "SWE")
# dta2$date2 <- as.Date(as.character(dta2$date), format = "%Y-%m-%d")
# dta2$date <- NULL
# 
# dta2_fill <- dta2[date2 %between% c("1980-01-07", "2020-10-26")]
# ###### linear interpolation of SWE
# dta2_fill$SWE2 <- na.approx(object = dta2_fill$SWE)
# dta2$SWE2 <- dta2$SWE
# dta2_all <- rbind(dta2[date2 < as.Date("1980-01-06")], dta2_fill, dta2[date2 > as.Date("2020-10-26")])
# dta2_all$SWE <- NULL
# colnames(dta2_all) <- c("date2", "SWE")
# 
# dta_all <- data.table(merge(x = dta, y = dta2_all, by.x = "Date", by.y = "date2", all.x = T))
dta_all <- dta
###### adding missing dates
datum <- data.frame(Date=seq.Date(from = as.Date("1961-01-01"), to = as.Date("2020-10-31"), by = "day"))
dta_all <- merge(x = dta_all, y = datum, by.x = "Date", by.y = "Date", all.y = T)

saveRDS(dta_all, file = "C:/Users/souckovamarketa/OneDrive - CZU v Praze/R/Avalanche/Dat/data_daily_1961_2020.rds")

#dta_all <- readRDS("C:/Users/marketa.souckova/OneDrive - CZU v Praze/R/Krkonose/Rcode/RDS/data_daily_1962_2020.rds")

dta_all <- readRDS("./data_daily_1961_2020.rds")


#colnames(dta_all)[which(colnames(dta_all) == "D_mean")] <- "WD"
dta_all <- dta_all[Date >= as.Date("1962-07-01")]# 

################# WARM COLD ##################
setwd ("C:/Users/souckovamarketa/OneDrive - CZU v Praze/R/Krkonose/laviny/daily")
library(readxl)
brks <- read_excel("./W_C_events_dates_op.xlsx", 
                   col_types = c("numeric", "text", "numeric", 
                                 "numeric", "numeric", "text", "text", 
                                 "text"))

#brks <- read_excel("c:/Users/marketa.souckova/Documents/laviny/daily/W_C_events_dates_op.xlsx", 
                   col_types = c("numeric", "text", "numeric", 
                                 "numeric", "numeric", "text", "text", 
                                 "text", "text", "text", "text"))
brks <- as.Date(brks$toR, format = "%Y-%m-%d")
brks <- na.omit(brks)

end.brks <- seq.Date(from = as.Date("1962-06-30"), to = as.Date("2020-06-30"), by = "year")

fin.brks <- sort(c(brks, end.brks))

#dta_all <- data.table(dta_all)
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
setwd ("C:/Users/Marketa/OneDrive - CZU v Praze/R/Krkonose/Rcode/RDS/")

saveRDS(dta_melt, "./dta_melt_daily.rds")
dta_melt <- readRDS(file = "./dta_melt_daily.rds")
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

############## SIX DAYS LUCB #########

aval_total_lucb <- aval_total_lucb[date2 >= dta_melt[, min(Date)] + 10 & date2 <= as.Date("2020-10-31"), ]

aval_total_C_lucb <- aval_total_lucb[ C == 2 | event == 0, ]
aval_total_A_lucb <- aval_total_lucb[ A %in% c(2, 3, 4) | event == 0, ]

## Wet avalanches LUCB, C = 2
aval_lucb_C_list <- list()

for (i in 1:nrow(aval_total_C_lucb)){
  id <- aval_total_C_lucb[i,ID]
  start <- aval_total_C_lucb[i,date2]
  stop <- aval_total_C_lucb[i,date2] - 6
  dta_aval <- dta_melt[Date %between% c(stop[1], start[1]) ]
  dta_aval[, PLOT:= c(1:.N), by = variable]
  dta_aval$ID <- id
  aval_lucb_C_list[[i]] <- dta_aval
  print(i)
  
}
#10896
aval_lucb_C_dtafr <- rbindlist(aval_lucb_C_list)
aval_lucb_C_dtafr <- merge(x = aval_lucb_C_dtafr, y = aval_total_C_lucb[,.(event,ID)], by = "ID")

#############

## Slab avalanches LUCB, A = 2,3,4
aval_lucb_A_list <- list()

for (i in 1:nrow(aval_total_A_lucb)){
  id <- aval_total_A_lucb[i,ID]
  start <- aval_total_A_lucb[i,date2]
  stop <- aval_total_A_lucb[i,date2] - 6
  dta_aval <- dta_melt[Date %between% c(stop[1], start[1]) ]
  dta_aval[, PLOT:= c(1:.N), by = variable]
  dta_aval$ID <- id
  aval_lucb_A_list[[i]] <- dta_aval
  print(i)
  
}
#11360
aval_lucb_A_dtafr <- rbindlist(aval_lucb_A_list)
aval_lucb_A_dtafr <- merge(x = aval_lucb_A_dtafr, y = aval_total_A_lucb[,.(event,ID)], by = "ID")

#############

#### All avalanches together LUCB - original
aval_lucb_list <- list() 

for (i in 1:nrow(aval_total_lucb)){
  id <- aval_total_lucb[i,ID]
  start <- aval_total_lucb[i,date2]
  stop <- aval_total_lucb[i,date2] - 6
  dta_aval <- dta_melt[Date %between% c(stop[1], start[1]) ]
  dta_aval[, PLOT:= c(1:.N), by = variable]
  dta_aval$ID <- id
  aval_lucb_list[[i]] <- dta_aval
  print(i)
  
}
#11546
aval_lucb_dtafr <- rbindlist(aval_lucb_list)
aval_lucb_dtafr <- merge(x = aval_lucb_dtafr, y = aval_total_lucb[,.(event,ID)], by = "ID")


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
#10852
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
#11037
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
#11225
aval_lbou_dtafr <- rbindlist(aval_lbou_list)
aval_lbou_dtafr <- merge(x = aval_lbou_dtafr, y = aval_total_lbou[,.(event,ID)], by = "ID")

colnames(aval_lucb_A_dtafr)[4] <- "var"
colnames(aval_lucb_C_dtafr)[4] <- "var"
colnames(aval_lucb_dtafr)[4] <- "var"

colnames(aval_lbou_A_dtafr)[4] <- "var"
colnames(aval_lbou_C_dtafr)[4] <- "var"
colnames(aval_lbou_dtafr)[4] <- "var"

aval_lucb_melt <- melt(data = aval_lucb_dtafr, id.vars = c("ID", "Date", "PLOT", "var", "CAW", "event"))
aval_lucb_A_melt <- melt(data = aval_lucb_A_dtafr, id.vars = c("ID", "Date", "PLOT", "var", "CAW", "event"))
aval_lucb_C_melt <- melt(data = aval_lucb_C_dtafr, id.vars = c("ID", "Date", "PLOT", "var", "CAW", "event"))

aval_lbou_melt <- melt(data = aval_lbou_dtafr, id.vars = c("ID", "Date", "PLOT", "var", "CAW", "event"))
aval_lbou_A_melt <- melt(data = aval_lbou_A_dtafr, id.vars = c("ID", "Date", "PLOT", "var", "CAW", "event"))
aval_lbou_C_melt <- melt(data = aval_lbou_C_dtafr, id.vars = c("ID", "Date", "PLOT", "var", "CAW", "event"))

aval_lucb_melt$type <- "all"
aval_lucb_A_melt$type <- "slab"
aval_lucb_C_melt$type <- "wet"

aval_lbou_melt$type <- "all" 
aval_lbou_A_melt$type <- "slab" 
aval_lbou_C_melt$type <- "wet"

aval_melt_lucb_total <- rbind(aval_lucb_melt, aval_lucb_A_melt, aval_lucb_C_melt)
aval_melt_lbou_total <- rbind(aval_lbou_melt, aval_lbou_A_melt, aval_lbou_C_melt)
#aval_melt_lbou_total <- rbind(aval_lbou_A_melt, aval_lbou_C_melt)

aval_melt_lucb_total$stat <- "LUCB"
aval_melt_lbou_total$stat <- "LBOU"

aval_melt_total <- rbind(aval_melt_lucb_total, aval_melt_lbou_total)
#aval_melt_total <- aval_melt_lbou_total
aval_melt_total[, var_name:= paste0(var, "_", variable)]

setwd ("C:/Users/souckovamarketa/OneDrive - CZU v Praze/R/Avalanche/Bin/daily")

adcast_A <- dcast.data.table(aval_melt_total[type == "slab"], Date + ID + PLOT + event + CAW + stat  ~ var_name, value.var = 'value')
adcast_C <- dcast.data.table(aval_melt_total[type == "wet"], Date + ID + PLOT + event + CAW + stat  ~ var_name, value.var = 'value')
adcast_all <- dcast.data.table(aval_melt_total[type == "all"], Date + ID + PLOT + event + CAW + stat  ~ var_name, value.var = 'value')

#setwd ("C:/Users/Marketa/OneDrive - ?ZU v Praze/R/Krkonose/Rcode/RDS/")

saveRDS(adcast_A, "./adcast_A_daily.rds")
saveRDS(adcast_C, "./adcast_C_daily.rds")
saveRDS(adcast_all, ".adcast_all_daily.rds")

readRDS("./adcast_A_daily.rds")
readRDS("./adcast_C_daily.rds")
#adcast_C_daily <- readRDS("~/laviny/data/adcast_C_daily.rds")
#readRDS("./data/adcast_all_daily.rds")


