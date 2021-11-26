library(data.table)
library(readr)
library(zoo)
library(circular)

circ <- function(x){
  a <- mean(circular(x, units = 'degrees'), na.rm = T)[[1]]
  a[which(a < 0)] <- a[which(a < 0)] +360
  return(a)
}
setwd("C:/Users/Marketa/OneDrive - CZU v Praze/R/Krkonose/laviny")

setwd("C:/Users/Marketa/OneDrive - CZU v Praze/R/Avalanche/Data")

setwd("D:/marketa2/marketa/Hourly_new")
################### AVAL DATA ######################
Aval <- data.table(read_delim("./Aval_utf_8.txt", 
                              "\t", escape_double = FALSE, col_types = cols( A = col_character(), 
                                                                             B = col_character(), C = col_character(), 
                                                                             D = col_character(), E = col_character(), 
                                                                             F = col_character(), G = col_character(), 
                                                                             H = col_character(), J = col_character(), 
                                                                             K = col_number(), Kmax = col_number(),
                                                                             L = col_number(), 
                                                                             M = col_number(), Mmax = col_number(),
                                                                             N = col_number(), 
                                                                             O = col_number(), 
                                                                             cadastr_letter = col_character(), 
                                                                             cadastr_number = col_number(), date = col_character(), 
                                                                             day = col_number(), event = col_number(),
                                                                             exposure = col_character(), locality = col_character(), 
                                                                             month = col_number(), notes = col_character(), 
                                                                             ranking = col_number(), season = col_character(), 
                                                                             year = col_number()), trim_ws = TRUE, locale = locale(encoding = "windows-1252")))
problems(Aval)
Aval <- Aval[!is.na(date)]

Aval$DATE2 <- as.Date(strtrim(x = Aval$date, width = 10))
Aval$DATE3 <- as.POSIXct(paste0(Aval$DATE2, " 07:00"), format = "%Y-%m-%d %H:%M")
Aval$DATE_OFF <- as.POSIXct(Aval$DATE3 + (17*60*60))

W_aval <- c(24:37)
E_aval <- c(1,2,3,4, 5, 6, 7,8,9,10,11,12, 13,14,15,16,17,18,19,20,21,22,23,38,39)
Aval[, locality := ifelse(cadastr_number %in% W_aval, 'W', 'E')]
aval_lbou <- Aval[locality == "W",]
#aval_lucb <- Aval[locality == "E",]
aval_lbou[event == 1]
#aval_lucb <- aval_lucb[event == 1]

#non_aval_lucb <- Aval[year(DATE3) > 2008 & month(DATE3) %in% c(10, 11, 12, 1, 2, 3, 4, 5) & event == 0,]
non_aval_lbou <- Aval[year(DATE3) > 2003 & month(DATE3) %in% c(10, 11, 12, 1, 2, 3, 4, 5) & event == 0,]

dates <- list()
for (i in 1:nrow(aval_lbou)){
  a <- seq.Date(from = as.Date(aval_lbou[i, DATE2]) - 6, to = as.Date(aval_lbou[i, DATE2]) + 6, by = "day")
  dates[[i]] <- a
}
dates_fin <- unique(as.Date(unlist(dates)))

non_aval_lbou <- non_aval_lbou[!DATE2 %in% dates_fin]

non_aval_lbou[, locality:= "W"]
#non_aval_lucb[, locality:= "E"]

aval_lbou <- aval_lbou[year(DATE3) > 2003]#194
#aval_lucb <- aval_lucb[year(DATE3) > 2008]#85?

#aval_total_lucb <- rbind(aval_lucb, non_aval_lucb)#3161
aval_total_lbou <- rbind(aval_lbou, non_aval_lbou)#4378

#aval_total_lucb$ID <- paste0("Aval ", 1:nrow(aval_total_lucb))
aval_total_lbou$ID <- paste0("Aval ", 1:nrow(aval_total_lbou))

########### METEO DATA ###############
daily <- readRDS("D:/marketa2/marketa/data_daily_1962_2020.rds")
daily$UTC <- as.POSIXct(paste0(as.character(daily$Date), " 12:00:00"), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
daily <- daily[,c(3,4,13,14)]

dta_lbou <- readRDS("D:/marketa2/marketa/Hourly_new/data/hourly_data_2004_2020.RDS")

colnames(dta_lbou) <- c("Date", "SD", "NSS", "SWE", "H", "WD", "WSavg","WSmax", "Tair", "P", "SLd", "time", "Date_min", "RBLB", "Tsoil05", "Rain_Tw", "Rain_Ta")
dta_lbou$time <- dta_lbou$RBLB <- dta_lbou$WSmax <- dta_lbou$Tsoil05 <-NULL

dates <- gsub(pattern = " UTC", replacement = "", x = dta_lbou$Date_min)
dta_lbou$UTC <- as.POSIXct(x = dates, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
dta_lbou$Date <- dta_lbou$Date_min <- dta_lbou$SD <- dta_lbou$NSS <- dta_lbou$SWE <- NULL 

dta_lbou <- merge(x = daily, y = dta_lbou, by = "UTC", all.y = T)
# what is this for?
NonNAindexSD <- which(!is.na(dta_lbou$SD))
NonNAindexNSS <- which(!is.na(dta_lbou$NSS))
NonNAindexSWE <- which(!is.na(dta_lbou$SWE))

dta_lbou$SD[min(NonNAindexSD):max(NonNAindexSD)] <- na.approx(object = dta_lbou$SD)
dta_lbou$NSS[min(NonNAindexNSS):max(NonNAindexNSS)] <- na.approx(object = dta_lbou$NSS)
dta_lbou$SWE[min(NonNAindexSWE):max(NonNAindexSWE)] <- na.approx(object = dta_lbou$SWE)

###### WARM COLD ######

library(readxl)
brks <- read_excel("D:/marketa2/marketa/W_C_events_dates_op.xlsx", 
                   col_types = c("numeric", "text", "numeric", 
                                 "numeric", "numeric", "text", "text", 
                                 "text"))

#brks <- read_excel("c:/Users/marketa.souckova/Documents/laviny/daily/W_C_events_dates_op.xlsx", 
#                   col_types = c("numeric", "text", "numeric", 
#                                 "numeric", "numeric", "text", "text", 
#                                 "text", "text", "text", "text"))
brks <- as.Date(brks$toR, format = "%Y-%m-%d")
brks <- na.omit(brks)

brks <- brks[which(brks >= as.Date("2003-04-11"))]

end.brks <- seq.Date(from = as.Date("2004-06-30"), to = as.Date("2020-06-30"), by = "year")

fin.brks <- sort(c(brks, end.brks, as.Date("2020-11-01")))

dta_lbou <- data.table(dta_lbou)
dta_lbou[, CAW:= cut(x = as.Date(UTC), breaks = fin.brks, labels = c(1:(length(fin.brks)-1)))]

labs <- rep(c("cold", "warm"), 18)[-36]

dta_lbou[, CAW2:= cut(x = as.Date(UTC), breaks = fin.brks, labels = labs)]

dta_lbou[,.(min(as.Date(UTC)), max(as.Date(UTC))), by= .(CAW,CAW2)]

dta_lbou$CAW <- NULL
colnames(dta_lbou)[ncol(dta_lbou)] <- "CAW"
#setwd ("c:/Users/marketa.souckova/Documents/laviny/")
dta_melt <- melt(dta_lbou, id.vars = c("UTC", "CAW")) # this is probably not needed to change, since the needed for cycles are not using this dta for aval info

############# ROLL DATA ###########

hours <-  c(24,72,144)# adjusted according to daily data steps
for (i in c(1:length(hours))){
  sum_colname <- paste0("value", hours[i], "_sum")
  mean_colname <- paste0("value", hours[i], "_mean")
  circmean_colname <- paste0("value", hours[i], "_circmean")
  colname <- paste0("value", hours[i])
  dta_melt[, c(sum_colname) := rollsum(value, hours[i], na.rm = T, align = "right", fill = NA), by = .(variable)]
  message("rollsum calculated...")
  dta_melt[, c(mean_colname) := rollmean(value, hours[i], na.rm = T, align = "right", fill = NA), by = .(variable)]
  message("rollmean calculated...")
  dta_melt[, c(circmean_colname) := rollapply(data = value, width = hours[i], FUN = circ, align = "right", fill = NA), by = .(variable)]
  message("rollcircle calculated...")
  dta_melt[, c(colname) := if (variable %in% c("P", "SLd")) { get(sum_colname) }
           else if (variable %in% c("WD")) { get(circmean_colname)}
           else { get(mean_colname) } ]#what about wind?
  dta_melt[, c(sum_colname):= NULL ]
  dta_melt[, c(mean_colname):= NULL ]
  dta_melt[, c(circmean_colname):= NULL ]
  gc()
  print(hours[i])
}

############## SIX DAYS LBOU #########

aval_total_lbou <- aval_total_lbou[DATE_OFF >= dta_melt[, min(UTC)] + 5*24*60*60 & DATE_OFF <= as.POSIXct("2020-10-31"), ]

aval_total_C_lbou <- aval_total_lbou[ C == 2 | event == 0, ]
aval_total_A_lbou <- aval_total_lbou[ A %in% c(2, 3, 4) | event == 0, ]

## Wet avalanches lbou, C = 2
aval_lbou_C_list <- list()
#sloupec <- which(colnames(aval_total_C_lbou) == "ID")  # Order of column "ID"
#sloupec1 <- which(colnames(aval_total_C_lbou) == "DATE_OFF") # Order of column "DATE_OFF"

for (i in 1:nrow(aval_total_C_lbou)){
  id <- aval_total_C_lbou[i,ID]
  start <- aval_total_C_lbou[i,DATE_OFF]
  stop <- aval_total_C_lbou[i,DATE_OFF] - 5*24*60*60
  dta_aval <- dta_melt[ as.Date(UTC) %between% c(as.Date(stop[1]), as.Date(start[1])), ]
  dta_aval[, PLOT:= c(1:.N), by = variable]
  dta_aval$ID <- id
  aval_lbou_C_list[[i]] <- dta_aval
  print(i)
  
}

aval_lbou_C_dtafr <- rbindlist(aval_lbou_C_list)
aval_lbou_C_dtafr <- merge(x = aval_lbou_C_dtafr, y = aval_total_C_lbou[,.(event,ID)], by = "ID")

#############

## Slab avalanches lbou, A = 2,3,4
aval_lbou_A_list <- list()
#sloupec <- which(colnames(aval_total_A_lbou) == "ID")  # Order of column "ID"
#sloupec1 <- which(colnames(aval_total_A_lbou) == "DATE_OFF") # Order of column "DATE_OFF"

for (i in 1:nrow(aval_total_A_lbou)){
  id <- aval_total_A_lbou[i,ID]
  start <- aval_total_A_lbou[i,DATE_OFF]
  stop <- aval_total_A_lbou[i,DATE_OFF] - 5*24*60*60
  dta_aval <- dta_melt[ as.Date(UTC) %between% c(as.Date(stop[1]), as.Date(start[1])) ]
  dta_aval[, PLOT:= c(1:.N), by = variable]
  dta_aval$ID <- id
  aval_lbou_A_list[[i]] <- dta_aval
  print(i)
  
}

aval_lbou_A_dtafr <- rbindlist(aval_lbou_A_list)
aval_lbou_A_dtafr <- merge(x = aval_lbou_A_dtafr, y = aval_total_A_lbou[,.(event,ID)], by = "ID")

#############

#############NAMES FOR ALL AVAL
colnames(aval_lbou_C_dtafr)[4] <- "var"
colnames(aval_lbou_A_dtafr)[4] <- "var"

aval_lbou_C_melt <- melt(data = aval_lbou_C_dtafr, id.vars = c("ID", "UTC", "PLOT", "var", "CAW", "event"))
aval_lbou_A_melt <- melt(data = aval_lbou_A_dtafr, id.vars = c("ID", "UTC", "PLOT", "var", "CAW", "event"))

aval_lbou_C_melt[, var_name:= paste0(var, "_", variable)]
aval_lbou_A_melt[, var_name:= paste0(var, "_", variable)]

adcast_A <- dcast.data.table(aval_lbou_A_melt, UTC + ID + PLOT + event + CAW + event  ~ var_name, value.var = 'value')
adcast_C <- dcast.data.table(aval_lbou_C_melt, UTC + ID + PLOT + event + CAW + event  ~ var_name, value.var = 'value')

saveRDS(object = adcast_A, file = "./data/adcast_A.rds")
saveRDS(object = adcast_C, file = "./data/adcast_C.rds")
