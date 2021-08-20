library(data.table)
library(readr)
library(ggplot2)
library(lubridate)
install.packages("writexl")
library(writexl)
setwd ("c:/Users/marketa.souckova/Documents/laviny/")
C:Users\Marketa\Documents\CZU\R\Krkonose
Aval <- data.table(read_delim("data/Aval_utf_8.txt", 
                              "\t", escape_double = FALSE, col_types = cols(date = col_character(), 
                                                                            ranking = col_number(), season = col_character(), 
                                                                            locality = col_character(), exposure = col_character(), 
                                                                            day = col_number(), month = col_number(), 
                                                                            year = col_number(), cadastr_number = col_number(), 
                                                                            cadastr_letter = col_character(), 
                                                                            A = col_number(), B = col_number(), 
                                                                            C = col_number(), D = col_number(), 
                                                                            E = col_number(), F = col_number(), 
                                                                            G = col_number(), H = col_number(), 
                                                                            J = col_number(), K = col_number(), 
                                                                            Kmax = col_number(), L = col_number(), 
                                                                            M = col_number(), Mmax = col_number(), 
                                                                            N = col_number(), O = col_number(), 
                                                                            notes = col_character(), event = col_number()), 
                              trim_ws = TRUE))
# Avalanche locality dedication# preppared but not included in a script right now, avalanche paths are recorded in cadastr_number 
W_aval <- c(24:37)
E_aval <- c(1,2,3,4, 5, 6, 7,8,9,10,11,12, 13,14,15,16,17,18,19,20,21,22,23,38,39)

Aval[, locality := ifelse(cadastr_number %in% W_aval, 'W', 'E')]
aval_lbou <- Aval[locality == "W",]
aval_lucb <- Aval[locality == "E",]

# LBOU data hourly (2004-2009)
dta <- data.table(read_delim("C:/Users/marketa.souckova/Documents/laviny/LBOU/data_hourly_2004_2020.txt", 
                             "\t", escape_double = FALSE, col_types = cols(D = col_double(), 
                                                                           Fmax = col_double(), Fprum = col_double(), 
                                                                           H = col_double(), RGLB1H = col_double(), 
                                                                           SCE = col_double(), SNO = col_double(), 
                                                                           SRA1H = col_double(), SSV1H = col_double(), 
                                                                           SVH = col_double(), T = col_double(), 
                                                                           T05 = col_double(), date = col_character(), 
                                                                           datum = col_date(format = "%Y-%m-%d"), 
                                                                           time = col_character()), trim_ws = TRUE))

dates <- gsub(pattern = " UTC", replacement = "", x = dta$date)
#dta$date <- NULL
dta$date <- as.POSIXct(x = dates, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

times <- dta$time
#dta$time <- NULL
dta$time <- as.POSIXct(x = times, format = "%Y-%m-%d %H:%M")

dta$DATE2 <- strtrim(x = as.character(dta$date), width = 10)# d?lka 10 znak? zbaven? se minut a sekund
dta$DATE2 <- as.POSIXct(paste(dta$date), format = "%Y-%m-%d")
#data_hourly_2004_2020 <- as.POSIXct(paste(data_hourly_2004_2020$date), format = "%d.%m.%Y %H:%M")

Aval$DATE2 <- strtrim(x = Aval$date, width = 10)# d?lka 10 znak? zbaven? se minut a sekund
Aval$DATE3 <- as.POSIXct(paste0(Aval$DATE2, " 07:00"), format = "%d.%m.%Y %H:%M")
Aval$DATE_OFF <- as.POSIXct(Aval$DATE3 + (17*60*60))# srovn?n? na 24 h od 7:00

Aval_short <- Aval[DATE3 %between% c(dta[1,DATE2] + 5*24*60*60, dta[nrow(dta),DATE2])]#dtum prvn? laviny a meteo 5 dn? p?ed
boxplot (Aval_short$N)

summary (Aval_short$N)

Aval_short$ID <- paste0("Aval ", 1:nrow(Aval_short))#vlo?en? sloupce v?dy? AVal_ a s ka?d?m ??dkem se bude p?ipisovat ??slo

#create list, s t?m ?e pro ka?dou lavinu v data table Aval_short

aval_list <- list()
for (i in 1:nrow(Aval_short)){
  id <- Aval_short[i,ID]#30 sloupec Aval 1,2,3...
  start = Aval_short[i,29]#29 sloupec pro ka?dou lavinu, datum
  stop =  Aval_short[i,29] - 5*24*60*60 #okno -5, form?t Posix pracuje se sekundami
  dta_aval <- dta[DATE2 %between% c(stop[1], start[1]) , ]#vezmi ??dky start, stop a pro ty mi vykresli meteo prom?nn?
  dta_aval$PLOT <- 1:nrow(dta_aval)# pro ka?dou ud?lost graf? kv?li ?emu? pro lep?? vykreslen? dat # K ?EMU PLOT?
  dta_aval$ID <- id # sloupec id asi definov?n? parametru?
  aval_list[[i]] <- dta_aval
  print(i)
}
#6183 lavin 2004-2020
aval_dtafr <- rbindlist(aval_list)
aval_dtafr$ranking <- aval_dtafr$date <- aval_dtafr$day <- aval_dtafr$month <- aval_dtafr$year <- aval_dtafr$datum <-aval_dtafr$time <- NULL #vymaz?n? nepot?ebn?ch sloupc?

aval_melt <- melt(data = aval_dtafr, id.vars = c("ID", "DATE2", "PLOT"))#
aval_melt[ , station:="LBOU"]
saveRDS(object = aval_melt, file = "C:/Users/marketa.souckova/Documents/laviny/aval_melt.rds")
aval_melt <- readRDS(file = "C:/Users/marketa.souckova/Documents/laviny/aval_melt.rds")

dta_LUCB <- data.table(read_delim("C:/Users/marketa.souckova/Documents/laviny/LBOU/LUCB_dta_hourly_2004_2020.txt", 
                                  "\t", escape_double = FALSE, col_types = cols(datum = col_date(format = "%Y-%m-%d"),SCE = col_double(), SNO = col_double(), SVH = col_double(),
                                                                                H = col_double(), D = col_double(),
                                                                                Fprum = col_double(), Fmax = col_double(), 
                                                                                T = col_double(), SRA1H = col_double(), SSV1H = col_double(), 
                                                                                T05 = col_double(), TPM = col_double(), 
                                                                                date = col_character()), 
                                  trim_ws = TRUE))
dates <- gsub(pattern = " UTC", replacement = "", x = dta_LUCB$date)
#dta_LUCB$date <- NULL
dta_LUCB$date <- as.POSIXct(x = dates, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

times <- dta_LUCB$time
#dta_LUCB$time <- NULL
dta_LUCB$time <- as.POSIXct(x = times, format = "%Y-%m-%d %H:%M")

dta_LUCB$DATE2 <- strtrim(x = as.character(dta_LUCB$date), width = 10)# d?lka 10 znak? zbaven? se minut a sekund
dta_LUCB$DATE2 <- as.POSIXct(paste(dta_LUCB$date), format = "%Y-%m-%d")
#data_hourly_2004_2020 <- as.POSIXct(paste(data_hourly_2004_2020$date), format = "%d.%m.%Y %H:%M")

Aval$DATE2 <- strtrim(x = Aval$date, width = 10)# d?lka 10 znak? zbaven? se minut a sekund
Aval$DATE3 <- as.POSIXct(paste0(Aval$DATE2, " 07:00"), format = "%d.%m.%Y %H:%M")
Aval$DATE_OFF <- as.POSIXct(Aval$DATE3 + (17*60*60))# srovn?n? na 24 h od 7:00

Aval_short <- Aval[DATE3 %between% c(dta_LUCB[1,DATE2] + 5*24*60*60, dta_LUCB[nrow(dta_LUCB),DATE2])]#dtum prvn? laviny a meteo 5 dn? p?ed
boxplot (Aval_short$N)
summary (Aval_short$N)

Aval_short$ID <- paste0("Aval ", 1:nrow(Aval_short))#vlo?en? sloupce v?dy? AVal_ a s ka?d?m ??dkem se bude p?ipisovat ??slo
#

aval_list <- list()
for (i in 1:nrow(Aval_short)){
  id <- Aval_short[i,30]#30 sloupec Aval 1,2,3...
  start = Aval_short[i,29]#29 sloupec pro ka?dou lavinu, datum
  stop =  Aval_short[i,29] - 5*24*60*60 #okno -5, form?t Posix pracuje se sekundami
  dta_LUCB_aval <- dta_LUCB[DATE2 %between% c(stop[1], start[1]) , ]#vezmi ??dky start, stop a pro ty mi vykresli meteo prom?nn?
  dta_LUCB_aval$PLOT <- 1:nrow(dta_LUCB_aval)# pro ka?dou ud?lost graf? kv?li ?emu? pro lep?? vykreslen? dat # K ?EMU PLOT?
  dta_LUCB_aval$ID <- id # sloupec id asi definov?n? parametru?
  aval_list[[i]] <- dta_LUCB_aval
  print(i)
}

head (aval_melt_total)
#4159 lavin 2009-2020
aval_dta_LUCBfr <- rbindlist(aval_list)

aval_dta_LUCBfr$ranking <- aval_dta_LUCBfr$date <- aval_dta_LUCBfr$day <- aval_dta_LUCBfr$month <- aval_dta_LUCBfr$year <- aval_dta_LUCBfr$datum <-aval_dta_LUCBfr$time <- NULL #vymaz?n? nepot?ebn?ch sloupc?

aval_melt_LUCB <- melt(data = aval_dta_LUCBfr, id.vars = c("ID", "DATE2", "PLOT"))#
aval_melt_LUCB[ , station:="LUCB"]
saveRDS(object = aval_melt_LUCB, file = "C:/Users/marketa.souckova/Documents/laviny/aval_melt_LUCB.rds")
aval_melt_LUCB <- readRDS(file = "laviny/aval_melt_LUCB.rds")

LUCB_LBOU <-rbind(aval_melt,aval_melt_LUCB)
# definition of TARGET Z/V missing
sub_dta_LUCB_LBOU <- LUCB_LBOU[ID %in% target]
sub_dta2_LUCB <- sub_dta_LBOU[,mean(value, na.rm = T), by = .(PLOT, variable)]#pr?m?r v?ech hodnot mokr?ch lavin, jak stejn? vytvo??m pro nelaviny, event = 0? 

sub_dta2_LUCB_LBOU <- sub_dta_LUCB_LBOU[,mean(value, na.rm = T), by = .(PLOT, variable, station)]

LBOU_LUCB ggplot(sub_dta2_LUCB_LBOU)+
  geom_line(aes(x = PLOT, y = V1, color = station, col= "red, blue"))+
  #geom_smooth(aes(x = PLOT, y = V1), method = "lm")+
  #scale_color_manual(values = c("red", "blue"))+
  facet_wrap(~ variable, scales = "free_y")+
  scale_x_reverse()
saveRDS(object = sub_dta2_LUCB_LBOU, file = "C:/Users/marketa.souckova/Documents/laviny/6dayprior_AD_LBOU_LUCB.rds")
# TPM delete I don't have it in both datasets

LBOU_LUCB <- ggplot(aval_melt_total)+
  geom_line(aes(x = PLOT, y = value, color = stat))+
  facet_wrap(~ variable, scales = "free_y")+
  scale_x_reverse()
saveRDS(object = aval_melt_total, file = "C:/Users/marketa.souckova/Documents/laviny/6dayprior_AD_LBOU_LUCB.rds")

head (aval_melt_total)

#rename
names(sub_dta2_LUCB_LBOU)[names(sub_dta2_LUCB_LBOU) == "SCE"] <- " [snow depth cm]"
names(sub_dta2_LUCB_LBOU)[names(sub_dta2_LUCB_LBOU) == "SVH"] <- "snow water equivalent [mm]"
names(sub_dta2_LUCB_LBOU)[names(sub_dta2_LUCB_LBOU) == "SNO"] <- "new snow sum [cm]"
names(sub_dta2_LUCB_LBOU)[names(sub_dta2_LUCB_LBOU) == "D"] <- "wind direction"
names(sub_dta2_LUCB_LBOU)[names(sub_dta2_LUCB_LBOU) == "Fprum"] <- "averadge wind speed"
names(sub_dta2_LUCB_LBOU)[names(sub_dta2_LUCB_LBOU) == "Fmax"] <- "maximum wind speed"
names(sub_dta2_LUCB_LBOU)[names(sub_dta2_LUCB_LBOU) == "T05"] <- "soil temperature"
names(sub_dta2_LUCB_LBOU)[names(sub_dta2_LUCB_LBOU) == "SRA1H"] <- "precipitation"
names(sub_dta2_LUCB_LBOU)[names(sub_dta2_LUCB_LBOU) == "SSV1H"] <- "sun light"
names(sub_dta2_LUCB_LBOU)[names(sub_dta2_LUCB_LBOU) == "RGLB1H"] <- "longwave radiation"
