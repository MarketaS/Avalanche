#úprava meteo dat 2015-2020 a spojení s hodinovými daty 2009-2014

dir <- "C:/Users/Marketa/Documents/CZU/R/Krkonose" # odkud se berou data
setwd <- "C:/Users/Marketa/Documents/CZU/R/Krkonose"

###### 2. load all libraries you need to execute the script
install.packages('writexl')
library(writexl)
library(readxl)
library(data.table)
library(readr)

LUCB_meteo_2004_2014<-read_xlsx("C:/Users/Marketa/Documents/CZU/R/Krkonose/LUCB_data_hourly_2004_2014.xlsx",col_types = c(rep('guess',1), rep('numeric',12), rep('guess',2)))
as.data.table(LUCB_meteo_2004_2014)
LUCB_meteo_2004_2014<-(data.table(LUCB_meteo_2004_2014))
class (LUCB_meteo_2004_2014)
str(LUCB_meteo_2004_2014)


#nazvy <- gsub(x = LUCB_meteo_2015_2020[1,], pattern = " ", replacement = "_")#nahrazeni mezery za _

LUCB_meteo_2015_2020<-read.csv("C:/Users/Marketa/Documents/CZU/R/Krkonose/LUCB_2015_2020.csv", sep = ";")

# get column names
colnames(LUCB_meteo_2015_2020)

# Rename column where names is "Sepal.Length"
#names(LUCB_meteo_2015_2020)[names(LUCB_meteo_2015_2020) == "rok"] <- "year"
#names(LUCB_meteo_2015_2020)[names(LUCB_meteo_2015_2020) == "mìsíc"] <- "month"
#names(LUCB_meteo_2015_2020)[names(LUCB_meteo_2015_2020) == "den"] <- "day"
#names(LUCB_meteo_2015_2020)[names(LUCB_meteo_2015_2020) == "hodina"] <- "hour"
#names(LUCB_meteo_2015_2020)[names(LUCB_meteo_2015_2020) == "relativní vlhkost vzduchu (%)"] <- "H"
#names(LUCB_meteo_2015_2020)[names(LUCB_meteo_2015_2020) == "smìr vìtru (°)"] <- "D"
#names(LUCB_meteo_2015_2020)[names(LUCB_meteo_2015_2020) == "rychlost vìtru (m/s)"] <- "Fprum"
#names(LUCB_meteo_2015_2020)[names(LUCB_meteo_2015_2020) == "maximální rychlost vìtru (m/s)"] <- "Fmax"
#names(LUCB_meteo_2015_2020)[names(LUCB_meteo_2015_2020) == "teplota vzduchu (°C)"] <- "T"
#names(LUCB_meteo_2015_2020)[names(LUCB_meteo_2015_2020) == "srážky (mm/1hod)"] <- "SRA1H"
#names(LUCB_meteo_2015_2020)[names(LUCB_meteo_2015_2020) == "sluneèní svit (0,1hod/1hod)"] <- "SSV1H"

LUCB_meteo_2015_2020<-data.table(LUCB_meteo_2015_2020)
class (LUCB_meteo_2015_2020)
str (LUCB_meteo_2015_2020)
LUCB_meteo_2015_2020[, day := substr(day, 4, 99)]# delete val
LUCB_meteo_2015_2020[,datum := as.Date(do.call(paste, c(.SD, sep = '-')), format = '%Y-%m-%d'), .SDcols = c('year','month','day')]
LUCB_meteo_2015_2020[,time := paste(as.character(datum), as.character(hour))]

LUCB_meteo_2015_2020$realTime = as.POSIXct(LUCB_meteo_2015_2020$time, format = '%Y-%m-%d %H:%M')
names(LUCB_meteo_2015_2020)[names(LUCB_meteo_2015_2020) == "realTime"] <- "date"
# manipulate data to desired format
LUCB_meteo_2015_2020$H <-as.numeric(LUCB_meteo_2015_2020$H)
LUCB_meteo_2015_2020$D <-as.numeric(LUCB_meteo_2015_2020$D)
LUCB_meteo_2015_2020$SSV1H <-as.numeric(LUCB_meteo_2015_2020$SSV1H)

LUCB_meteo_2015_2020[, year := NULL]
LUCB_meteo_2015_2020[, month := NULL]
LUCB_meteo_2015_2020[, day := NULL]
LUCB_meteo_2015_2020[, hour := NULL]#odmazani nepotrebnych sloupcu
LUCB_meteo_2015_2020[,.(date, H, D, Fprum, Fmax, T, SRA1H, SSV1H, datum)]

LUCB_meteo_2015_2020<-LUCB_meteo_2015_2020[!is.na(date)]

SNO_SCE_SVH_2015_2020<-read.csv("C:/Users/Marketa/Documents/CZU/R/Krkonose/SCE_SNO_SVH_LUCB_2015_2020.csv",sep = ";")
SNO_SCE_SVH_2015_2020 <- as.data.table (SNO_SCE_SVH_2015_2020)
SNO_SCE_SVH_2015_2020[, day := substr(day, 4, 99)]# delete val
str(SNO_SCE_SVH_2015_2020)
SNO_SCE_SVH_2015_2020$SCE <-as.numeric (SNO_SCE_SVH_2015_2020$SCE)
SNO_SCE_SVH_2015_2020$SNO <-as.numeric (SNO_SCE_SVH_2015_2020$SNO)
SNO_SCE_SVH_2015_2020$SVH <-as.numeric (SNO_SCE_SVH_2015_2020$SVH)

SNO_SCE_SVH_2015_2020[,datum := as.Date(do.call(paste, c(.SD, sep = '-')), format = '%Y-%m-%d'), .SDcols = c('year','month','day')]
SNO_SCE_SVH_2015_2020[,time := paste(as.character(datum), as.character(hour))]
names(SNO_SCE_SVH_2015_2020)[names(SNO_SCE_SVH_2015_2020) == "date"] <- "time"
SNO_SCE_SVH_2015_2020<-SNO_SCE_SVH_2015_2020[!is.na(date)]
dta<-merge(SNO_SCE_SVH_2015_2020[,.(datum, SCE, SNO, SVH)], LUCB_meteo_2015_2020, by ="datum")
str(dta)
LUCB_meteo_2004_2014$datum = as.Date(LUCB_meteo_2004_2014$datum, format = '%Y-%m-%d')
dta[, time := NULL]
LUCB_meteo_2004_2014[, time := NULL]

#tady nekde chyba posunuto o 1 h v case - cisla 0:00 odpovídají 0:01
LUCB_dta_hourly_2004_2020<- rbind(LUCB_meteo_2004_2014, dta, fill = TRUE)

class(dta$datum)
class(LUCB_meteo_2004_2014$datum)

write_xlsx(LUCB_dta_hourly_2004_2020, "C:/Users/Marketa/Documents/CZU/R/Krkonose/LUCB_dta_hourly_2004_2020.xlsx")
save.image(file = "LUCB_2004_2020.RData")
load("LUCB_2004_2020.RData.RData")