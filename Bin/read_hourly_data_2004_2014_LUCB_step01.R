getwd()
dir <- "C:/Users/Marketa/Documents/CZU/R/Krkonose" # odkud se berou data
# set your data directory
datadir <- paste0(dir,'/data')#struktura zdrojov? data
CHMI_dir <- paste0(datadir,'/converted')# transformovaná data do xlsx
###### 2. load all libraries you need to execute the script
library(data.table)
library(readxl)
# 3. find files in directory
all_files <- list.files(CHMI_dir)
station <- 'H1LUCB'

station_files <- list.files(CHMI_dir, pattern = paste0("*","_",station,"*"))
#test_station_files <- list.files(CHMI_dir, pattern = paste0("*","2010-2014","*"))
nskip <- 5#WHAT IS THIS FOR?
# read data
D_T05_a <- read_excel(paste0(CHMI_dir, '/',station_files[1]), skip = nskip-3, sheet = 1, na = "")  #skip prvn???? 3 řadky - hlavička excelu, sheet 1, na - jsou označeny jako prázdné buňky
D_T05_b <- read_excel(paste0(CHMI_dir, '/',station_files[2]), skip = nskip-3, sheet = 1, na = "")

# merge data based on rows
D_T05 <- rbind(D_T05_a,D_T05_b)
# rename colums
names(D_T05)[1:4] <- c('year', 'month', 'day', 'hour')
# remove unneccesarry data
rm(D_T05_a,D_T05_b)
# manipulate data to desired format
# 1. change data to data.table
D_T05 <- as.data.table(D_T05)
D_T05[, day := substr(day, 4, 99)]# delete val
D_T05[, date := strptime(paste0( year, month , day, ' ', hour), format = '%Y%m%d %H:%M')]
D_T05[, year := NULL]
D_T05[, month := NULL]
D_T05[, day := NULL]
D_T05[, hour := NULL]#odmazani nepotrebnych sloupcu
D_T05 <- D_T05[,.(date, D, T05)]

D_T05_1H<-D_T05[!is.na(date)]

Dmax_1H_a <- read_excel(paste0(CHMI_dir, '/',station_files[3]), skip = nskip-2, sheet = 1, na = "")
Dmax_1H_b <- read_excel(paste0(CHMI_dir, '/',station_files[4]), skip = nskip-2, sheet = 1, na = "")
Dmax_1H <- rbind(Dmax_1H_a,Dmax_1H_b)
rm(Dmax_1H_a,Dmax_1H_b)
Dmax_1H <- as.data.table(Dmax_1H)
Dmax_1H[, date := strptime(paste0( day, ' ', time), format = '%d.%m.%Y %H:%M')]
# rename the data column
names(Dmax_1H)[4] <- 'Dmax'
Dmax_1H <- Dmax_1H[,.(date, Dmax)]
Dmax_1H[!is.na(date)]

Fmax_1H_a <- read_excel(paste0(CHMI_dir, '/',station_files[5]), skip = nskip-2, sheet = 1, na = "")
Fmax_1H_b <- read_excel(paste0(CHMI_dir, '/',station_files[6]), skip = nskip-2, sheet = 1, na = "")
Fmax_1H <- rbind(Fmax_1H_a,Fmax_1H_b)
rm(Fmax_1H_a,Fmax_1H_b)
Fmax_1H <- as.data.table(Fmax_1H)
Fmax_1H[, date := strptime(paste0( day, ' ', time), format = '%d.%m.%Y %H:%M')]
# rename the data column
names(Fmax_1H)[4] <- 'Fmax'
Fmax_1H <- Fmax_1H[,.(date, Fmax)]
Fmax_1H[!is.na(date)]

Fprum_1H_a <- read_excel(paste0(CHMI_dir, '/',station_files[7]), skip = nskip-2, sheet = 1, na = "")
Fprum_1H_b <- read_excel(paste0(CHMI_dir, '/',station_files[8]), skip = nskip-2, sheet = 1, na = "")
Fprum_1H <- rbind(Fprum_1H_a,Fprum_1H_b)
rm(Fprum_1H_a,Fprum_1H_b)
Fprum_1H <- as.data.table(Fprum_1H)
Fprum_1H[, date := strptime(paste0( day, ' ', time), format = '%d.%m.%Y %H:%M')]
# rename the data column
names(Fprum_1H)[4] <- 'Fprum'
Fprum_1H <- Fprum_1H[,.(date, Fprum)]
Fprum_1H[!is.na(date)]

H_1H_a <- read_excel(paste0(CHMI_dir, '/',station_files[9]), skip = nskip-2, sheet = 1, na = "")
H_1H_b <- read_excel(paste0(CHMI_dir, '/',station_files[10]), skip = nskip-2, sheet = 1, na = "")
H_1H <- rbind(H_1H_a,H_1H_b)
rm(H_1H_a,H_1H_b)
H_1H <- as.data.table(H_1H)
H_1H[, date := strptime(paste0( day, ' ', time), format = '%d.%m.%Y %H:%M')]
# rename the data column
names(H_1H)[4] <- 'H'
H_1H <- H_1H[,.(date, H)]
H_1H[!is.na(date)]

O_a <- read_excel(paste0(CHMI_dir, '/',station_files[12]), skip = nskip-5, sheet = 1, na = "")
O_b <- read_excel(paste0(CHMI_dir, '/',station_files[13]), skip = nskip-5, sheet = 1, na = "")
O <- rbind(O_a,O_b)
# rename colums
names(O)[1:4] <- c('year', 'month', 'day', 'hour')
rm(O_a,O_b)
names(O)[5] <- 'O'
O <- as.data.table (O)
O[, day := substr(day, 4, 99)]# delete val
O[, date := strptime(paste0( year, month , day, ' ', hour), format = '%Y%m%d %H:%M')]
O[, year := NULL]
O[, month := NULL]
O[, day := NULL]
O[, hour := NULL]#odmazani nepotrebnych sloupcu
O <- O[,.(date, O)]
O[!is.na(date)]

SCE_SNO_SVH <- read_excel(paste0(CHMI_dir, '/',station_files[14]), skip = nskip-3, sheet = 1, na = "")
names(SCE_SNO_SVH)[1:3] <- c('year', 'month', 'day')
SCE_SNO_SVH
snow <- as.data.table(SCE_SNO_SVH)
snow[, date := strptime(paste0( year, month , day), format = '%Y%m%d')]
snow[, year := NULL]
snow[, month := NULL]
snow[, day := NULL]
snow[, month := NULL]
snow <- snow[,.(date, SCE, SNO, SVH)]
snow_prep <-snow[,.(date, SCE, SNO, SVH)]
snow_hour <- snow_prep[sort(rep(1:nrow(snow_prep),24))]
# to add hours you can do it the following
hours_in_day <- paste0(0:23, ':00')
# add hours to your data by repeating your hour vector for the number of days you had in the snow data.  snow_hour[,hours:= rep(hours_in_day, nrow(snow_prep))]
snow_hour[,hours:= rep(hours_in_day, nrow(snow_prep))]
# merge the new date as you did before
snow_hour[, date:=  strptime(paste0(date, ' ', hours), format = '%Y-%m-%d %H:%M')]
snow_1H<-snow_hour
snow_1H[,hours := NULL]
snow_1H<-snow_1H[,.(date, SCE, SNO, SVH)]
snow_1H[!is.na(date)]

T_1H_a <- read_excel(paste0(CHMI_dir, '/',station_files[19]), skip = nskip-2, sheet = 1, na = "")
T_1H_b <- read_excel(paste0(CHMI_dir, '/',station_files[20]), skip = nskip-2, sheet = 1, na = "")
T_1H <- rbind(T_1H_a,T_1H_b)
rm(T_1H_a,T_1H_b)
T_1H <- as.data.table(T_1H)
T_1H[, date := strptime(paste0( day, ' ', time), format = '%d.%m.%Y %H:%M')]
# rename the data column
names(T_1H)[4] <- 'T'
T_1H <- T_1H[,.(date, T)]
T_1H[!is.na(date)]

TPM_1H_a <- read_excel(paste0(CHMI_dir, '/',station_files[21]), skip = nskip-2, sheet = 1, na = "")
TPM_1H_b <- read_excel(paste0(CHMI_dir, '/',station_files[22]), skip = nskip-2, sheet = 1, na = "")
TPM_1H <- rbind(TPM_1H_a,TPM_1H_b)
rm(TPM_1H_a,TPM_1H_b)
TPM_1H <- as.data.table(TPM_1H)
TPM_1H[, date := strptime(paste0( day, ' ', time), format = '%d.%m.%Y %H:%M')]
# rename the data column
names(TPM_1H)[4] <- 'TPM'
TPM_1H <- TPM_1H[,.(date, TPM)]
TPM_1H[!is.na(date)]

install.packages('writexl')
library(writexl)

#' @param adresar Is a working folder containing data - csv files only 
#' @param stanice Is a single value or vector containg names of the stations. It means file name without a suffix.  
#' @param parametr Is a single value or vector with meteorological variable which should be loaded from the file. i.e. c('SNO','SCE','SVH')
#' @param value Is by default set to 'Value'. This only plays a role, when columns containing the keen values are not originaly named by the parametr (see above), but differently.
#' @param par.order Is order of the parametr vector. The column will be named by this parametr instead of original name (i.e. 'Value') 
#' @param nazev.STA is vector of station codes i.e H1LBOU01, it is better to be created in advance, because files have names of different lenght  

dir <- "C:/Users/Marketa/Documents/CZU/R/Krkonose/data/converted/"
stanice2 = 'SRA1H_SSV_1H_H1LUCB01_2004-2009'
stanice = 'SRA1H_SSV_1H_H1LUCB01_2010-2014'

parametr2 = 'SRA1H'
parametr3 = 'SSV1H'

nazev.STA = 'H1LUCB01'

nacti_hod_Data_SRA <- function(adresar,stanice,parametr, value = 'Value', par.order = 1, nazev.STA = nazev.STA, sep = ';'){
  
  library("data.table")
  
  k = length(parametr)  
  
  if (length(stanice) == 1){
    
    
    dta_SRA <- fread(paste0(adresar, stanice, '.csv'), sep = sep, header = TRUE, skip = 2, na.strings = '', stringsAsFactors = F)
    names(dta_SRA)[1:4] <- c('Y','M','D', 'H')
    
    dta_SRA <- dta_SRA[!is.na(H),] # skip all NA values in hour column
    dta_SRA[, D := gsub('Val', '', D)] # substitute all "Val" string - get rid of it
    
    dta_SRA[,datum := as.Date(do.call(paste, c(.SD, sep = '-')), format = '%Y-%m-%d'), .SDcols = c('Y','M','D')]
    
    dta_SRA[,time := paste(as.character(datum), as.character(H))]
    dta_SRA$realTime = as.POSIXct(dta_SRA$time, format = '%Y-%m-%d %H:%M')
    
    # delete columns 'datum' and 'time'
    dta_SRA[, c('datum','time') := NULL]
    
    # add column with station name
    dta_SRA[, nazev := nazev.STA]
    
    # moje
    
    # if column is called Value, then rename it to parametr (or one of the parameter item)
    names(dta_SRA)[names(dta_SRA) == value] = parametr[par.order]
    
    hodnoty_hod=dta_SRA[,parametr,with=FALSE]
    Time = dta_SRA[,realTime]
    nazev = dta_SRA[,nazev]
    
    dta_SRA = dta_SRA[,velicina := names(hodnoty_hod)]
    velicina = dta_SRA[, velicina]
    
    dta_SRA <- cbind(Time, hodnoty_hod, nazev, velicina)
    
    # delete NA values in Time column
    dta_SRA <- dta_SRA[!is.na(Time),]
    
  } else {
    
    DTA <- list()
    
    for (i in seq_along(stanice)){
      
      print (i)
      print(stanice[i])
      
      dta_SRA <- fread(paste0(adresar, stanice[i], '.csv'), sep = sep, header = TRUE, skip = 2, na.strings = '', stringsAsFactors = F)
      names(dta_SRA)[1:4] <- c('Y','M','D', 'H')
      
      dta_SRA <- dta_SRA[!is.na(H),] # skip all NA values in hour column
      dta_SRA[, D := gsub('Val', '', D)] # substitute all "Val" string - get rid of it
      
      dta_SRA[,datum := as.Date(do.call(paste, c(.SD, sep = '-')), format = '%Y-%m-%d'), .SDcols = c('Y','M','D')]
      
      dta_SRA[,time := paste(as.character(datum), as.character(H))]
      dta_SRA$realTime = as.POSIXct(dta_SRA$time, format = '%Y-%m-%d %H:%M')
      
      # delete columns 'datum' and 'time'
      dta_SRA[, c('datum','time') := NULL]
      
      
      # add column with station name
      dta_SRA[, nazev := nazev.STA[i]]
      
      # moje
      
      # if column is called Value, then rename it to parametr (or one of the parameter item)
      names(dta_SRA)[names(dta_SRA) == value] = parametr[par.order]
      
      hodnoty_hod=dta_SRA[,parametr,with=FALSE]
      Time = dta_SRA[,realTime]
      nazev = dta_SRA[,nazev]
      
      dta_SRA = dta_SRA[,velicina := names(hodnoty_hod)]
      velicina = dta_SRA[, velicina]
      
      dta_SRA <- cbind(Time, hodnoty_hod, nazev, velicina)
      
      # delete NA values in Time column
      dta_SRA <- dta_SRA[!is.na(Time),]
      dta_SRA[, velicina]
      
      print ("done")
      
      DTA[[i]] <- dta_SRA
      
    }
    
    dta_SRA <- do.call(rbind, DTA)
    
  }
  
  dta_SRA
  
}

# RGLB_LBOU1 <- nacti_hod_Data_SRA(adresar = dir, stanice = stanice, parametr = parametr1, nazev.STA = nazev.STA)
SRA_LUCB1 <- nacti_hod_Data_SRA(adresar = dir, stanice = stanice, parametr = parametr2, nazev.STA = nazev.STA)
SSV_LUCB1 <- nacti_hod_Data_SRA(adresar = dir, stanice = stanice, parametr = parametr3, nazev.STA = nazev.STA)

# RGLB_LBOU2 <- nacti_hod_Data_SRA(adresar = dir, stanice = stanice2, parametr = parametr1, nazev.STA = nazev.STA)
SRA_LUCB2 <- nacti_hod_Data_SRA(adresar = dir, stanice = stanice2, parametr = parametr2, nazev.STA = nazev.STA)
SSV_LUCB2 <- nacti_hod_Data_SRA(adresar = dir, stanice = stanice2, parametr = parametr3, nazev.STA = nazev.STA)

names(SRA_LUCB1) = c('date', 'SRA1H', 'nazev', 'velicina')
names(SSV_LUCB1) = c('date', 'SSV1H', 'nazev', 'velicina')

#names(SRA_LBOU1) = c('date', 'SRA1H', 'nazev', 'velicina')
#names(SSV_LBOU1) = c('date', 'SSV1H', 'nazev', 'velicina')
#names(RGLB_LBOU1) = c('date', 'RGLB1H', 'nazev', 'velicina')

names(SRA_LUCB2) = c('date', 'SRA1H', 'nazev', 'velicina')
names(SSV_LUCB2) = c('date', 'SSV1H', 'nazev', 'velicina')

#names(SRA_LBOU2) = c('date', 'SRA1H', 'nazev', 'velicina')
#names(SSV_LBOU2) = c('date', 'SSV1H', 'nazev', 'velicina')
#names(RGLB_LBOU2) = c('date', 'RGLB1H', 'nazev', 'velicina')

SRA_LUCB <- rbind(SRA_LUCB2, SRA_LUCB1)
SSV_LUCB <- rbind(SSV_LUCB2, SSV_LUCB1)

#RGLB_LBOU <- rbind(RGLB_LBOU2, RGLB_LBOU1)
#SRA_LBOU <- rbind(SRA_LBOU2, SRA_LBOU1)
#SSV_LBOU <- rbind(SSV_LBOU2, SSV_LBOU1)

#SSV_LBOU[, SSV1H := as.numeric(SSV1H)] 

#merge_all <- function(x,y){merge(x, y, all=T)}
#full_dt_2004_2014 <- Reduce(merge_all , x = list(H_1H, snow_1H, SRA_LBOU[, 1:2], SSV_LBOU[, 1:2], RGLB_LBOU[, 1:2], T_1H, Fprum_1H, Fmax_1H, D_T05_1H))
merge_all <- function(x,y){merge(x, y, all=T)}
full_dt_2004_2014 <- Reduce(merge_all , x = list(H_1H, snow_1H, SRA_LUCB[, 1:2], SSV_LUCB[, 1:2], T_1H, Fprum_1H, Fmax_1H, D_T05_1H, TPM_1H))
#full_dt_2004_2014 <- Reduce(merge_all , x = list(snow_1H, H_1H, D_T05_1H, Fprum_1H, Fmax_1H, T_1H, TPM_1H, , SRA_LBOU[, 1:2], SSV_LBOU[, 1:2], RGLB_LBOU[, 1:2]))
write.csv(full_dt_2004_2014, "C:/Users/Marketa/Documents/CZU/R/Krkonose/LUCB_data_hourly_2004_2014.csv")
# NELZE VYGENEROVAT S TPM_1H pro LBOU, LUCB lze
full_dt_2004_2014<- as.data.table(full_dt_2004_2014)
full_dt_2004_2014<- full_dt_2004_2014[!is.na(date),]


#proměnné denní
#Tmin <- data_1961_2003 [, Tmin := min(c(T7, T14, T21), na.rm = F), by = date]
#Tmax <- data_1961_2003 [, Tmax := max(c(T7, T14, T21), na.rm = F), by = date]
#proměnné hodinnové
#Thourly_min <- full_dt_2004_2014 [, Tmin := min(T), as.Date(date)]
#Thourly_max <- full_dt_2004_2014 [, Tmax := max(T), as.Date(date)]
#Tdifference_24h <- full_dt_2004_2014 [, Tdiff := max(T)-min(T), as.Date(date)]
#Hmix <- full_dt_2004_2014 [, Hmin := min(H), as.Date(date)]
#Hmax<- full_dt_2004_2014 [, Hmax := max(H), as.Date(date)]


#odstraním RGLB, páč to není v min datech: RGLB = sum(RGLB1H)
#data_daily_2004_2014 <- full_dt_2004_2014 [, .(Hprum = mean(H), SCE = mean(SCE), SNO = mean(SNO), SVH = mean(SVH), SRA= sum(SRA1H), SSV= sum(SSV1H),Tprum = mean(T), Dprum = mean(D), T05prum = mean(T05), Fprum = mean(Fprum), Fmax = mean(Fmax) ), as.Date(date)]
LUCB_data_daily_2004_2014 <- full_dt_2004_2014 [, .(Hprum = mean(H), SCE = mean(SCE), SNO = mean(SNO), SVH = mean(SVH), SRA= sum(SRA1H), SSV= sum(SSV1H),Tprum = mean(T), Dprum = mean(D), T05prum = mean(T05), Fprum = mean(Fprum), Fmax = mean(Fmax) ), as.Date(date)]

colnames(LUCB_data_daily_2004_2014)
names(LUCB_data_daily_2004_2014)[1] <- "date"
LUCB_data_daily_2004_2014<- LUCB_data_daily_2004_2014[!is.na(date),]
write_xlsx(LUCB_data_daily_2004_2014, "C:/Users/Marketa/Documents/CZU/R/Krkonose/LUCB_data_daily_2004_2014.xlsx")

#as.POSIXct(data_daily_2004_2014$as.Date, 
# data_daily_2004_2014 <- read_xlsx("data_daily_2004_2014.xlsx", sheet = 1, skip = 0, na ="", col_types = c(rep('guess',1), rep('numeric',11)))
str(LUCB_data_daily_2004_2014)
#data_daily_2004_2014<- as.data.table(data_daily_2004_2014)


saveRDS(LUCB_data_daily_2004_2014, file = "my_data.rds")
save.image(file = "LUCB01.RData")
load("LUCB01.RData")


#I dont need these variables right now
#stavpocasi <- read_excel(paste0(CHMI_dir, '/',station_files[17]), skip = nskip-3, sheet = 1, na = "")  
#jevy <- read_excel(paste0(CHMI_dir, '/',station_files[11]), skip = nskip-4, sheet = 1, na = "")  

#daily values from hourly
#fce as.Date(date)] only uses the date information and not the hourly information, so it will sum or take the mean for each day.
#Once you have the daily values of your newer data, you can join them using the function merge
#merge(data_daily1961_2003, data_daily20042014, all = T). Use all = T when you want to keep all data.


