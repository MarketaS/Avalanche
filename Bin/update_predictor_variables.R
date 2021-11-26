library(data.table)

dta_dir <- 'data12112021/'
dta_dir <- ("C:/Users/souckovamarketa/OneDrive - CZU v Praze/R/Avalanche/Bin/daily/")
adcast_A_daily <- readRDS(paste0(dta_dir, "adcast_A_daily.rds"))
adcast_C_daily <- readRDS(paste0(dta_dir, "adcast_C_daily.rds"))

## adcast_A_daily
adcast_A_daily_sub <- subset(adcast_A_daily, select = -c(ID, PLOT, stat))
adcast_A_daily_unique <- unique(adcast_A_daily_sub) 
adcast_A_daily_aval_days <- adcast_A_daily[event == 1, max(as.Date(Date)), .(ID)]
names(adcast_A_daily_aval_days)[2] <- "Date"

# New predictors
adcast_A_daily_unique[,NSSsum3:= frollsum(NSS_value,3, align = "right", na.rm = T)]
adcast_A_daily_unique[,NSSsum6:= frollsum(NSS_value,6, align = "right", na.rm = T)]
adcast_A_daily_unique[,Rain_Ta_sum3:= frollsum(Rain_Ta_value,3, align = "right", na.rm = T)]
adcast_A_daily_unique[,Rain_Ta_sum6:= frollsum(Rain_Ta_value,6, align = "right", na.rm = T)]
adcast_A_daily_unique[,Tmax3:= frollapply(Tair_value,3, max,  align = "right", na.rm = T, fill = NA)]
adcast_A_daily_unique[,Tmax6:= frollapply(Tair_value,6, max,  align = "right", na.rm = T)]
adcast_A_daily_unique[,Tmin3:= frollapply(Tair_value,3, min,  align = "right", na.rm = T)]
adcast_A_daily_unique[,Tmin6:= frollapply(Tair_value,6, min,  align = "right", na.rm = T)]
adcast_A_daily_unique[Tmax3 == Inf | Tmax3 == -Inf,Tmax3:= NA]
adcast_A_daily_unique[Tmax6 == Inf | Tmax6 == -Inf,Tmax6:= NA]
adcast_A_daily_unique[Tmin3 == Inf | Tmin3 == -Inf,Tmin3:= NA]
adcast_A_daily_unique[Tmin6 == Inf | Tmin6 == -Inf,Tmin6:= NA]

adcast_A_daily_unique[,Tamp3:= Tmax3-Tmin3]
adcast_A_daily_unique[,Tamp6:= Tmax6-Tmin6]
adcast_A_daily_unique[,SD2:= shift(SD_value, 2,  type = 'lag')]
adcast_A_daily_unique[,SD3:= shift(SD_value, 3,  type = 'lag')]
adcast_A_daily_unique[,SD4:= shift(SD_value, 4,  type = 'lag')]
adcast_A_daily_unique[,SD6:= shift(SD_value, 6,  type = 'lag')]
adcast_A_daily_unique[,SDdif2:= SD_value - SD2]
adcast_A_daily_unique[,SDdif3:= SD_value - SD3]
adcast_A_daily_unique[,SDdif4:= SD_value - SD4]
adcast_A_daily_unique[,SDdif6:= SD_value - SD6]

adcast_A_daily_unique[,keep:= TRUE]
adcast_A_daily_unique[,Date_check := shift(Date, 6,  type = 'lag')]
adcast_A_daily_unique[difftime(Date, Date_check, units = 'days') > 6, keep := FALSE]
adcast_A_daily_unique[event == 1 & !(Date %in% adcast_A_daily_aval_days$Date), keep := FALSE]
adcast_A_daily_unique[event == 1 & Date %in% adcast_A_daily_aval_days$Date, keep := TRUE]
adcast_A_daily_unique[, aval_before := which.min(difftime( adcast_A_daily_aval_days$Date,Date, units = 'days') < 6)-1,.(Date)]
adcast_A_daily_unique[aval_before > 0, aval_date_before := adcast_A_daily_aval_days$Date[aval_before]]
adcast_A_daily_unique[, norm_date := Date]
adcast_A_daily_unique[event == 0 & difftime( Date, aval_date_before,units = "days") < 6, keep:= FALSE]


adcast_A_daily_unique <- subset(adcast_A_daily_unique, select = -c(SD2,SD3,SD4,SD6,Date_check, aval_before, aval_date_before, norm_date))

adcast_A_daily_updated <- adcast_A_daily_unique[keep == TRUE]
adcast_A_daily_updated <- subset(adcast_A_daily_updated, select = -c(keep))

saveRDS(adcast_A_daily_updated, file = 'adcast_A_daily_upd.rds')

## adcast_C_daily
adcast_C_daily_sub <- subset(adcast_C_daily, select = -c(ID, PLOT))
adcast_C_daily_unique <- unique(adcast_C_daily_sub) 
adcast_C_daily_aval_days <- adcast_C_daily[event == 1, max(as.Date(Date)), .(ID)]
names(adcast_C_daily_aval_days)[2] <- "Date"

# New predictors
adcast_C_daily_unique[,NSSsum3:= frollsum(NSS_value,3, align = "right", na.rm = T)]
adcast_C_daily_unique[,NSSsum6:= frollsum(NSS_value,6, align = "right", na.rm = T)]
adcast_C_daily_unique[,Rain_Ta_sum3:= frollsum(Rain_Ta_value,3, align = "right", na.rm = T)]
adcast_C_daily_unique[,Rain_Ta_sum6:= frollsum(Rain_Ta_value,6, align = "right", na.rm = T)]
adcast_C_daily_unique[,Tmax3:= frollapply(Tair_value,3, max,  align = "right", na.rm = T)]
adcast_C_daily_unique[,Tmax6:= frollapply(Tair_value,6, max,  align = "right", na.rm = T)]
adcast_C_daily_unique[,Tmin3:= frollapply(Tair_value,3, min,  align = "right", na.rm = T)]
adcast_C_daily_unique[,Tmin6:= frollapply(Tair_value,6, min,  align = "right", na.rm = T)]
adcast_C_daily_unique[Tmax3 == Inf | Tmax3 == -Inf,Tmax3:= NA]
adcast_C_daily_unique[Tmax6 == Inf | Tmax6 == -Inf,Tmax6:= NA]
adcast_C_daily_unique[Tmin3 == Inf | Tmin3 == -Inf,Tmin3:= NA]
adcast_C_daily_unique[Tmin6 == Inf | Tmin6 == -Inf,Tmin6:= NA]
adcast_C_daily_unique[,Tamp3:= Tmax3-Tmin3]
adcast_C_daily_unique[,Tamp6:= Tmax6-Tmin6]
adcast_C_daily_unique[,SD2:= shift(SD_value, 2,  type = 'lag')]
adcast_C_daily_unique[,SD3:= shift(SD_value, 3,  type = 'lag')]
adcast_C_daily_unique[,SD4:= shift(SD_value, 4,  type = 'lag')]
adcast_C_daily_unique[,SD6:= shift(SD_value, 6,  type = 'lag')]
adcast_C_daily_unique[,SDdif2:= SD_value - SD2]
adcast_C_daily_unique[,SDdif3:= SD_value - SD3]
adcast_C_daily_unique[,SDdif4:= SD_value - SD4]
adcast_C_daily_unique[,SDdif6:= SD_value - SD6]

adcast_C_daily_unique[,keep:= TRUE]
adcast_C_daily_unique[,Date_check := shift(Date, 6,  type = 'lag')]
adcast_C_daily_unique[difftime(Date, Date_check, units = 'days') > 6, keep := FALSE]
adcast_C_daily_unique[event == 1 & !(Date %in% adcast_C_daily_aval_days$Date), keep := FALSE]
adcast_C_daily_unique[event == 1 & Date %in% adcast_C_daily_aval_days$Date, keep := TRUE]
adcast_C_daily_unique[, aval_before := which.min(difftime( adcast_C_daily_aval_days$Date,Date, units = 'days') < 6)-1,.(Date)]
adcast_C_daily_unique[aval_before > 0, aval_date_before := adcast_C_daily_aval_days$Date[aval_before]]
adcast_C_daily_unique[, norm_date := Date]
adcast_C_daily_unique[event == 0 & difftime( Date, aval_date_before,units = "days") < 6, keep:= FALSE]
adcast_C_daily_unique <- subset(adcast_C_daily_unique, select = -c(SD2,SD3,SD4,SD6,Date_check, aval_before, aval_date_before, norm_date))
adcast_C_daily_updated <- adcast_C_daily_unique[keep == TRUE]
adcast_C_daily_updated <- subset(adcast_C_daily_updated, select = -c(keep))

saveRDS(adcast_C_daily_updated, file = 'adcast_C_daily_upd.rds')


adcast_C_daily_updated2004 <- adcast_C_daily_updated[Date > as.Date('01.01.2004', format = '%d.%m.%Y')]
adcast_A_daily_updated2004 <- adcast_A_daily_updated[Date > as.Date('01.01.2004', format = '%d.%m.%Y')]
