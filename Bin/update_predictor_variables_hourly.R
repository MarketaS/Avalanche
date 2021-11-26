library(data.table)

dta_dir <- 'data12112021/'

adcast_A <- readRDS(paste0(dta_dir, "adcast_A.rds"))

adcast_C <- readRDS(paste0(dta_dir, "adcast_C.rds"))

## adcast_A
adcast_A_sub <- subset(adcast_A, select = -c(ID, PLOT))
adcast_A_unique <- unique(adcast_A_sub) 
adcast_A_aval_days <- adcast_A[event == 1, max(UTC), .(ID)]
names(adcast_A_aval_days)[2] <- "Date"
names(adcast_A_unique)[1] <- "Date"

# New predictors
adcast_A_unique[,NSSsum72:= frollsum(NSS_value,72, align = "right", na.rm = T)]
adcast_A_unique[,NSSsum144:= frollsum(NSS_value,144, align = "right", na.rm = T)]
adcast_A_unique[,Rain_Ta_sum72:= frollsum(Rain_Ta_value,72, align = "right", na.rm = T)]
adcast_A_unique[,Rain_Ta_sum144:= frollsum(Rain_Ta_value,144, align = "right", na.rm = T)]
adcast_A_unique[,Tmax72:= frollapply(Tair_value,72, max,  align = "right", na.rm = T)]
adcast_A_unique[,Tmax144:= frollapply(Tair_value,144, max,  align = "right", na.rm = T)]
adcast_A_unique[,Tmin72:= frollapply(Tair_value,72, min,  align = "right", na.rm = T)]
adcast_A_unique[,Tmin144:= frollapply(Tair_value,144, min,  align = "right", na.rm = T)]
adcast_A_unique[Tmax72 == Inf | Tmax72 == -Inf,Tmax72:= NA]
adcast_A_unique[Tmax144 == Inf | Tmax144 == -Inf,Tmax144:= NA]
adcast_A_unique[Tmin72 == Inf | Tmin72 == -Inf,Tmin72:= NA]
adcast_A_unique[Tmin144 == Inf | Tmin144 == -Inf,Tmin144:= NA]
adcast_A_unique[,Tamp72:= Tmax72-Tmin72]
adcast_A_unique[,Tamp144:= Tmax144-Tmin144]
adcast_A_unique[,SD48:= shift(SD_value, 48,  type = 'lag')]
adcast_A_unique[,SD72:= shift(SD_value, 72,  type = 'lag')]
adcast_A_unique[,SD96:= shift(SD_value, 96,  type = 'lag')]
adcast_A_unique[,SD144:= shift(SD_value, 144,  type = 'lag')]
adcast_A_unique[,SDdif48:= SD_value - SD48]
adcast_A_unique[,SDdif72:= SD_value - SD72]
adcast_A_unique[,SDdif96:= SD_value - SD96]
adcast_A_unique[,SDdif144:= SD_value - SD144]

adcast_A_unique[,keep:= TRUE]
adcast_A_unique[,Date_check := shift(Date, 144,  type = 'lag')]
adcast_A_unique[difftime(Date, Date_check, units = "hours") > 144, keep := FALSE]
adcast_A_unique[event == 1 & !(Date %in% adcast_A_aval_days$Date), keep := FALSE]
adcast_A_unique[event == 1 & Date %in% adcast_A_aval_days$Date, keep := TRUE]
adcast_A_unique[, aval_before := which.min(difftime( adcast_A_aval_days$Date,Date, units = "hours") < 144)-1,.(Date)]
adcast_A_unique[aval_before > 0, aval_date_before := adcast_A_aval_days$Date[aval_before]]
adcast_A_unique[, norm_date := Date]
adcast_A_unique[event == 0 & difftime( Date, aval_date_before,units = "hours") < 144, keep:= FALSE]


adcast_A_unique <- subset(adcast_A_unique, select = -c(SD48,SD72,SD96,SD144,Date_check, aval_before, aval_date_before, norm_date))

adcast_A_updated <- adcast_A_unique[keep == TRUE]
names(adcast_A_updated)[1] <- "UTC"
adcast_A_updated <- subset(adcast_A_updated, select = -c(keep))

saveRDS(adcast_A_updated, file = 'adcast_A_upd.rds')

## adcast_C
adcast_C_sub <- subset(adcast_C, select = -c(ID, PLOT))
adcast_C_unique <- unique(adcast_C_sub) 
adcast_C_aval_days <- adcast_C[event == 1, max(UTC), .(ID)]
names(adcast_C_aval_days)[2] <- "Date"
names(adcast_C_unique)[1] <- "Date"

# New predictors
adcast_C_unique[,NSSsum72:= frollsum(NSS_value,72, align = "right", na.rm = T)]
adcast_C_unique[,NSSsum144:= frollsum(NSS_value,144, align = "right", na.rm = T)]
adcast_C_unique[,Rain_Ta_sum72:= frollsum(Rain_Ta_value,72, align = "right", na.rm = T)]
adcast_C_unique[,Rain_Ta_sum144:= frollsum(Rain_Ta_value,144, align = "right", na.rm = T)]
adcast_C_unique[,Tmax72:= frollapply(Tair_value,72, max,  align = "right", na.rm = T)]
adcast_C_unique[,Tmax144:= frollapply(Tair_value,144, max,  align = "right", na.rm = T)]
adcast_C_unique[,Tmin72:= frollapply(Tair_value,72, min,  align = "right", na.rm = T)]
adcast_C_unique[,Tmin144:= frollapply(Tair_value,144, min,  align = "right", na.rm = T)]
adcast_C_unique[Tmax72 == Inf | Tmax72 == -Inf,Tmax72:= NA]
adcast_C_unique[Tmax144 == Inf | Tmax144 == -Inf,Tmax144:= NA]
adcast_C_unique[Tmin72 == Inf | Tmin72 == -Inf,Tmin72:= NA]
adcast_C_unique[Tmin144 == Inf | Tmin144 == -Inf,Tmin144:= NA]
adcast_C_unique[,Tamp72:= Tmax72-Tmin72]
adcast_C_unique[,Tamp144:= Tmax144-Tmin144]
adcast_C_unique[,SD48:= shift(SD_value, 48,  type = 'lag')]
adcast_C_unique[,SD72:= shift(SD_value, 72,  type = 'lag')]
adcast_C_unique[,SD96:= shift(SD_value, 96,  type = 'lag')]
adcast_C_unique[,SD144:= shift(SD_value, 144,  type = 'lag')]
adcast_C_unique[,SDdif48:= SD_value - SD48]
adcast_C_unique[,SDdif72:= SD_value - SD72]
adcast_C_unique[,SDdif96:= SD_value - SD96]
adcast_C_unique[,SDdif144:= SD_value - SD144]

adcast_C_unique[,keep:= TRUE]
adcast_C_unique[,Date_check := shift(Date, 144,  type = 'lag')]
adcast_C_unique[difftime(Date, Date_check, units = "hours") > 144, keep := FALSE]
adcast_C_unique[event == 1 & !(Date %in% adcast_C_aval_days$Date), keep := FALSE]
adcast_C_unique[event == 1 & Date %in% adcast_C_aval_days$Date, keep := TRUE]
adcast_C_unique[, aval_before := which.min(difftime( adcast_C_aval_days$Date,Date, units = "hours") < 144)-1,.(Date)]
adcast_C_unique[aval_before > 0, aval_date_before := adcast_C_aval_days$Date[aval_before]]
adcast_C_unique[, norm_date := Date]
adcast_C_unique[event == 0 & difftime( Date, aval_date_before,units = "hours") < 144, keep:= FALSE]
adcast_C_unique <- subset(adcast_C_unique, select = -c(SD48,SD72,SD96,SD144,Date_check, aval_before, aval_date_before, norm_date))
adcast_C_updated <- adcast_C_unique[keep == TRUE]
names(adcast_C_updated)[1] <- "UTC"
adcast_C_updated <- subset(adcast_C_updated, select = -c(keep))

saveRDS(adcast_C_updated, file = 'adcast_C_upd.rds')
