library(data.table)
library(readr)
library(ggplot2)
library(lubridate)
library(writexl)
library(dplyr)  
library(zoo)
setwd ("c:/Users/marketa.souckova/Documents/laviny/")
######### READ DATA ######################
# loading the avalanche data (0: NAd,1: Ad)
Aval <- data.table(read_delim("./data/Aval_utf_8.txt", 
                              "\t", escape_double = FALSE, 
                              col_types = cols(A = col_character(), 
                              B = col_character(), C = col_character(), 
                              D = col_character(), E = col_character(), 
                              F = col_character(), G = col_character(), 
                              H = col_character(), J = col_character(),
                              K = col_number(), L = col_number(), 
                              M = col_number(), N = col_number(), 
                              O = col_character(), cadastr_letter = col_character(), 
                              cadastr_number = col_number(), date = col_character(), 
                              day = col_number(), event = col_number(), 
                              exposure = col_character(), locality = col_character(), 
                              month = col_number(), notes = col_character(), 
                              ranking = col_number(), season = col_character(), 
                              year = col_number()), trim_ws = TRUE))

# loading the meteo hourly and snow (daily- hourly value is copies 24 times) data (0: NAd,1: Ad)                                                                     
dta <- data.table(read_delim("./data/data_hourly_2004_2020.txt", 
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

# loading the meteo, snow daily data, we need to join LBOU with HARR and LUCB with PEC according to best formula
lbou_daily <- data.table(read_delim("./data/LBOU_daily_1961_2020.txt", "\t", 
                                    escape_double = FALSE, 
                                    col_types = cols(date = col_date(format = "%d.%m.%Y"),
                                                     Hprum = col_double(), SCE = col_double(),
                                                     SNO = col_double(), SVH = col_double(), 
                                                     SRA = col_double(), SSV = col_double(), 
                                                     Tprum = col_double(), T05prum = col_double(),
                                                     Fprum = col_double(), Fmax = col_double(), 
                                                     Dprum = col_double()), trim_ws = TRUE))
# manipulating dates format
dates <- gsub(pattern = " UTC", replacement = "", x = dta$date)
dta$date <- as.POSIXct(x = dates, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

times <- dta$time
dta$time <- as.POSIXct(x = times, format = "%Y-%m-%d %H:%M")

dta$DATE2 <- strtrim(x = as.character(dta$date), width = 10)
dta$DATE2 <- as.POSIXct(paste(dta$DATE2), format = "%Y-%m-%d")
# setting up breakpoint date to separate cold and warm temperature event for each year in period 2004-2020
dta[datum > as.Date("2003-10-30") & datum < as.Date("2004-03-06"), CAW:= "cold"]
dta[datum > as.Date("2004-03-07") & datum < as.Date("2004-10-30"), CAW:= "warm"]

dta[datum > as.Date("2004-10-30") & datum < as.Date("2005-03-07"), CAW:= "cold"]
dta[datum > as.Date("2005-03-08") & datum < as.Date("2005-10-30"), CAW:= "warm"]
dta[datum > as.Date("2005-11-1") & datum < as.Date("2006-03-06"), CAW:= "cold"]
dta[datum > as.Date("2006-03-07") & datum < as.Date("2006-10-30"), CAW:= "warm"]
dta[datum > as.Date("2006-11-1") & datum < as.Date("2007-01-30"), CAW:= "cold"]
dta[datum > as.Date("2007-01-31") & datum < as.Date("2007-10-30"), CAW:= "warm"]
dta[datum > as.Date("2007-11-1") & datum < as.Date("2008-03-26"), CAW:= "cold"]
dta[datum > as.Date("2008-03-27") & datum < as.Date("2008-10-30"), CAW:= "warm"]

dta[datum > as.Date("2008-11-1") & datum < as.Date("2009-03-26"), CAW:= "cold"]
dta[datum > as.Date("2009-03-27") & datum < as.Date("2009-10-30"), CAW:= "warm"]

dta[datum > as.Date("2009-11-1") & datum < as.Date("2010-03-11"), CAW:= "cold"]
dta[datum > as.Date("2010-03-12") & datum < as.Date("2010-10-30"), CAW:= "warm"]

dta[datum > as.Date("2010-11-1") & datum < as.Date("2011-02-27"), CAW:= "cold"]
dta[datum > as.Date("2011-02-28") & datum < as.Date("2011-10-30"), CAW:= "warm"]

dta[datum > as.Date("2011-11-1") & datum < as.Date("2012-02-10"), CAW:= "cold"]
dta[datum > as.Date("2012-02-11") & datum < as.Date("2012-10-30"), CAW:= "warm"]

dta[datum > as.Date("2012-11-1") & datum < as.Date("2013-03-31"), CAW:= "cold"]
dta[datum > as.Date("2013-04-01") & datum < as.Date("2013-10-30"), CAW:= "warm"]

dta[datum > as.Date("2013-11-1") & datum < as.Date("2014-01-30"), CAW:= "cold"]
dta[datum > as.Date("2014-01-31") & datum < as.Date("2014-10-30"), CAW:= "warm"]

dta[datum > as.Date("2014-11-1") & datum < as.Date("2015-02-08"), CAW:= "cold"]
dta[datum > as.Date("2015-02-09") & datum < as.Date("2015-10-30"), CAW:= "warm"]

dta[datum > as.Date("2015-11-1") & datum < as.Date("2016-01-23"), CAW:= "cold"]
dta[datum > as.Date("2016-01-24") & datum < as.Date("2016-10-30"), CAW:= "warm"]

dta[datum > as.Date("2016-11-1") & datum < as.Date("2017-01-11"), CAW:= "cold"]
dta[datum > as.Date("2017-01-12") & datum < as.Date("2017-10-30"), CAW:= "warm"]

dta[datum > as.Date("2017-11-1") & datum < as.Date("2018-03-03"), CAW:= "cold"]
dta[datum > as.Date("2018-03-04") & datum < as.Date("2018-10-30"), CAW:= "warm"]

dta[datum > as.Date("2018-11-1") & datum < as.Date("2019-01-26"), CAW:= "cold"]
dta[datum > as.Date("2019-01-27") & datum < as.Date("2019-10-30"), CAW:= "warm"]

dta[datum > as.Date("2019-11-1") & datum < as.Date("2020-03-30"), CAW:= "cold"]
dta[datum > as.Date("2020-03-31") & datum < as.Date("2020-10-30"), CAW:= "warm"]

dta$date <- dta$datum <-dta$time <- NULL
# merging data variables in melt format according to a date a CAW=cold and warm temperature event
#dta_melt <- melt(dta, id.vars = "DATE2")
dta_melt <- melt(dta, id.vars = c("DATE2", "CAW"))
# rolling means for time steps 24--144 hours, mean values for most of the variables, sum for SRA1H - precipitation totals and SSV1H: hours of sunlight (must be devided by 10)
hours <-  c(24,48,72,96,120,144)
for (i in c(1:length(hours))){
  sum_colname <- paste0("value", hours[i], "_sum")
  mean_colname <- paste0("value", hours[i], "_mean")
  colname <- paste0("value", hours[i])
  dta_melt[, c(sum_colname) := rollsum(value, hours[i], na.rm = T, align = "right", fill = NA), by = variable]
  dta_melt[, c(mean_colname) := rollmean(value, hours[i], na.rm = T, align = "right", fill = NA), by = variable]
  dta_melt[, c(colname) := ifelse(variable %in% c("SRA1H", "SSV1H"), get(sum_colname), get(mean_colname))]
  dta_melt[, c(sum_colname):= NULL ]
  dta_melt[, c(mean_colname):= NULL ]
  gc()
  print(hours[i])
}
saveRDS(object = dta_melt, file = "data/dta_melt_CAW.rds")
dta_melt <- readRDS(file = "data/dta_melt_CAW.rds")
# from rolling means we cut 6 days prior the avalanche event
Aval$DATE2 <- strtrim(x = Aval$date, width = 10)
# all avalanche dates set to 7:00
Aval$DATE3 <- as.POSIXct(paste0(Aval$DATE2, " 07:00"), format = "%d.%m.%Y %H:%M")
# all avalanche dates set to 24:00 - from midnight we calculate 6 days prior the avla event
Aval$DATE_OFF <- as.POSIXct(Aval$DATE3 + (17*60*60))
# search for the avalanches which are within a time window 
Aval_short <- Aval[DATE_OFF %between% c(dta_melt[complete.cases(dta_melt), DATE2][1], dta_melt[nrow(dta_melt), DATE2])]

Aval_short$ID <- paste0("Aval ", 1:nrow(Aval_short))
aval_list <- list()
for (i in 1:nrow(Aval_short)){
  id <- Aval_short[i,30]# column Aval ID 1,2,3...
  start <- Aval_short[i,29]#29 date for each avalanche
  stop <- Aval_short[i,29] - 5*24*60*60 # window, Posixct format works with seconds
  dta_aval <- dta_melt[DATE2 %between% c(stop[1], start[1]) , ]#take rows from start, stop and for these ones take meteo and snow variables
  dta_aval[, PLOT:= c(1:.N), by = variable]
  dta_aval$ID <- id #unique ID
  aval_list[[i]] <- dta_aval
  print(i)
}
#6183 lavin od roku 2004-2020 pro LBOU
aval_dtafr <- rbindlist(aval_list)
aval_dtafr <- merge(x = aval_dtafr, y = Aval_short[,.(event,ID)], by = "ID")

saveRDS(object = aval_dtafr, file = "data/aval_dtafr.rds")
aval_dtafr <- readRDS(file = "data/aval_dtafr.rds")
#column names 
colnames(aval_dtafr) <- c(colnames(aval_dtafr)[1:3], "var", colnames(aval_dtafr)[5:length(colnames(aval_dtafr))])
#colnames(aval_dtafr) <- c(colnames(aval_dtafr)[1:2], "var", colnames(aval_dtafr)[4:length(colnames(aval_dtafr))])
#melt acccording to a selected id.vars
aval_melt <- melt(data = aval_dtafr, id.vars = c("ID", "DATE2", "PLOT", "event", "var", "CAW"))
#aval_melt <- melt(data = aval_dtafr, id.vars = c("ID", "DATE2", "PLOT", "event", "var"))


aval_melt[, var_name:= paste0(var, "_", variable)]
saveRDS(object = aval_melt, file = "data/aval_melt.rds")
aval_melt <- readRDS(file = "data/aval_melt.rds")

# column format:dcast which fits to logistic regresion:glm model
adcast <- dcast.data.table(aval_melt, DATE2 + ID + PLOT + event + CAW  ~ var_name, value.var = 'value')
#selection of warm and cold events
adcast_W <-adcast[CAW == 'warm']
adcast_C <-adcast[CAW == 'cold']
write_csv(adcast, "C:/Users/marketa.souckova/Documents/laviny/aval_dcast.csv")
saveRDS(object = adcast, file = "data/adcast.rds")
adcast <- readRDS(file = "data/adcast.rds")

#most significant day within one variable
g_SCE <-glm(event ~ SCE_value + SCE_value24 + SCE_value48 + SCE_value72 + SCE_value96 + SCE_value120 + SCE_value144, data = adcast_W, family = 'binomial')
summary(g_SCE)
#gt_SCE_C = glm(event ~ SCE_value24 + SCE_value48 + SCE_value72 + SCE_value96 + SCE_value120 + SCE_value144, data = adcast_c, family = 'binomial')
#summary(gt_SCE_C)
g_T = glm(event ~  T_value + T_value24 + T_value48 + T_value72 + T_value96 + T_value120 + T_value144, data = adcast, family = 'binomial')
summary(gt_T)
g_SRA1H = glm(event ~ SRA1H_value +SRA1H_value24 + SRA1H_value48 + SRA1H_value72 + SRA1H_value96 + SRA1H_value120 + + SRA1H_value144, data = adcast, family = 'binomial')
summary (g_SRA1H )
g_SVH <-glm(event ~ SVH_value + SVH_value24 + SVH_value48 + SVH_value72 + SVH_value96 + SVH_value120 + SVH_value144, data = adcast_W, family = 'binomial')
summary(g_SVH)
g_SNO <-glm(event ~ SNO_value + SNO_value24 + SNO_value48 + SNO_value72 + SNO_value96 + SNO_value120 + SNO_value144, data = adcast_W, family = 'binomial')
summary(g_SNO)
g_SSV1H <-glm(event ~ SSV1H_value + SSV1H_value24 + SSV1H_value48 + SSV1H_value72 + SSV1H_value96 + SSV1H_value120 + SSV1H_value144, data = adcast_W, family = 'binomial')
summary(g_SSV1H)

# selection from all available variables the best predictors for warm events
dta = data.table (adcast_W)
candi = names(dta)[-1]
nn = length(candi)
new_candi = candi[5:nn]
M = list()
for (i in 1:length(new_candi)){
  print(i)
  M[[i]] = lm(dta$event ~ dta[[new_candi[i]]])# the  predictor
}

sM = data.table(new_candi, aic = lapply(M, AIC), lapply(M, function(x)summary(x)$coe[2,4] ))
sM[, aic := unlist(aic)]
sM[, V3 := unlist(V3)]

#write_xlsx(sM, 'sM.xlsx')

M2 = list()
candi = candi[candi!='SVH_value']
nn = length(candi)
new_candi = candi[5:nn]
for (i in 1:length(new_candi)){
  M2[[i]] = lm(dta$event ~ dta$SVH_value + dta[[new_candi[i]]] )
}

sM2 = data.table(new_candi, aic = lapply(M2, AIC), lapply(M2, function(x)summary(x)$coe[3,4] ))
sM2[, aic := unlist(aic)]
sM2[, V3 := unlist(V3)]

#write_xlsx(sM2, 'sM2.xlsx')

M3 = list()
candi = candi[candi!='SVH_value']# exclude already selected variable
candi = candi[candi!='SNO_value48']

nn = length(candi)
new_candi = candi[5:nn]
for (i in 1:length(new_candi)){
  M3[[i]] = lm(dta$event ~ dta$SVH_value + dta$SNO_value48 + dta[[new_candi[i]]] )#building of already selected variables and others
}

sM3 = data.table(new_candi, aic = lapply(M3, AIC), lapply(M3, function(x)summary(x)$coe[3,4] ))# i am not sure what position is coe [3,4]
sM3[, aic := unlist(aic)]
sM3[, V3 := unlist(V3)]
#write_xlsx(sM3, 'sM3.xlsx')

M4 = list()
candi = candi[candi!='SVH_value']
candi = candi[candi!='T_value24']* eliminate already chosen predictor

nn = length(candi)
new_candi = candi[5:nn]
for (i in 1:length(new_candi)){
  M4[[i]] = lm(dta$event ~ dta$SVH_value +  + dta$SNO_value48 + dta$T_value24 + dta[[new_candi[i]]] )
}

sM4 = data.table(new_candi, aic = lapply(M4, AIC), lapply(M4, function(x)summary(x)$coe[3,4] ))
sM4[, abs(aic := unlist(aic))]
sM4[, V3 := unlist(V3)]
#write_xlsx(sM4, 'sM4.xlsx')

M5 = list()
candi = candi[candi!='SVH_value']
candi = candi[candi!='SRA1H_value120'] #eliminate already chosen predictor

nn = length(candi)
new_candi = candi[5:nn]
for (i in 1:length(new_candi)){
  M5[[i]] = lm(dta$event ~ dta$SVH_value +  + dta$SNO_value48 + dta$T_value24 + dta$SRA1H_value120 + dta[[new_candi[i]]] )
}

sM5 = data.table(new_candi, aic = lapply(M5, AIC), lapply(M5, function(x)summary(x)$coe[3,4] ))
sM5[, aic := unlist(aic)]

sM5[, V3 := unlist(V3)]
#write_xlsx(sM5, 'sM5.xlsx')

M6 = list()
candi = candi[candi!='SSV1H_value72']
candi = candi[candi!='SRA1H_value120'] #eliminate already chosen predictor

nn = length(candi)
new_candi = candi[5:nn]
for (i in 1:length(new_candi)){
  M6[[i]] = lm(dta$event ~ dta$SVH_value +  + dta$SNO_value48 + dta$T_value24 + dta$SRA1H_value120 + dta$SSV1H_value72 + dta[[new_candi[i]]] )
}# best predictors for LOWEST AIC right now - dont know how to incorporate significant p value - right now I have mostly 0 p value

sM6 = data.table(new_candi, aic = lapply(M6, AIC), lapply(M6, function(x)summary(x)$coe[3,4] ))
sM6[, aic := unlist(aic)]
sM6[, V3 := unlist(V3)]
write_xlsx(sM6, 'sM6.xlsx')

# selection from all available variables the best predictors for cold events
dta = data.table (adcast_C)
candi = names(dta)[-1]
nn = length(candi)
new_candi = candi[5:nn]
M = list()
for (i in 1:length(new_candi)){
  print(i)
  M[[i]] = lm(dta$event ~ dta[[new_candi[i]]])# the  predictor
}

sM_C = data.table(new_candi, aic = lapply(M, AIC), lapply(M, function(x)summary(x)$coe[2,4] ))
sM_C[, aic := unlist(aic)]
sM_C[, V3 := unlist(V3)]

M2 = list()
candi = candi[candi!='RGLB1H_value48']
candi = candi[candi!='RGLB1H_value144']
nn = length(candi)
new_candi = candi[5:nn]
for (i in 1:length(new_candi)){
  M2[[i]] = lm(dta$event ~ dta$RGLB1H_value48 + dta[[new_candi[i]]] )
}

sM2_C = data.table(new_candi, aic = lapply(M2, AIC), lapply(M2, function(x)summary(x)$coe[3,4] ))
sM2_C[, aic := unlist(aic)]
sM2_C[, V3 := unlist(V3)]

write_xlsx(sM2_C, 'sM2_C.xlsx')

M3 = list()
candi = candi[candi!='T_value']
nn = length(candi)
new_candi = candi[5:nn]
for (i in 1:length(new_candi)){
  M3[[i]] = lm(dta$event ~ dta$RGLB1H_value48 + dta$T_value + dta[[new_candi[i]]] )
}

sM3_C = data.table(new_candi, aic = lapply(M3, AIC), lapply(M3, function(x)summary(x)$coe[2,4] ))
sM3_C[, aic := unlist(aic)]
sM3_C[, V3 := unlist(V3)]

write_xlsx(sM3_C, 'sM3_C.xlsx')

M3 = list()
candi = candi[candi!='SVH_value72']
nn = length(candi)
new_candi = candi[5:nn]
for (i in 1:length(new_candi)){
  M3[[i]] = lm(dta$event ~ dta$RGLB1H_value48 + dta$T_value + dta$T_value + dta[[new_candi[i]]] )
}

#g3 = glm(event ~ SCE_value24 + SNO_value24 + SVH_value24 + H_value24 + D_value24 + Fprum_value24 + Fmax_value24 + T_value24 +  SRA1H_value24 + SSV1H_value24 + T05_value24 +
           SCE_value48 + SNO_value48 + SVH_value48 + H_value48 + D_value48 + Fprum_value48 + Fmax_value48 + T_value48 +  SRA1H_value48 + SSV1H_value48 + T05_value48 +
           SCE_value72 + SNO_value72 + SVH_value72 + H_value72 + D_value72 + Fprum_value72 + Fmax_value72 + T_value72 +  SRA1H_value72 + SSV1H_value72 + T05_value72 +
           SCE_value96 + SNO_value96 + SVH_value96 + H_value96 + D_value96 + Fprum_value96 + Fmax_value96 + T_value96 +  SRA1H_value96 + SSV1H_value96 + T05_value96 +
           SCE_value120 + SNO_value120 + SVH_value120 + H_value120 + D_value120 + Fprum_value120 + Fmax_value120 + T_value120 +  SRA1H_value120 + SSV1H_value120 + T05_value120, data = adcast_W, family = 'binomial')
#summary (g3)

#g2 = glm(event ~ SCE_value24 + SNO_value24 + SVH_value24 + H_value24 + D_value24 + Fprum_value24 + Fmax_value24 + T_value24 +  SRA1H_value24 + SSV1H_value24 + T05_value24 +
           SCE_value48 + SNO_value48 + SVH_value48 + H_value48 + D_value48 + Fprum_value48 + Fmax_value48 + T_value48 +  SRA1H_value48 + SSV1H_value48 + T05_value48 +
           SCE_value72 + SNO_value72 + SVH_value72 + H_value72 + D_value72 + Fprum_value72 + Fmax_value72 + T_value72 +  SRA1H_value72 + SSV1H_value72 + T05_value72 +
           SCE_value96 + SNO_value96 + SVH_value96 + H_value96 + D_value96 + Fprum_value96 + Fmax_value96 + T_value96 +  SRA1H_value96 + SSV1H_value96 + T05_value96 +
           SCE_value120 + SNO_value120 + SVH_value120 + H_value120 + D_value120 + Fprum_value120 + Fmax_value120 + T_value120 +  SRA1H_value120 + SSV1H_value120 + T05_value120, data = adcast_C, family = 'binomial')
#summary (g2)

#aval_melt$var <- aval_melt$variable <- NULL
library(dplyr)
aval_nonaval <- Aval_short[,.(event, ID)]
#aval_melt_ID <- data.table(full_join(x = aval_melt, y = aval_nonaval, by = "ID"))
#Aval_short

#creating min, max for T and Fmax
####### MIN MAX pro T FMAX ###############
fmax_t <- list()
hours <- c(24,48,72,96,120,144)
for (i in c(1:length(hours))){
  a <- aval_melt[PLOT %between% c(1, hours[i]) & var %in% c("T", "Fmax") & variable == "value", {min = min(value, na.rm = T); max = max(value, na.rm = T); T0 = value[1]; list(min = min, max = max, T0 = T0)}, by = .(ID, event, var)]
  a$H <- hours[i]
  fmax_t[[i]] <- a
  print(hours[i])
}
fmax_t <- rbindlist(fmax_t)


########### SUM pro PRECIP ###########

precip <- list()
hours <- c(24,48,72,96,120,144)
for (i in c(1:length(hours))){
  a <- aval_melt[PLOT %between% c(1, hours[i]) & var %in% c("SRA1H") & variable == "value", {sum = sum(value, na.rm = T); T0 = value[1] ; list(sum = sum, T0 = T0)}, by = .(ID, event, var)]
  a$H <- hours[i]
  precip[[i]] <- a
  print(hours[i])
}
precip <- rbindlist(precip)

############ AVG for the rest of the variables ###########

avg_rest <- list()
hours <- c(24,48,72,96,120,144)
for (i in c(1:length(hours))){
  a <- aval_melt[PLOT %between% c(1, hours[i]) & !var %in% c("SRA1H", "T", "Fmax") & variable == "value", {avg = mean(value, na.rm = T); T0 = value[1] ; list(avg = avg, T0 = T0)}, by = .(ID, event, var)]
  a$H <- hours[i]
  avg_rest[[i]] <- a
  print(hours[i])
}

avg_rest <- rbindlist(avg_rest)

#################### ? SNOW DRIFT ? ######think how to set it up

#snow_drift <- list()
hours <- c(24,48,72,96,120,144)
for (i in c(1:length(hours))){
  a <- aval_melt[PLOT %between% c(1, hours[i]) & var %in% c("SRA1H") & variable == "value", {sum_p = sum(value, na.rm = T); list(sum_p = sum_p)}, by = .(ID, event)]
  b <- aval_melt[PLOT %between% c(1, hours[i]) & var %in% c("Fprum") & variable == "value", {mean_f = mean(value, na.rm = T); list(mean_f = mean_f)}, by = .(ID, event)]
  a$H <- hours[i]
  b$H <- hours[i]
  c <- merge(x = a, y = b, by = c("ID", "event", "H"))
  c$f4 <- (c$mean_f)^4  ##########  opravdu ????
  avg_rest[[i]] <- a
  print(hours[i])
}

# creating deltas of temperatures - important variable, it's just one values how to merge it with other parametres from aval_melt (rolling means of meteo and snow variables)
########### DELTA T ######################

delta_t <- fmax_t[var == "T", ]

delta_t[,delta_min:= T0 - min]
delta_t[,delta_max:= T0 - max]

########### DELTA SCE ####################

delta_sce <- list()
hours <- c(24,48,72,96,120,144)
for (i in c(1:length(hours))){
  a <- aval_melt[PLOT %between% c(1, hours[i]) & var %in% c("SCE") & variable == "value", {max_sce = max(value, na.rm = T); T0 = value[1] ; list(max_sce = max_sce, T0 = T0)}, by = .(ID, event, var)]
  a$H <- hours[i]
  delta_sce[[i]] <- a
  print(hours[i])
}

delta_sce <- rbindlist(delta_sce)

delta_sce[,delta_sce:=max_sce - T0]

############ FIVE DAY SUM of SNOW ##########################

sno_sum_5d <- aval_melt[PLOT %between% c(1, 120) & var %in% c("SNO") & variable == "value", .(sum = sum(unique(value), na.rm = T)), by = .(ID, event, var)]

################# MERGE ###################################

fmin <- dcast(fmax_t[var == "Fmax", .(ID, event, min,H, var)], ID+event~var+H, value.var = "min")
tmin <- dcast(fmax_t[var == "T", .(ID, event, min,H, var)], ID+event~var+H, value.var = "min")

fmax <- dcast(fmax_t[var == "Fmax", .(ID, event, max,H, var)], ID+event~var+H, value.var = "max")
tmax <- dcast(fmax_t[var == "T", .(ID, event, max,H, var)], ID+event~var+H, value.var = "max")

#delta_T_max <- dcast(delta_t[var == "T", .(ID, event, max,H, var)], ID+event~var+H, value.var = "max")

model_dta_T_Fmin <- merge(x = fmin, y = tmin, by = c("ID", "event"))
model_dta_T_Fmax <- merge(x = fmax, y = tmax, by = c("ID", "event"))
#model_delta_T_SCE <- merge (x= delta_sce, y = delta_t, by = c("ID", "event"))
###########################################################
############################################################


precip[,delta:= T0 - sum]

avg_rest[,delta:= T0 - avg]

unique

#boxplots for selected variables

ggplot(fmax_t)+
  geom_boxplot(aes(x = event, y = min, group = event, fill = as.factor(event)))+
  facet_grid(variable~H)+
  labs(fill = "Event")

ggplot(fmax_t)+
  geom_boxplot(aes(x = event, y = max, group = event, fill = as.factor(event)))+
  facet_grid(variable~H)+
  labs(fill = "Event")

ggplot(precip)+
  geom_boxplot(aes(x = event, y = sum, group = event, fill = as.factor(event)))+
  facet_grid(variable~H)+
  labs(fill = "Event")

ggplot(avg_rest)+
  geom_boxplot(aes(x = event, y = avg, group = event, fill = as.factor(event)))+
  facet_grid(variable~H, scales = "free_y")+
  labs(fill = "Event")

# variables with CAW separation
ggplot(precip[!is.na(CAW),])+
  geom_boxplot(aes(x = event, y = sum, group = event, fill = as.factor(event)))+
  facet_grid(var~H+CAW)+
  labs(fill = "event")

ggplot(avg_rest[!is.na(CAW),])+
  geom_boxplot(aes(x = event, y = avg, group = event, fill = as.factor (event)))+
  facet_grid(var ~ H + CAW, scales = "free_y")+
  labs(fill = "event")

install.packages("reshape2")
library(reshape2)

# rolling mean, I need mean for: "H", "SCE", "SVH", "T", "Fprum", "Fmax", "T05", "D")
#aval_melt_ID[, value24 := frollmean(value, 24, na.rm = T), by = .(variable, ID)]
#aval_melt_ID[, value48 := frollmean(value, 48, na.rm = T), by = .(variable, ID)]
#aval_melt_ID[, value72 := frollmean(value, 72, na.rm = T), by = .(variable, ID)]
#aval_melt_ID[, value96 := frollmean(value, 96, na.rm = T), by = .(variable, ID)]
#[, value120 := frollmean(value, 120, na.rm = T), by = .(variable, ID)]

# also I wanna establish min and max 24,48,72,96,120 of "T", "Fmax" and mark them as TAmin, TAmax
#??aval_melt_ID[, value24 := froll(value, 24, na.rm = T), by = .(c(T,Fmax) variable, ID)]
# sum of precipitation 24, 48, 72, 96, 120  
#aval_melt_ID[, value24 := frollsum(value, 24, na.rm = T), by = .(variable, ID)]
#also I wanna establish the difference (delta) in 48,72,96,120 hours of snow depth SDdif - BE AWARE that snow daity are just daily data and I copied the same value to every hour - DON'T use sum, Tmean, and Tmax 
#so called "48, 72, 96, 120 difference in snow heigh" and "24h differencence in mean, maximum air temperature"
# 5 day sum of new snow height - we have daily data so just sum one daily value of 5 days
# establish snow drift parameter: product of sum of hourly precipitation and avg. wind speed to the fourth power - Sdrift 0,24,48,72,96,120

summary(g)
library(ggplot2)
# Basic box plot
#p <- ggplot(g, aes(x=event, y=value24_SCE)) + 
  geom_boxplot()
# Rotate the box plot
  #p + coord_flip()
# Notched box plot
  #ggplot(g, aes(x=event, y=value24_SCE)) + 
  # geom_boxplot(notch=TRUE)
# Change outlier, color, shape and size
  #ggplot(ToothGrowth, aes(x=dose, y=len)) + 
  #geom_boxplot(outlier.colour="red", outlier.shape=8,
  #outlier.size=4)




