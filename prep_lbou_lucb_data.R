library(data.table)
library(readr)
library(zoo)

################### AVAL DATA ######################

Aval <- data.table(read_delim("./my_data/Aval_utf_8.txt", 
                              "\t", escape_double = FALSE, col_types = cols(A = col_character(), 
                                                                            B = col_character(), C = col_character(), 
                                                                            D = col_character(), E = col_character(), 
                                                                            F = col_character(), G = col_character(), 
                                                                            H = col_character(), J = col_character(), 
                                                                            K = col_number(), L = col_number(), 
                                                                            M = col_number(), N = col_number(), 
                                                                            O = col_number(), cadastr_letter = col_character(), 
                                                                            cadastr_number = col_number(), date = col_character(), 
                                                                            day = col_number(), event = col_number(), 
                                                                            exposure = col_character(), locality = col_character(), 
                                                                            month = col_number(), notes = col_character(), 
                                                                            ranking = col_number(), season = col_character(), 
                                                                            year = col_number()), trim_ws = TRUE, locale = locale(encoding = "windows-1252")))

Aval$DATE2 <- strtrim(x = Aval$date, width = 10)
Aval$DATE3 <- as.POSIXct(paste0(Aval$DATE2, " 07:00"), format = "%d.%m.%Y %H:%M")
Aval$DATE_OFF <- as.POSIXct(Aval$DATE3 + (17*60*60))

W_aval <- c(24:37)
E_aval <- c(1,2,3,4, 5, 6, 7,8,9,10,11,12, 13,14,15,16,17,18,19,20,21,22,23,38,39)
Aval[, locality := ifelse(cadastr_number %in% W_aval, 'W', 'E')]
aval_lbou <- Aval[locality == "W",]
aval_lucb <- Aval[locality == "E",]
aval_lbou[event == 1]
aval_lucb <- aval_lucb[event == 1]

non_aval_lucb <- Aval[year(DATE3) > 2008 & month(DATE3) %in% c(10, 11, 12, 1, 2, 3, 4, 5)]
non_aval_lbou <- Aval[year(DATE3) > 2003 & month(DATE3) %in% c(10, 11, 12, 1, 2, 3, 4, 5)]
non_aval_lbou[, locality:= "W"]
non_aval_lucb[, locality:= "E"]

aval_lbou <- aval_lbou[year(DATE3) > 2003]
aval_lucb <- aval_lucb[year(DATE3) > 2008]

aval_total_lucb <- rbind(aval_lucb, non_aval_lucb)
aval_total_lbou <- rbind(aval_lbou, non_aval_lbou)

aval_total_lucb$ID <- paste0("Aval ", 1:nrow(aval_total_lucb))
aval_total_lbou$ID <- paste0("Aval ", 1:nrow(aval_total_lbou))

########### METEO DATA ###############

dta_lbou <- data.table(read_delim("./my_data/data_hourly_2004_2020.txt", 
                             "\t", escape_double = FALSE, col_types = cols(D = col_double(), 
                                                                           Fmax = col_double(), Fprum = col_double(), 
                                                                           H = col_double(), RGLB1H = col_double(), 
                                                                           SCE = col_double(), SNO = col_double(), 
                                                                           SRA1H = col_double(), SSV1H = col_double(), 
                                                                           SVH = col_double(), T = col_double(), 
                                                                           T05 = col_double(), date = col_character(), 
                                                                           datum = col_date(format = "%Y-%m-%d"), 
                                                                           time = col_character()), trim_ws = TRUE))

dta_lucb <- data.table(read_delim("./LUCB_dta_hourly_2004_2020.txt", 
                                  "\t", escape_double = FALSE, col_types = cols(datum = col_date(format = "%Y-%m-%d"),SCE = col_double(), SNO = col_double(), SVH = col_double(),
                                                                                H = col_double(), D = col_double(),
                                                                                Fprum = col_double(), Fmax = col_double(), 
                                                                                T = col_double(), SRA1H = col_double(), SSV1H = col_double(), 
                                                                                T05 = col_double(), TPM = col_double(), 
                                                                                date = col_character()), 
                                  trim_ws = TRUE))

dta_lucb <- dta_lucb[c(469:nrow(dta_lucb)),]

dta_lbou$time <- dta_lbou$RGLB1H <- NULL
dta_lucb$TPM <- NULL

dta_lbou <- dta_lbou[,c(1:11,13,12)]

dta_lbou$stat <- "LBOU"
dta_lucb$stat <- "LUCB"

dta <- rbind(dta_lbou, dta_lucb)

dates <- gsub(pattern = " UTC", replacement = "", x = dta$date)
dta$date <- as.POSIXct(x = dates, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

dta$DATE2 <- strtrim(x = as.character(dta$date), width = 10)
dta$DATE2 <- as.POSIXct(paste(dta$date), format = "%Y-%m-%d")

###### WARM COLD ######

dta[datum > as.Date("2003-10-30") & datum <= as.Date("2004-03-06"), CAW:= "cold"]
dta[datum >= as.Date("2004-03-07") & datum <= as.Date("2004-10-30"), CAW:= "warm"]

dta[datum >= as.Date("2004-10-30") & datum <= as.Date("2005-03-07"), CAW:= "cold"]
dta[datum >= as.Date("2005-03-08") & datum <= as.Date("2005-10-31"), CAW:= "warm"]
dta[datum >= as.Date("2005-11-1") & datum <= as.Date("2006-03-06"), CAW:= "cold"]
dta[datum >= as.Date("2006-03-07") & datum <= as.Date("2006-10-31"), CAW:= "warm"]
dta[datum >= as.Date("2006-11-1") & datum <= as.Date("2007-01-30"), CAW:= "cold"]
dta[datum >= as.Date("2007-01-31") & datum <= as.Date("2007-10-31"), CAW:= "warm"]
dta[datum >= as.Date("2007-11-1") & datum <= as.Date("2008-03-26"), CAW:= "cold"]
dta[datum >= as.Date("2008-03-27") & datum <= as.Date("2008-10-31"), CAW:= "warm"]

dta[datum >= as.Date("2008-11-1") & datum <= as.Date("2009-03-26"), CAW:= "cold"]
dta[datum >= as.Date("2009-03-27") & datum <= as.Date("2009-10-31"), CAW:= "warm"]

dta[datum >= as.Date("2009-11-1") & datum <= as.Date("2010-03-11"), CAW:= "cold"]
dta[datum >= as.Date("2010-03-12") & datum <= as.Date("2010-10-31"), CAW:= "warm"]

dta[datum >= as.Date("2010-11-1") & datum <= as.Date("2011-02-27"), CAW:= "cold"]
dta[datum >= as.Date("2011-02-28") & datum <= as.Date("2011-10-31"), CAW:= "warm"]

dta[datum >= as.Date("2011-11-1") & datum <= as.Date("2012-02-10"), CAW:= "cold"]
dta[datum >= as.Date("2012-02-11") & datum <= as.Date("2012-10-31"), CAW:= "warm"]

dta[datum >= as.Date("2012-11-1") & datum <= as.Date("2013-03-31"), CAW:= "cold"]
dta[datum >= as.Date("2013-04-01") & datum <= as.Date("2013-10-31"), CAW:= "warm"]

dta[datum >= as.Date("2013-11-1") & datum <= as.Date("2014-01-30"), CAW:= "cold"]
dta[datum >= as.Date("2014-01-31") & datum <= as.Date("2014-10-31"), CAW:= "warm"]

dta[datum >= as.Date("2014-11-1") & datum <= as.Date("2015-02-08"), CAW:= "cold"]
dta[datum >= as.Date("2015-02-09") & datum <= as.Date("2015-10-31"), CAW:= "warm"]

dta[datum >= as.Date("2015-11-1") & datum <= as.Date("2016-01-23"), CAW:= "cold"]
dta[datum >= as.Date("2016-01-24") & datum <= as.Date("2016-10-31"), CAW:= "warm"]

dta[datum >= as.Date("2016-11-1") & datum <= as.Date("2017-01-11"), CAW:= "cold"]
dta[datum >= as.Date("2017-01-12") & datum <= as.Date("2017-10-31"), CAW:= "warm"]

dta[datum >= as.Date("2017-11-1") & datum <= as.Date("2018-03-03"), CAW:= "cold"]
dta[datum >= as.Date("2018-03-04") & datum <= as.Date("2018-10-31"), CAW:= "warm"]

dta[datum >= as.Date("2018-11-1") & datum <= as.Date("2019-01-26"), CAW:= "cold"]
dta[datum >= as.Date("2019-01-27") & datum <= as.Date("2019-10-31"), CAW:= "warm"]

dta[datum >= as.Date("2019-11-1") & datum <= as.Date("2020-03-30"), CAW:= "cold"]
dta[datum >= as.Date("2020-03-31") & datum <= as.Date("2020-10-31"), CAW:= "warm"]

dta$date <- dta$datum <- NULL
dta$T05 <- NULL
dta$SCE <- NULL

dta_melt <- melt(dta, id.vars = c("DATE2", "CAW", "stat"))

############# ROLL DATA ###########

hours <-  c(24,48,72,96,120,144)
for (i in c(1:length(hours))){
  sum_colname <- paste0("value", hours[i], "_sum")
  mean_colname <- paste0("value", hours[i], "_mean")
  colname <- paste0("value", hours[i])
  dta_melt[, c(sum_colname) := rollsum(value, hours[i], na.rm = T, align = "right", fill = NA), by = .(variable, stat)]
  dta_melt[, c(mean_colname) := rollmean(value, hours[i], na.rm = T, align = "right", fill = NA), by = .(variable, stat)]
  dta_melt[, c(colname) := ifelse(variable %in% c("SRA1H", "SSV1H"), get(sum_colname), get(mean_colname))]
  dta_melt[, c(sum_colname):= NULL ]
  dta_melt[, c(mean_colname):= NULL ]
  gc()
  print(hours[i])
}

aaa <- dta_melt[variable == "T",.(DATE2, CAW, stat, variable, value)]
aaa[, variable:= "Tdiff"]
hours <-  c(24,48,72,96,120,144)
for (i in c(1:length(hours))){
  colname <- paste0("value", hours[i])
  aaa[, c(colname) := rollapply(data = value, width = hours[i], FUN = function(X){X[length(X)] - X[1]}, align = "right", fill = NA), by = stat]
  print(hours[i])
}

dta_melt <- rbind(dta_melt, aaa)

############## FIVE DAYS LUCB #########

aval_total_lucb <- aval_total_lucb[DATE_OFF >= dta_melt[stat == "LUCB", min(DATE2)] + 5*24*60*60, ]

aval_lucb_list <- list()
for (i in 1:nrow(aval_total_lucb)){
  id <- aval_total_lucb[i,ID]
  start <- aval_total_lucb[i,29]
  stop <- aval_total_lucb[i,29] - 5*24*60*60
  dta_aval <- dta_melt[stat == "LUCB" & DATE2 %between% c(stop[1], start[1]) ]
  dta_aval[, PLOT:= c(1:.N), by = variable]
  dta_aval$ID <- id
  aval_lucb_list[[i]] <- dta_aval
  print(i)
  
}

aval_lucb_dtafr <- rbindlist(aval_lucb_list)
aval_lucb_dtafr <- merge(x = aval_lucb_dtafr, y = aval_total_lucb[,.(event,ID)], by = "ID")

############## FIVE DAYS LBOU #########

aval_total_lbou <- aval_total_lbou[DATE_OFF >= dta_melt[stat == "LBOU", min(DATE2)] + 5*24*60*60, ]

aval_lbou_list <- list()
for (i in 1:nrow(aval_total_lbou)){
  id <- aval_total_lbou[i,ID]
  start <- aval_total_lbou[i,29]
  stop <- aval_total_lbou[i,29] - 5*24*60*60
  dta_aval <- dta_melt[stat == "LBOU" & DATE2 %between% c(stop[1], start[1]) , ]
  dta_aval[, PLOT:= c(1:.N), by = variable]
  dta_aval$ID <- id 
  aval_lbou_list[[i]] <- dta_aval
  print(i)
}

aval_lbou_dtafr <- rbindlist(aval_lbou_list)
aval_lbou_dtafr <- merge(x = aval_lbou_dtafr, y = aval_total_lbou[,.(event,ID)], by = "ID")

colnames(aval_lbou_dtafr)[5] <- "var"
colnames(aval_lucb_dtafr)[5] <- "var"

aval_lucb_melt <- melt(data = aval_lucb_dtafr, id.vars = c("ID", "DATE2", "PLOT", "stat", "var", "CAW", "event"))
aval_lbou_melt <- melt(data = aval_lbou_dtafr, id.vars = c("ID", "DATE2", "PLOT", "stat", "var", "CAW", "event"))

aval_melt_total <- rbind(aval_lbou_melt, aval_lucb_melt)
aval_melt_total[, var_name:= paste0(var, "_", variable)]

saveRDS(object = aval_melt_total, file = "./my_data/lbou_lucb_data.rds")
aval_melt_total <- readRDS(file = "./my_data/lbou_lucb_data.rds")

adcast <- dcast.data.table(aval_melt_total, DATE2 + ID + PLOT + event + CAW + stat + event  ~ var_name, value.var = 'value')

########### GLM LBOU #########################

glm_data_lbou <- adcast[stat == "LBOU"]
glm_data_lbou_W <- adcast[CAW == "warm"]
glm_data_lbou_C <- adcast[CAW == "cold"]

g_SCE_lbou <- glm(event ~ SCE_value + SCE_value24 + SCE_value48 + SCE_value72 + SCE_value96 + SCE_value120 + SCE_value144, data = glm_data_lbou, family = 'binomial')

g_T_lbou <- glm(event ~  T_value + T_value24 + T_value48 + T_value72 + T_value96 + T_value120 + T_value144, data = glm_data_lbou_W, family = 'binomial')

g_SRA1H_lbou <- glm(event ~ SRA1H_value +SRA1H_value24 + SRA1H_value48 + SRA1H_value72 + SRA1H_value96 + SRA1H_value120 + + SRA1H_value144, data = glm_data_lbou, family = 'binomial')

g_SVH_lbou <- glm(event ~ SVH_value + SVH_value24 + SVH_value48 + SVH_value72 + SVH_value96 + SVH_value120 + SVH_value144, data = glm_data_lbou_W, family = 'binomial')

g_SNO_lbou <- glm(event ~ SNO_value + SNO_value24 + SNO_value48 + SNO_value72 + SNO_value96 + SNO_value120 + SNO_value144, data = glm_data_lbou_W, family = 'binomial')

g_SSV1H_lbou <- glm(event ~ SSV1H_value + SSV1H_value24 + SSV1H_value48 + SSV1H_value72 + SSV1H_value96 + SSV1H_value120 + SSV1H_value144, data = glm_data_lbou_W, family = 'binomial')

########### GLM LUCB #########################

glm_data_lucb <- adcast[stat == "LUCB"]
glm_data_lucb_W <- adcast[CAW == "warm"]
glm_data_lucb_C <- adcast[CAW == "cold"]

g_SCE_lucb <- glm(event ~ SCE_value + SCE_value24 + SCE_value48 + SCE_value72 + SCE_value96 + SCE_value120 + SCE_value144, data = glm_data_lucb, family = 'binomial')

g_T_lucb <- glm(event ~  T_value + T_value24 + T_value48 + T_value72 + T_value96 + T_value120 + T_value144, data = glm_data_lucb_W, family = 'binomial')

g_SRA1H_lucb <- glm(event ~ SRA1H_value +SRA1H_value24 + SRA1H_value48 + SRA1H_value72 + SRA1H_value96 + SRA1H_value120 + + SRA1H_value144, data = glm_data_lucb, family = 'binomial')

g_SVH_lucb <- glm(event ~ SVH_value + SVH_value24 + SVH_value48 + SVH_value72 + SVH_value96 + SVH_value120 + SVH_value144, data = glm_data_lucb_W, family = 'binomial')

g_SNO_lucb <- glm(event ~ SNO_value + SNO_value24 + SNO_value48 + SNO_value72 + SNO_value96 + SNO_value120 + SNO_value144, data = glm_data_lucb_W, family = 'binomial')

g_SSV1H_lucb <- glm(event ~ SSV1H_value + SSV1H_value24 + SSV1H_value48 + SSV1H_value72 + SSV1H_value96 + SSV1H_value120 + SSV1H_value144, data = glm_data_lucb_W, family = 'binomial')
