library(data.table)
library(zoo)
setwd ("c:/Users/marketa.souckova/Documents/laviny/daily/")
################### AVAL DATA ######################

aval <- data.table(read.table("c:/Users/marketa.souckova/Documents/laviny/daily/avalanche_event1.txt", header = T, sep = "\t"))

str(aval)
aval$date <- as.character(aval$date)
aval$date2 <- as.Date(aval$date, format = "%d.%m.%Y")

W_aval <- c(24:37)
E_aval <- c(1,2,3,4, 5, 6, 7,8,9,10,11,12, 13,14,15,16,17,18,19,20,21,22,23,38,39)

aval[, locality := ifelse(cadastr_number %in% W_aval, 'W', 'E')]

aval_lbou <- aval[locality == "W",]
aval_lucb <- aval[locality == "E",]

non_aval <- data.table(read.table("c:/Users/marketa.souckova/Documents/laviny/daily/avalanche_event1_0.txt", header = T, sep = "\t"))
non_aval$date <- as.character(non_aval$date)
non_aval$date2 <- as.Date(non_aval$date, format = "%d.%m.%Y")

non_aval_lucb <- non_aval[!date2 %in% aval_lucb$date2 & month(date2) %in% c(10, 11, 12, 1, 2, 3, 4, 5) & event == 0]
non_aval_lbou <- non_aval[!date2 %in% aval_lbou$date2 & month(date2) %in% c(10, 11, 12, 1, 2, 3, 4, 5) & event == 0]

non_aval_lbou[, locality:= "W"]
non_aval_lucb[, locality:= "E"]

aval_total_lucb <- rbind(aval_lucb, non_aval_lucb)
aval_total_lbou <- rbind(aval_lbou, non_aval_lbou)

aval_total_lucb$ID <- paste0("Aval ", 1:nrow(aval_total_lucb))
aval_total_lbou$ID <- paste0("Aval ", 1:nrow(aval_total_lbou))

########### METEO DATA ###############

dta <- data.frame(readRDS("c:/Users/marketa.souckova/Documents/laviny/daily/Meteo_4Aval.RDS"))

dta <- dta[,-which(grepl(pattern = "Station", colnames(dta)))]
dta <- dta[,-which(grepl(pattern = "note", colnames(dta)))]
dta <- dta[,-which(grepl(pattern = "grad", colnames(dta)))]


colnames(dta) <- c("Date", "WSavg", "SD", "NSS", "SLd", "P", "Tair", "H", "Rain_Tw", "Rain_Ta")

dta2 <- data.table(read.table("c:/Users/marketa.souckova/Documents/laviny/daily/LBOU_daily_1961_2020.txt", header = T, sep = ","))
dta2 <- dta2[, .(date, SVH)]
colnames(dta2) <- c("date", "SWE")
dta2$date2 <- as.Date(as.character(dta2$date), format = "%Y-%m-%d")
dta2$date <- NULL

dta2_fill <- dta2[date2 %between% c("1980-01-06", "2020-10-26")]
###### linear interpolation of SWE
dta2_fill$SWE2 <- na.approx(object = dta2_fill$SWE)
dta2$SWE2 <- dta2$SWE
dta2_all <- rbind(dta2[date2 < as.Date("1980-01-06")], dta2_fill, dta2[date2 > as.Date("2020-10-26")])
dta2_all$SWE <- NULL
colnames(dta2_all) <- c("date2", "SWE")

dta_all <- data.table(merge(x = dta, y = dta2_all, by.x = "Date", by.y = "date2", all.x = T))

###### adding missing dates
datum <- data.frame(Date=seq.Date(from = as.Date("1961-01-01"), to = as.Date("2020-12-31"), by = "day"))
dta_all <- merge(x = dta_all, y = datum, by.x = "Date", by.y = "Date", all.y = T)

saveRDS(dta_all, file = "c:/Users/marketa.souckova/Documents/laviny/denni_data.rds")
dta <- readRDS("C:/Users/marketa.souckova/OneDrive - CZU v Praze/R/Krkonose/Rcode/RDS/denni_data.rds")
dta_all <- dta_all[Date >= as.Date("1962-07-01")]

################# WARM COLD ##################

#dta_all <- data.table(dta_all)
#dta_all[month(Date) %in% c(11,12,1,2,3,4), CAW:= "cold"]
#dta_all[!month(Date) %in% c(11,12,1,2,3,4), CAW:= "warm"]

library(readxl)
brks <- read_excel("c:/Users/marketa.souckova/Documents/laviny/daily/W_C_events_dates_op.xlsx", 
                   col_types = c("numeric", "text", "numeric", 
                                 "numeric", "numeric", "text", "text", 
                                 "text", "text", "text", "text"))
brks <- as.Date(brks$toR, format = "%Y-%m-%d")
brks <- na.omit(brks)

end.brks <- seq.Date(from = as.Date("1962-06-30"), to = as.Date("2020-06-30"), by = "year")

fin.brks <- sort(c(brks, end.brks))

dta_all <- data.table(dta_all)
dta_all[, CAW:= cut(x = Date, breaks = fin.brks, labels = c(1:(length(fin.brks)-1)))]

dta_all$CAW <- as.numeric(dta_all$CAW)

dta_all[is.na(CAW), CAW:= 113]

dta_all[,.(min(Date), max(Date)), by= CAW]

labs <- rep(c("cold", "warm"), 57)[c(-114,-113)]

dta_all[, CAW2:= cut(x = Date, breaks = fin.brks, labels = labs)]

dta_all[is.na(CAW2), CAW2:= "warm"]

dta_all$CAW <- NULL
colnames(dta_all)[ncol(dta_all)] <- "CAW"
setwd ("c:/Users/marketa.souckova/Documents/laviny/")
dta_melt <- melt(dta_all, id.vars = c("Date", "CAW")) # this is probably not needed to change, since the needed for cycles are not using this dta for aval info
saveRDS(dta_melt, "./data/dta_melt_daily.rds")
dta_melt <- readRDS(file = "./data/dta_melt_daily.rds")
#mooving averadge 3,5,10 days
days <-  c(3,5,10)
for (i in c(1:length(days))){
  sum_colname <- paste0("value", days[i], "_sum")
  mean_colname <- paste0("value", days[i], "_mean")
  colname <- paste0("value", days[i])
  dta_melt[, c(sum_colname) := rollsum(value, days[i], na.rm = T, align = "right", fill = NA), by = .(variable)]
  dta_melt[, c(mean_colname) := rollmean(value, days[i], na.rm = T, align = "right", fill = NA), by = .(variable)]
  dta_melt[, c(colname) := ifelse(variable %in% c("P", "SLd"), get(sum_colname), get(mean_colname))]
  dta_melt[, c(sum_colname):= NULL ]
  dta_melt[, c(mean_colname):= NULL ]
  gc()
  print(days[i])
}

############## TEN DAYS LUCB #########

aval_total_lucb <- aval_total_lucb[date2 >= dta_melt[, min(Date)] + 10 & date2 <= as.Date("2020-12-31"), ]

aval_total_C_lucb <- aval_total_lucb[ C == 2 | event == 0, ]
aval_total_A_lucb <- aval_total_lucb[ A %in% c(2, 3, 4) | event == 0, ]

## Wet avalanches LUCB, C = 2
aval_lucb_C_list <- list()

for (i in 1:nrow(aval_total_C_lucb)){
  id <- aval_total_C_lucb[i,ID]
  start <- aval_total_C_lucb[i,date2]
  stop <- aval_total_C_lucb[i,date2] - 10
  dta_aval <- dta_melt[Date %between% c(stop[1], start[1]) ]
  dta_aval[, PLOT:= c(1:.N), by = variable]
  dta_aval$ID <- id
  aval_lucb_C_list[[i]] <- dta_aval
  print(i)
  
}
#3052 > 2903
aval_lucb_C_dtafr <- rbindlist(aval_lucb_C_list)
aval_lucb_C_dtafr <- merge(x = aval_lucb_C_dtafr, y = aval_total_C_lucb[,.(event,ID)], by = "ID")

#############

## Slab avalanches LUCB, A = 2,3,4
aval_lucb_A_list <- list()

for (i in 1:nrow(aval_total_A_lucb)){
  id <- aval_total_A_lucb[i,ID]
  start <- aval_total_A_lucb[i,date2]
  stop <- aval_total_A_lucb[i,date2] - 10
  dta_aval <- dta_melt[Date %between% c(stop[1], start[1]) ]
  dta_aval[, PLOT:= c(1:.N), by = variable]
  dta_aval$ID <- id
  aval_lucb_A_list[[i]] <- dta_aval
  print(i)
  
}
#3052 > 2903
aval_lucb_A_dtafr <- rbindlist(aval_lucb_A_list)
aval_lucb_A_dtafr <- merge(x = aval_lucb_A_dtafr, y = aval_total_A_lucb[,.(event,ID)], by = "ID")

#############

#### All avalanches together LUCB - original
aval_lucb_list <- list() 

for (i in 1:nrow(aval_total_lucb)){
  id <- aval_total_lucb[i,ID]
  start <- aval_total_lucb[i,date2]
  stop <- aval_total_lucb[i,date2] - 10
  dta_aval <- dta_melt[Date %between% c(stop[1], start[1]) ]
  dta_aval[, PLOT:= c(1:.N), by = variable]
  dta_aval$ID <- id
  aval_lucb_list[[i]] <- dta_aval
  print(i)
  
}
#3052 > 2903
aval_lucb_dtafr <- rbindlist(aval_lucb_list)
aval_lucb_dtafr <- merge(x = aval_lucb_dtafr, y = aval_total_lucb[,.(event,ID)], by = "ID")


############## FIVE DAYS LBOU #########

aval_total_lbou <- aval_total_lbou[date2 >= dta_melt[, min(Date)] + 10 & date2 <= as.Date("2020-12-31"), ]

aval_total_C_lbou <- aval_total_lbou[C == 2 | event == 0, ]
aval_total_A_lbou <- aval_total_lbou[A %in% c(2, 3, 4) | event == 0, ]

## Wet avalanches lbou, C = 2
aval_lbou_C_list <- list()

for (i in 1:nrow(aval_total_C_lbou)){
  id <- aval_total_C_lbou[i,ID]
  start <- aval_total_C_lbou[i,date2]
  stop <- aval_total_C_lbou[i,date2] - 10
  dta_aval <- dta_melt[Date %between% c(stop[1], start[1]) ]
  dta_aval[, PLOT:= c(1:.N), by = variable]
  dta_aval$ID <- id
  aval_lbou_C_list[[i]] <- dta_aval
  print(i)
  
}
#3052 > 2903
aval_lbou_C_dtafr <- rbindlist(aval_lbou_C_list)
aval_lbou_C_dtafr <- merge(x = aval_lbou_C_dtafr, y = aval_total_C_lbou[,.(event,ID)], by = "ID")

#############

## Slab avalanches lbou, A = 2,3,4
aval_lbou_A_list <- list()

for (i in 1:nrow(aval_total_A_lbou)){
  id <- aval_total_A_lbou[i,ID]
  start <- aval_total_A_lbou[i,date2]
  stop <- aval_total_A_lbou[i,date2] - 10
  dta_aval <- dta_melt[Date %between% c(stop[1], start[1]) ]
  dta_aval[, PLOT:= c(1:.N), by = variable]
  dta_aval$ID <- id
  aval_lbou_A_list[[i]] <- dta_aval
  print(i)
  
}
#3052 > 2903
aval_lbou_A_dtafr <- rbindlist(aval_lbou_A_list)
aval_lbou_A_dtafr <- merge(x = aval_lbou_A_dtafr, y = aval_total_A_lbou[,.(event,ID)], by = "ID")

#############

#### All avalanches together LBOU
aval_lbou_list <- list() # upravit
#change colum number to Aval Id

for (i in 1:nrow(aval_total_lbou)){
  id <- aval_total_lbou[i,ID]
  start <- aval_total_lbou[i,date2] # sloupec
  stop <- aval_total_lbou[i,date2] - 10
  dta_aval <- dta_melt[Date %between% c(stop[1], start[1]) , ]
  dta_aval[, PLOT:= c(1:.N), by = variable]
  dta_aval$ID <- id 
  aval_lbou_list[[i]] <- dta_aval
  print(i)
}
#4601 > 4254
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

aval_melt_lucb_total$stat <- "LUCB"
aval_melt_lbou_total$stat <- "LBOU"

#aval_melt_total <- rbind(aval_melt_lucb_total, aval_melt_lbou_total)
aval_melt_total <-aval_melt_lbou_total
aval_melt_total[, var_name:= paste0(var, "_", variable)]

adcast_A <- dcast.data.table(aval_melt_total[type == "slab"], Date + ID + PLOT + event + CAW + stat  ~ var_name, value.var = 'value')
adcast_C <- dcast.data.table(aval_melt_total[type == "wet"], Date + ID + PLOT + event + CAW + stat  ~ var_name, value.var = 'value')
adcast_all <- dcast.data.table(aval_melt_total[type == "all"], Date + ID + PLOT + event + CAW + stat  ~ var_name, value.var = 'value')
saveRDS(adcast_A, "./data/adcast_A_daily.rds")
saveRDS(adcast_C, "./data/adcast_C_daily.rds")
saveRDS(adcast_all, "./data/adcast_all_daily.rds")
readRDS("./data/adcast_A_daily.rds")
readRDS("./data/adcast_C_daily.rds")
adcast_C_daily <- readRDS("~/laviny/data/adcast_C_daily.rds")
readRDS("./data/adcast_all_daily.rds")

#glm
adcast_all$SD_value <-  adcast_all$SD_value3 <- adcast_all$SD_value5 <- adcast_all$SD_value10 <- NULL
adcast_all$Rain_Tw_value  <- adcast_all$Rain_Tw_value3  <- adcast_all$Rain_Tw_value5  <- adcast_all$Rain_Tw_value10 <-NULL
adcast_all$Rain_Ta_value <- adcast_all$Rain_Ta_value3 <- adcast_all$Rain_Ta_value5 <- adcast_all$Rain_Ta_value10 <-NULL
adcast_all$H_value <- adcast_all$H_value3 <- adcast_all$H_value5 <- adcast_all$H_value10 <-NULL
glm_data_lbou_d <- adcast_all[stat == "LBOU"]
glm_data_lbou_W_d <- glm_data_lbou_d[CAW == "warm"]
glm_data_lbou_C_d <- glm_data_lbou_d[CAW == "cold"]


# selection from all available variables the best predictors for warm events ALL AVALANCHES
dta = data.table(glm_data_lbou_W_d)
candi = names(dta)[-1]
nn = length(candi)
new_candi = candi[6:nn]
M = list()
for (i in 1:length(new_candi)){
  print(i)
  M[[i]] = glm(dta$event ~ dta[[new_candi[i]]])# the  predictor
}

sM = data.table(new_candi, aic = lapply(M, AIC), lapply(M, function(x)summary(x)$coe[2,3]))
sM[, aic := unlist(aic)]
sM[, V3 := unlist(V3)]
sM[,abs := abs(aic)]

M2 = list()
candi = candi[candi!='SWE_value']
nn = length(candi)
new_candi = candi[6:nn]
for (i in 1:length(new_candi)){
  print(i)
  M2[[i]] = glm(dta$event ~ dta$SWE_value + dta[[new_candi[i]]] )
}

sM2 = data.table(new_candi, aic = lapply(M2, AIC), lapply(M2, function(x)summary(x)$coe[2,3]))
sM2[, aic := unlist(aic)]
sM2[, V3 := unlist(V3)]
sM2[,abs := abs(aic)]

M3 = list()
# exclude already selected variable
candi = candi[candi!='SLd_value']

nn = length(candi)
new_candi = candi[6:nn]
for (i in 1:length(new_candi)){
  print(i)
  M3[[i]] = glm(dta$event ~ dta$SWE_value + dta$SLd_value + dta[[new_candi[i]]] )#building of already selected variables and others
}

sM3 = data.table(new_candi, aic = lapply(M3, AIC), lapply(M3, function(x)summary(x)$coe[2,3]))# i am not sure what position is coe [3,4]
sM3[, aic := unlist(aic)]
sM3[, V3 := unlist(V3)]
sM3[,abs := abs(aic)]


M4 = list()
candi = candi[candi!='WSavg_value'] # eliminate already chosen predictor

nn = length(candi)
new_candi = candi[6:nn]
for (i in 1:length(new_candi)){
  print(i)
  M4[[i]] = glm(dta$event ~ dta$SWE_value + dta$SLd_value +dta$WSavg_value + dta[[new_candi[i]]] )
}

sM4 = data.table(new_candi, aic = lapply(M4, AIC), lapply(M4, function(x)summary(x)$coe[2,3]))
sM4[, aic := unlist(aic)]
sM4[, V3 := unlist(V3)]
sM4[,abs := abs(aic)]

M5 = list()

candi = candi[candi!='Tair_value'] #eliminate already chosen predictor
#Rain_Ta_value
nn = length(candi)
new_candi = candi[6:nn]
for (i in 1:length(new_candi)){
  print(i)
  M5[[i]] = glm(dta$event ~  dta$SWE_value + dta$SLd_value +dta$WSavg_value + dta$Tair_value + dta[[new_candi[i]]] )
}

sM5 = data.table(new_candi, aic = lapply(M5, AIC), lapply(M5, function(x)summary(x)$coe[2,3]))
sM5[, aic := unlist(aic)]
sM5[, V3 := unlist(V3)]
sM5[,abs := abs(aic)]

M6 = list()
candi = candi[candi!='WSavg_value3'] #eliminate already chosen predictor

#Rain_Ta_value5
nn = length(candi)
new_candi = candi[6:nn]
for (i in 1:length(new_candi)){
  print(i)
  M6[[i]] = glm(dta$event ~ dta$SWE_value + dta$SLd_value +dta$WSavg_value + dta$Tair_value  +dta$WSavg_value3 + dta[[new_candi[i]]] )
}
# best predictors for LOWEST AIC right now 

sM6 = data.table(new_candi, aic = lapply(M6, AIC), lapply(M6, function(x)summary(x)$coe[2,3]))
sM6[, aic := unlist(aic)]
sM6[, V3 := unlist(V3)]
sM6[,abs := abs(aic)]
#Rain_Ta_value3
#	P_value

g1 <- glm(formula = event ~ SWE_value + SLd_value +WSavg_value+ Rain_Ta_value + Rain_Ta_value5, data =glm_data_lbou_W_d, family = binomial)
summary (g1)
g1a <- glm(formula = event ~ SWE_value + SLd_value +WSavg_value+ H_value + Tair_value5, data =glm_data_lbou_W_d, family = binomial)
summary (g1a)
g1b <- glm(formula = event ~ SWE_value + SLd_value +WSavg_value + Tair_value + WSavg_value3 + P_value, data =glm_data_lbou_W_d, family = binomial)
summary (g1a)
summary (g1b)
install.packages("caret")
library (caret)
require(graphics)
p1<- predict(g1, newdata = NULL, type="response", se.fit = FALSE, na.action = na.pass)

lbou_w_d <- varImp(g1b, scale = TRUE)

tempVar <- rownames(lbou_w_d)

lbou_w_d <- data.table (lbou_w_d)
lbou_w_d$variable <- tempVar
lbou_w_d[variable == "P_value3", variable:= "P"]
lbou_w_d[variable == "Tair_value", variable:= "Tair"]
lbou_w_d[variable == "WSavg_value", variable:= "WSavg"]
lbou_w_d[variable == "SLd_value", variable:= "SLd"]
lbou_w_d[variable == "SWE_value", variable:= "SWE"]
lbou_w_d[, type := "lbou_w_d"]

saveRDS(lbou_w_d, "./data/varImp_W_LBOU_all.rds")
readRDS ("./data/varImp_W_LBOU_all.rds")

#cold events LBOU ALL avalanches

dta = data.table(glm_data_lbou_C_d)
candi = names(dta)[-1]
nn = length(candi)
new_candi = candi[6:nn]
M = list()
for (i in 1:length(new_candi)){
  print(i)
  M[[i]] = glm(dta$event ~ dta[[new_candi[i]]])# the  predictor
}

sM = data.table(new_candi, aic = lapply(M, AIC), lapply(M, function(x)summary(x)$coe[2,3]))
sM[, aic := unlist(aic)]
sM[, V3 := unlist(V3)]
sM[,abs := abs(aic)]

M2 = list()
candi = candi[candi!='SWE_value10']
nn = length(candi)
new_candi = candi[6:nn]
for (i in 1:length(new_candi)){
  print(i)
  M2[[i]] = glm(dta$event ~ dta$SWE_value10 + dta[[new_candi[i]]] )
}

sM2 = data.table(new_candi, aic = lapply(M2, AIC), lapply(M2, function(x)summary(x)$coe[2,3]))
sM2[, aic := unlist(aic)]
sM2[, V3 := unlist(V3)]
sM2[,abs := abs(aic)]

M3 = list()
# exclude already selected variable
candi = candi[candi!='SLd_value']

nn = length(candi)
new_candi = candi[6:nn]
for (i in 1:length(new_candi)){
  print(i)
  M3[[i]] = glm(dta$event ~ dta$SWE_value10 + dta$SLd_value + dta[[new_candi[i]]] )#building of already selected variables and others
}

sM3 = data.table(new_candi, aic = lapply(M3, AIC), lapply(M3, function(x)summary(x)$coe[2,3]))# i am not sure what position is coe [3,4]
sM3[, aic := unlist(aic)]
sM3[, V3 := unlist(V3)]
sM3[,abs := abs(aic)]


M4 = list()
candi = candi[candi!='Tair_value'] # eliminate already chosen predictor

nn = length(candi)
new_candi = candi[6:nn]
for (i in 1:length(new_candi)){
  print(i)
  M4[[i]] = glm(dta$event ~ dta$SWE_value10 + dta$SLd_value +dta$Tair_value + dta[[new_candi[i]]] )
}

sM4 = data.table(new_candi, aic = lapply(M4, AIC), lapply(M4, function(x)summary(x)$coe[2,3]))
sM4[, aic := unlist(aic)]
sM4[, V3 := unlist(V3)]
sM4[,abs := abs(aic)]

M5 = list()

candi = candi[candi!='SLd_value3'] #eliminate already chosen predictor
#Rain_Ta_value
nn = length(candi)
new_candi = candi[6:nn]
for (i in 1:length(new_candi)){
  print(i)
  M5[[i]] = glm(dta$event ~  dta$SWE_value10 + dta$SLd_value + dta$Tair_value+ dta$SLd_value3 ++ dta[[new_candi[i]]] )
}

sM5 = data.table(new_candi, aic = lapply(M5, AIC), lapply(M5, function(x)summary(x)$coe[2,3]))
sM5[, aic := unlist(aic)]
sM5[, V3 := unlist(V3)]
sM5[,abs := abs(aic)]

M6 = list()
candi = candi[candi!='SLd_value5'] #eliminate already chosen predictor

#Rain_Ta_value5
nn = length(candi)
new_candi = candi[6:nn]
for (i in 1:length(new_candi)){
  print(i)
  M6[[i]] = glm(dta$event ~ dta$SWE_value10 + dta$SLd_value + dta$Tair_value+ dta$SLd_value3  +dta$SLd_value5 + dta[[new_candi[i]]] )
}
# best predictors for LOWEST AIC right now 

sM6 = data.table(new_candi, aic = lapply(M6, AIC), lapply(M6, function(x)summary(x)$coe[2,3]))
sM6[, aic := unlist(aic)]
sM6[, V3 := unlist(V3)]
sM6[,abs := abs(aic)]
#Rain_Ta_value3
#	P_value

g1 <- glm(formula = event ~ SWE_value + SLd_value +WSavg_value+ Rain_Ta_value + Rain_Ta_value5, data =glm_data_lbou_W_d, family = binomial)
summary (g1)
g1a <- glm(formula = event ~ SWE_value + SLd_value +WSavg_value+ H_value + Tair_value5, data =glm_data_lbou_W_d, family = binomial)
summary (g1a)
g1b <- glm(formula = event ~ SWE_value + SLd_value +WSavg_value + Tair_value + WSavg_value3 + P_value, data =glm_data_lbou_W_d, family = binomial)
summary (g1a)
summary (g1b)
install.packages("caret")
library (caret)
require(graphics)
p1<- predict(g1, newdata = NULL, type="response", se.fit = FALSE, na.action = na.pass)

lbou_w_d <- varImp(g1b, scale = TRUE)

tempVar <- rownames(lbou_w_d)

lbou_w_d <- data.table (lbou_w_d)
lbou_w_d$variable <- tempVar
lbou_w_d[variable == "P_value3", variable:= "P"]
lbou_w_d[variable == "Tair_value", variable:= "Tair"]
lbou_w_d[variable == "WSavg_value", variable:= "WSavg"]
lbou_w_d[variable == "SLd_value", variable:= "SLd"]
lbou_w_d[variable == "SWE_value", variable:= "SWE"]
lbou_w_d[, type := "lbou_w_d"]

saveRDS(lbou_w_d, "./data/varImp_W_LBOU_all.rds")
readRDS ("./data/varImp_W_LBOU_all.rds")
