library(data.table)
library(readr)
library(ggplot2)
library(lubridate)
library(writexl)
library(dplyr)  
library(zoo)
setwd ("C:/Users/marketa.souckova/OneDrive - CZU v Praze/R/Krkonose/Rcode/RDS/")
aval_melt <- readRDS(file = "./lbou_lucb_data_new.rds")
aval_melt <- readRDS(file = "./my_data/lbou_lucb_data_new.rds")
aval_melt <- lbou_lucb_data_new

#creating min, max for T and Fmax
####### MIN MAX pro T FMAX ###############
fmax_t <- list()
hours <- c(24,48,72,96,120,144)
for (i in c(1:length(hours))){
  a <- aval_melt[PLOT %between% c(1, hours[i]) & var %in% c("T", "Fmax") & variable == "value", {min = min(value, na.rm = T); max = max(value, na.rm = T); T0 = value[1]; list(min = min, max = max, T0 = T0)}, by = .(ID, event, var, CAW)]
  a$H <- hours[i]
  fmax_t[[i]] <- a
  print(hours[i])
}
fmax_t <- rbindlist(fmax_t)

# graph article
########### SUM pro PRECIP ###########

precip <- list()
hours <- c(24,48,72,96,120,144)
for (i in c(1:length(hours))){
  a <- aval_melt[PLOT %between% c(1, hours[i]) & var %in% c("SRA1H") & variable == "value", {sum = sum(value, na.rm = T); T0 = value[1] ; list(sum = sum, T0 = T0)}, by = .(ID, event, var, CAW)]
  a$H <- hours[i]
  precip[[i]] <- a
  print(hours[i])
}
precip <- rbindlist(precip)

############ AVG for the rest of the variables ###########

avg_rest <- list()
hours <- c(24,48,72,96,120,144)
for (i in c(1:length(hours))){
  a <- aval_melt[PLOT %between% c(1, hours[i]) & !var %in% c("SRA1H", "Fprum", "Fmax", "Tdiff", "SNO") & variable == "value", {avg = mean(value, na.rm = T); T0 = value[1] ; list(avg = avg, T0 = T0)}, by = .(ID, event, var, CAW)]
  a$H <- hours[i]
  avg_rest[[i]] <- a
  print(hours[i])
}

avg_rest <- rbindlist(avg_rest)

# creating deltas of temperatures - important variable, it's just one values how to merge it with other parametres from aval_melt (rolling means of meteo and snow variables)
########### DELTA T ######################

delta_t <- fmax_t[var == "T", ]

delta_t[,delta_min:= T0 - min]
delta_t[,delta_max:= T0 - max]

########### DELTA SCE ####################

delta_sce <- list()
hours <- c(24,48,72,96,120,144)
for (i in c(1:length(hours))){
  a <- aval_melt[PLOT %between% c(1, hours[i]) & var %in% c("SCE") & variable == "value", {max_sce = max(value, na.rm = T); T0 = value[1] ; list(max_sce = max_sce, T0 = T0)}, by = .(ID, event, var, CAW)]
  a$H <- hours[i]
  delta_sce[[i]] <- a
  print(hours[i])
}

delta_sce <- rbindlist(delta_sce)

delta_sce[,delta_sce:=max_sce - T0]

############ FIVE DAY SUM of SNOW ##########################

sno_sum_5d <- aval_melt[PLOT %between% c(1, 120) & var %in% c("SNO") & variable == "value", .(sum = sum(unique(value), na.rm = T)), by = .(ID, event, var, CAW)]

################# MERGE ###################################

#fmin <- dcast(fmax_t[var == "Fmax", .(ID, event, min,H,CAW, var)], ID+event~var+H+CAW, value.var = "min")
#tmin <- dcast(fmax_t[var == "T", .(ID, event, min,H, CAW, var)], ID+event~var+H+CAW, value.var = "min")

fmax <- dcast(fmax_t[var == "Fmax", .(ID, event, max,H, var)], ID+event~var+H+CAW, value.var = "max")
tmax <- dcast(fmax_t[var == "T", .(ID, event, max,H, var)], ID+event~var+H+CAW, value.var = "max")

#delta_T_max <- dcast(delta_t[var == "T", .(ID, event, max,H, var)], ID+event~var+H, value.var = "max")

model_dta_T_Fmin <- merge(x = fmin, y = tmin, by = c("ID", "event"))
model_dta_T_Fmax <- merge(x = fmax, y = tmax, by = c("ID", "event"))
#model_delta_T_SCE <- merge (x= delta_sce, y = delta_t, by = c("ID", "event"))
###########################################################
############################################################

precip[,delta:= T0 - sum]

avg_rest[,delta:= T0 - avg]


#boxplots for selected variables

WS_Tmin <- ggplot(fmax_t[!is.na(CAW),])+
  geom_boxplot(aes(x = event, y = min, group = event, fill = as.factor(event)))+
  facet_grid(var+CAW~H)+
  labs(fill = "Event")

WS_Tmax <-ggplot(fmax_t[!is.na(CAW),])+
  geom_boxplot(aes(x = event, y = max, group = event, fill = as.factor(event)))+
  facet_grid(var+CAW~H)+
  labs(fill = "event")

#Fmin <-ggplot(fmin)+
  geom_boxplot(aes(x = event, y = min, group = event, fill = as.factor(event)))+
  facet_grid(var+CAW~H)+
  labs(fill = "Event")
  
NSS <-ggplot(sno_sum_5d)+
    geom_boxplot(aes(x = event, y = sum, group = event, fill = as.factor(event)))+
    facet_grid(var+CAW~sum)+
    labs(fill = "event")

P <-ggplot(precip[!is.na(CAW),])+
  geom_boxplot(aes(x = event, y = sum, group = event, fill = as.factor(event)))+
  facet_grid(var+CAW~H)+
  labs(fill = "Event")

SWE_H_WD_SLd_T <- ggplot(avg_rest[!is.na(CAW),])+
  geom_boxplot(aes(x = event, y = avg, group = event, fill = as.factor(event)))+
  facet_grid(var+CAW~H, scales = "free_y")+
  labs(fill = "event")

#################### ? SNOW DRIFT ? ######think how to set it up
#snow_drift <- list()
#hours <- c(24,48,72,96,120,144)
#for (i in c(1:length(hours))){
#a <- aval_melt[PLOT %between% c(1, hours[i]) & var %in% c("SRA1H") & variable == "value", {sum_p = sum(value, na.rm = T); list(sum_p = sum_p)}, by = .(ID, event)]
# b <- aval_melt[PLOT %between% c(1, hours[i]) & var %in% c("Fprum") & variable == "value", {mean_f = mean(value, na.rm = T); list(mean_f = mean_f)}, by = .(ID, event)]
# a$H <- hours[i]
# b$H <- hours[i]
# c <- merge(x = a, y = b, by = c("ID", "event", "H"))
# c$f4 <- (c$mean_f)^4  ##########  opravdu ????
# avg_rest[[i]] <- a
# print(hours[i])
#}

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


