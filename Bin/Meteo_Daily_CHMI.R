# analysis of the daily data provided by CHMI
# created by @Roman.Juras

library(data.table)
library(ggplot2)
library(ggpubr)

data.dir = 'd:/CESNET/Data_snih/Laviny_Krkonose/Avalanche_analysis/Data/'

#read the script dealing with older data from the Avalanche project (2013-2015)
source('2_dostupnost_dat_grafy.R')  # need SCE.Krk_All, script needs some dubugging, rather run each line individualy

# Reading all variables into one data.table per station

# data PEC p. Snezkou
data.list_Pec <- dir(paste0(data.dir, 'H1PECS01'))
data.list_Pec <- data.list_Pec[-2] # remove Fmax because of different columns

meteo_column_names <- c('Y', 'M', 'D', 'Value', 'Sign', 'Variable')

skip.list <- c(25, 24, 25, 30, 29, 30, 23, 25, 25, 25) # how many rows will be skipped for each file (this varies for each Station/variable)
variable.list <- c('F', 'H', 'P', 'SCE', 'SNO', 'SRA', 'SSV', 'T', 'TMA', 'TMI')
length(skip.list)

# reading daily data - downloaded from CHMI web database
#######
# data PEC p. Snezkou
DATA.Pec <- list()
for (i in 1:length(data.list_Pec)) {
 print(paste0('i= ', i))
  data.Pec <-  fread(paste0(data.dir, 'H1PECS01/', data.list_Pec[i]), sep = ';', 
                    header = T, skip = skip.list[i], na.strings = '', stringsAsFactors = F, dec = ',') 
 data.Pec[, variable := variable.list[i]]
 names(data.Pec) <- meteo_column_names
 data.Pec[,Date := as.Date(do.call(paste, c(.SD, sep = '-')), format = '%Y-%m-%d'), .SDcols = c('Y', 'M', 'D')]
 data.Pec[, c('Y', 'M', 'D') := NULL]
 setcolorder(data.Pec, c('Date', 'Value', 'Sign', 'Variable'))
 
 DATA.Pec[[i]] <- data.Pec 

}

data.Pec <- do.call(rbind, DATA.Pec) # merge list structure to the data.table structure
data.Pec[,  Station :=  'H1PECS01']

ggplot(data = data.Pec[!is.na(Value) & !is.na(Date)], aes(x = year(Date), col = Value)) + 
  geom_bar(stat="count", position = 'dodge', fill = 'red') +
  #facet_wrap(~month(date)) +
  facet_wrap('Variable') +
  ggtitle("Data availability from Pec p. Snezkou") +
  labs(y = 'Days available')

saveRDS(data.Pec, file = paste0(data.dir,'processed_data/data.Pec_1961-2020.RDS'))
ggsave('d:/CESNET/Data_snih/Laviny_Krkonose/Avalanche_analysis/Results/data_avalaibility_Pec.png', width = 14, height = 6, units = "in")


# Vitkovice

data.list_Vit <- dir(paste0(data.dir, 'H1VITK01'))

skip.list <- c(20, 27, 26, 24, 20, 20, 20, 20) # how many rows will be skipped for each file (this varies for each Station/variable)
variable.list <- c('F', 'SCE', 'SNO', 'SRA', 'SSV', 'T', 'TMA', 'TMI')
length(skip.list)

DATA.Vit <- list()
for (i in 1:length(data.list_Vit)) {
  print(paste0('i= ', i))
  data.Vit <-  fread(paste0(data.dir, 'H1VITK01/', data.list_Vit[i]), sep = ';', 
                     header = T, skip = skip.list[i], na.strings = '', stringsAsFactors = F, dec = ',') 
  data.Vit[, Variable := variable.list[i]]
  names(data.Vit) <- meteo_column_names
  data.Vit[,Date := as.Date(do.call(paste, c(.SD, sep = '-')), format = '%Y-%m-%d'), .SDcols = c('Y', 'M', 'D')]
  data.Vit[, c('Y', 'M', 'D') := NULL]
  setcolorder(data.Vit, c('Date', 'Value', 'Sign', 'Variable'))
  
  DATA.Vit[[i]] <- data.Vit 
  
}

data.Vit <- do.call(rbind, DATA.Vit) # merge list structuru to the data.table structure
data.Vit[,  Station :=  'H1VITK01']

######

# Test plot - data avalability
ggplot(data = data.Vit[!is.na(Value) & !is.na(Date)], aes(x = year(Date), col = Value)) + 
  geom_bar(stat="count", position = 'dodge', fill = 'blue') +
  #facet_wrap(~month(date)) +
  facet_wrap('Variable') +
  ggtitle("Data availability from Vitkovice") +
  labs(y = 'Days available')

saveRDS(data.Vit, file = paste0(data.dir,'processed_data/data.Vit_1961-2020.RDS'))
ggsave('d:/CESNET/Data_snih/Laviny_Krkonose/Avalanche_analysis/Results/data_avalaibility_Vit.png', width = 14, height = 6, units = "in")

#
# Harrachov
# data Harrachov
data.list_Har <- dir(paste0(data.dir, 'P2HARR01'))
data.list_Har <- data.list_Har[-2] # remove Fmax because of different columns

skip.list <- c(24, 24, 31, 30, 27, 23, 24, 24, 24) # how many rows will be skipped for each file (this varies for each Station/variable)
variable.list <- c('F', 'H', 'SCE', 'SNO', 'SRA', 'SSV', 'T', 'TMA', 'TMI')
length(skip.list)

DATA.Har <- list()
for (i in 1:length(data.list_Har)) {
  print(paste0('i= ', i))
  data.Har <-  fread(paste0(data.dir, 'P2HARR01/', data.list_Har[i]), sep = ';', 
                     header = T, skip = skip.list[i], na.strings = '', stringsAsFactors = F, dec = ',') 
  data.Har[, variable := variable.list[i]]
  names(data.Har) <- meteo_column_names
  data.Har[,Date := as.Date(do.call(paste, c(.SD, sep = '-')), format = '%Y-%m-%d'), .SDcols = c('Y', 'M', 'D')]
  data.Har[, c('Y', 'M', 'D') := NULL]
  setcolorder(data.Har, c('Date', 'Value', 'Sign', 'Variable'))
  
  DATA.Har[[i]] <- data.Har 
  
}

data.Har <- do.call(rbind, DATA.Har) # merge list structuru to the data.table structure
data.Har[,  Station :=  'P2HARR01']

ggplot(data = data.Har[!is.na(Value) & !is.na(Date)], aes(x = year(Date), col = Value)) + 
  geom_bar(stat="count", position = 'dodge', fill = 'green') +
  #facet_wrap(~month(date)) +
  facet_wrap('Variable') +
  ggtitle("Data availability from Harrachov") +
  labs(y = 'Days available')

saveRDS(data.Har, file = paste0(data.dir,'processed_data/data.Har_1961-2020.RDS'))
ggsave('d:/CESNET/Data_snih/Laviny_Krkonose/Avalanche_analysis/Results/data_avalaibility_Har.png', width = 14, height = 6, units = "in")

# Labska bouda
data.list_Lab <- dir(paste0(data.dir, 'H1LBOU01'))
data.list_Lab <- data.list_Lab[-2] # remove Fmax because of different columns

skip.list <- c(23, 22, 29, 28, 26, 22, 22) # how many rows will be skipped for each file (this varies for each Station/variable)
variable.list <- c('F', 'H', 'SCE', 'SNO', 'SRA', 'SSV', 'T')
length(skip.list)

DATA.Lab <- list()
for (i in 1:length(data.list_Lab)) {
  print(paste0('i= ', i))
  data.Lab <-  fread(paste0(data.dir, 'H1LBOU01/', data.list_Lab[i]), sep = ';', 
                     header = T, skip = skip.list[i], na.strings = '', stringsAsFactors = F, dec = ',') 
  data.Lab[, variable := variable.list[i]]
  names(data.Lab) <- meteo_column_names
  data.Lab[,Date := as.Date(do.call(paste, c(.SD, sep = '-')), format = '%Y-%m-%d'), .SDcols = c('Y', 'M', 'D')]
  data.Lab[, c('Y', 'M', 'D') := NULL]
  setcolorder(data.Lab, c('Date', 'Value', 'Sign', 'Variable'))
  
  DATA.Lab[[i]] <- data.Lab 
  
}

data.Lab <- do.call(rbind, DATA.Lab) # merge list structure to the data.table structure
data.Lab[,  Station :=  'H1LBOU01']

# data.Lab contains Snow data only from 1979, but we have also some older data available, which wil be read bellow 
# Merging our old data (from the project) with CHMI web data
# Merging snow data from Labska

LBOU_old <- SCE.Krk_all[nazev == 'H1LBOU01']
setnames(LBOU_old, 'datum', 'Date')
LBOU_old <- LBOU_old[!is.na(Date)]

LBOU_ALL <- merge(LBOU_old, data.Lab[Variable == 'SCE'], by = 'Date', all = T)
LBOU_ALL[, SCE_correct := ifelse(is.na(Value), SCE, Value)]
LBOU_ALL[, Station := ifelse(is.na(nazev), Station, nazev)]
LBOU_ALL[, Variable := "SCE"]

LBOU_ALL[, c('SCE', 'nazev', 'Value') := NULL]

setnames(LBOU_ALL, 'SCE_correct', 'Value')

setcolorder(LBOU_ALL, c("Date", "Value", "Sign", "Variable", "Station"))

# new snow
LBOU_old_SNO <- SNO.Krk_all[nazev == 'H1LBOU01']
setnames(LBOU_old_SNO, c('datum.SNO','nazev.SNO'), c('Date', 'nazev'))
LBOU_old_SNO <- LBOU_old_SNO[!is.na(Date)]
LBOU_old_SNO[, Variable := "SNO"]
LBOU_old_SNO[, Sign := NA]
setnames(LBOU_old_SNO, c('SNO', 'nazev'), c('Value', 'Station'))
setcolorder(LBOU_old_SNO, c("Date", "Value", "Sign", "Variable", "Station"))

LBOU_SNO_all <- merge(LBOU_old_SNO, data.Lab[Variable == 'SNO'], by = 'Date', all = T)
LBOU_SNO_all[, SNO_correct := ifelse(is.na(Value.x), Value.y, Value.x)]
LBOU_SNO_all[, Station := ifelse(is.na(Station.x), Station.y, Station.x)]
LBOU_SNO_all[, Variable.x := "SNO"]
LBOU_SNO_all[, c('Value.x', 'Station.x','Value.y', 'Variable.y', 'Station.y', 'Sign.x') := NULL]
setnames(LBOU_SNO_all, c('Variable.x', 'Sign.y', 'SNO_correct'), c('Variable', 'Sign', 'Value'))
setcolorder(LBOU_SNO_all, c("Date", "Value", "Sign", "Variable", "Station"))

data.Lab[Variable == 'SNO' | Variable == 'SCE']

# Delete all rows containing variable SCE or SNO to make place for the further rbinding with longer data
data.Lab_II <- data.Lab[!(Variable == 'SCE' | Variable == 'SNO')]
data.Lab_II <- rbind(data.Lab_II, LBOU_ALL, LBOU_SNO_all)

###

ggplot(data = data.Lab_II[!is.na(Value) & !is.na(Date)], aes(x = year(Date), col = Value)) + 
  geom_bar(stat="count", position = 'dodge', fill = 'violet') +
  #facet_wrap(~month(date)) +
  facet_wrap('Variable') +
  ggtitle("Data availability from Labska bouda") +
  labs(y = 'Days available')

saveRDS(data.Lab_II, file = paste0(data.dir,'processed_data/data.Lab_1961-2020.RDS'))
ggsave('d:/CESNET/Data_snih/Laviny_Krkonose/Avalanche_analysis/Results/data_avalaibility_Lab.png', width = 14, height = 6, units = "in", dpi = 300)

# Lucni bouda
#############
data.list_Luc <- dir(paste0(data.dir, 'H1LUCB01'))
data.list_Luc <- data.list_Luc[-2] # remove Fmax because of different columns

skip.list <- c(22, 21, 29, 28, 26, 21, 21) # how many rows will be skipped for each file (this varies for each Station/variable)
variable.list <- c('F', 'H', 'SCE', 'SNO', 'SRA', 'SSV', 'T')
length(skip.list)

DATA.Luc <- list()
for (i in 1:length(data.list_Luc)) {
  print(paste0('i= ', i))
  data.Luc <-  fread(paste0(data.dir, 'H1LUCB01/', data.list_Luc[i]), sep = ';', 
                     header = T, skip = skip.list[i], na.strings = '', stringsAsFactors = F, dec = ',') 
  data.Luc[, variable := variable.list[i]]
  names(data.Luc) <- meteo_column_names
  data.Luc[,Date := as.Date(do.call(paste, c(.SD, sep = '-')), format = '%Y-%m-%d'), .SDcols = c('Y', 'M', 'D')]
  data.Luc[, c('Y', 'M', 'D') := NULL]
  setcolorder(data.Luc, c('Date', 'Value', 'Sign', 'Variable'))
  
  DATA.Luc[[i]] <- data.Luc 
  
}

data.Luc <- do.call(rbind, DATA.Luc) # merge list structuru to the data.table structure
data.Luc[,  Station :=  'H1LUCB01']

ggplot(data = data.Luc[!is.na(Value) & !is.na(Date)], aes(x = year(Date), col = Value)) + 
  geom_bar(stat="count", position = 'dodge', fill = 'orange') +
  #facet_wrap(~month(date)) +
  facet_wrap('Variable') +
  ggtitle("Data avaiLucility from Lucni bouda") +
  labs(y = 'Days available')

saveRDS(data.Luc, file = paste0(data.dir,'processed_data/data.Luc_1961-2020.RDS'))
ggsave('d:/CESNET/Data_snih/Laviny_Krkonose/Avalanche_analysis/Results/data_avalaibility_Luc.png', width = 14, height = 6, units = "in")


## Merging all CHMI daily meteo data into one dta

data.daily_all <- rbind(data.Har,data.Lab_II, data.Luc, data.Vit, data.Pec)

write.table(data.daily_all, file = paste0(data.dir,'processed_data/data.daily_1961-2020.csv'), quote = F, row.names = F)
saveRDS(data.daily_all, file = paste0(data.dir,'processed_data/data.daily_1961-2020.RDS'))

##########
# compare if data from CHMI web and data from Laviny project - correct :)
# use variable from the file SnowDepth_AvalIndex.R
source('SnowDepth_AvaIndex.R')

Labska_compare <- merge(data.Lab[Variable == 'SCE'],  SCE.Krk_all[nazev == 'H1LBOU01'], by.x = 'Date', by.y = 'datum')

ggplot(Labska_compare, aes(x = Value, y = SCE)) + geom_point()


VITK_old <- SCE.Krk_all[nazev == 'H1VITK01']
VITK_old <- VITK_old[!is.na(Date)]
names(VITK_old) <- c('Date', 'SCE_V', 'nazev_V')

Labska_Vrbatka_compare <- merge(VITK_old, LBOU_old, by = 'Date', all = T)
Labska_Lucni_compare <- merge(data.Luc[Variable == 'SCE'], LBOU_ALL, by = 'Date')

# Merging Labska and Harrachov for the comaparison
Labska_Harr_SCE <- merge(data.Har[Variable == 'SCE'], LBOU_ALL, by = 'Date')
Labska_Harr_SNO <- merge(data.Har[Variable == 'SNO'], LBOU_old_SNO, by = 'Date')
Labska_Harr_SRA <- merge(data.Har[Variable == 'SRA'], data.Lab[Variable == 'SRA'], by = 'Date')
Labska_Harr_SSV <- merge(data.Har[Variable == 'SSV'], data.Lab[Variable == 'SSV'], by = 'Date')
Labska_Harr_T <- merge(data.Har[Variable == 'T'], data.Lab[Variable == 'T'], by = 'Date')
Labska_Harr_F <- merge(data.Har[Variable == 'F'], data.Lab[Variable == 'F'], by = 'Date')
Labska_Harr_H <- merge(data.Har[Variable == 'H'], data.Lab[Variable == 'H'], by = 'Date')


# Labska vs. Vrbatka
ggplot(Labska_Vrbatka_compare[!is.na(SCE_V) & SCE > 0], aes(x = SCE_V, y = SCE, colour = year(Date))) + 
  geom_point() + geom_smooth(se = F, method = lm ) + stat_regline_equation(label.y = 200) + 
  stat_cor(label.y = 150) +
  facet_wrap(~month(Date)) + labs(x = 'SD Vrbatka [cm]', y = 'SD Labska [cm]') + 
  ggtitle('SD comparison by months(boxes) and years(colour) - only values > 0 cm')

ggsave('d:/CESNET/Data_snih/Laviny_Krkonose/Avalanche_analysis/Results/Snow_Labska_Vrbatka.png', width = 14, height = 6, units = "in", dpi = 300)

# Labska vs. Lucni
ggplot(Labska_Lucni_compare[Value > 0 & SCE_correct > 0], aes(x = Value, y = SCE_correct, colour = year(Date))) + 
  geom_point() + geom_smooth(se = F, method = lm ) + stat_regline_equation(label.y = 250) + 
  stat_cor(label.y = 200) +
  facet_wrap(~year(Date)) + labs(x = 'SD Lucni [cm]', y = 'SD Labska [cm]') + 
  ggtitle('SD comparison by years - only values > 0 cm')

ggsave('d:/CESNET/Data_snih/Laviny_Krkonose/Avalanche_analysis/Results/Snow_Labska_Lucni_months.png', width = 14, height = 6, units = "in", dpi = 300)
ggsave('d:/CESNET/Data_snih/Laviny_Krkonose/Avalanche_analysis/Results/Snow_Labska_Lucni_all.png', width = 14, height = 6, units = "in", dpi = 300)
ggsave('d:/CESNET/Data_snih/Laviny_Krkonose/Avalanche_analysis/Results/Snow_Labska_Lucni_years.png', width = 14, height = 6, units = "in", dpi = 300)

# Labska vs. Harrachov
# SCE
ggplot(Labska_Harr_SCE[Value > 0 & SCE_correct > 0], aes(x = Value, y = SCE_correct, colour = year(Date))) + 
  geom_point() + geom_smooth(se = F, method = lm ) + stat_regline_equation(label.y = 350) + 
  stat_cor(label.y = 400) +
  #facet_wrap(~month(Date)) + labs(x = 'SD Harrachov [cm]', y = 'SD Labska [cm]') + 
  #scale_color_gradientn(colours = rainbow(5)) +
  labs(x = 'SD Harrachov [cm]', y = 'SD Labska [cm]') +
  ggtitle('SD comparison by years - only values > 0 cm')

ggsave('d:/CESNET/Data_snih/Laviny_Krkonose/Avalanche_analysis/Results/Snow_Harrachov_Labska_all.png', width = 14, height = 6, units = "in", dpi = 300)

# SNO
ggplot(Labska_Harr_SNO[Value > 0 & SNO > 0], aes(x = Value, y = SNO, colour = year(Date))) + 
  geom_point() + geom_smooth(se = F, method = lm ) + stat_regline_equation(label.y = 60) + 
  stat_cor(label.y = 70) +
  #facet_wrap(~month(Date)) + labs(x = 'SD Harrachov [cm]', y = 'SD Labska [cm]') + 
  #scale_color_gradientn(colours = rainbow(5)) +
  labs(x = 'Fresh snow Harrachov [cm]', y = 'Fresh snow Labska [cm]') +
  ggtitle('Fresh snow comparison by years - only values > 0 cm')

ggsave('d:/CESNET/Data_snih/Laviny_Krkonose/Avalanche_analysis/Results/NewSnow_Harrachov_Labska_all.png', width = 14, height = 6, units = "in", dpi = 300)

# SRA
ggplot(Labska_Harr_SRA[Value.x > 0 & Value.y > 0], aes(x = Value.x, y = Value.y, colour = year(Date))) + 
  geom_point() + geom_smooth(se = F, method = lm ) + stat_regline_equation(label.y = 150) + 
  stat_cor(label.y = 170) +
  #facet_wrap(~month(Date)) + labs(x = 'SD Harrachov [cm]', y = 'SD Labska [cm]') + 
  #scale_color_gradientn(colours = rainbow(5)) +
  labs(x = 'Precipitation Harrachov [mm]', y = 'Precipitation Labska [mm]') +
  ggtitle('Precipitation - only values > 0 mm')

ggsave('d:/CESNET/Data_snih/Laviny_Krkonose/Avalanche_analysis/Results/Precip_Harrachov_Labska_all.png', width = 14, height = 6, units = "in", dpi = 300)

# SSV
ggplot(Labska_Harr_SSV[Value.x > 0 & Value.y > 0], aes(x = Value.x, y = Value.y, colour = year(Date))) + 
  geom_point() + geom_smooth(se = F, method = lm ) + stat_regline_equation(label.y = 20) + 
  stat_cor(label.y = 22) +
  #facet_wrap(~month(Date)) + labs(x = 'SD Harrachov [cm]', y = 'SD Labska [cm]') + 
  #scale_color_gradientn(colours = rainbow(5)) +
  labs(x = 'Sunshine duration Harrachov [hours]', y = 'Sunshine duration Labska [hours]') +
  ggtitle('Sunshine duration - only values > 0 hours')

ggsave('d:/CESNET/Data_snih/Laviny_Krkonose/Avalanche_analysis/Results/Sunshine_Harrachov_Labska_all.png', width = 14, height = 6, units = "in", dpi = 300)

# T
ggplot(Labska_Harr_T, aes(x = Value.x, y = Value.y, colour = year(Date))) + 
  geom_point() + geom_smooth(se = F, method = lm ) + stat_regline_equation(label.y = 30) + 
  stat_cor(label.y = 32) +
  #facet_wrap(~month(Date)) + labs(x = 'SD Harrachov [cm]', y = 'SD Labska [cm]') + 
  #scale_color_gradientn(colours = rainbow(5)) +
  labs(x = 'Temperature Harrachov [?C]', y = 'Temperature Labska [?C]') +
  ggtitle('Air temperature ')

ggsave('d:/CESNET/Data_snih/Laviny_Krkonose/Avalanche_analysis/Results/Temperature_Harrachov_Labska_all.png', width = 14, height = 6, units = "in", dpi = 300)


# F
ggplot(Labska_Harr_F, aes(x = Value.x, y = Value.y, colour = year(Date))) + 
  geom_point() + geom_smooth(se = F, method = lm ) + stat_regline_equation(label.y = 30) + 
  stat_cor(label.y = 32) +
  #facet_wrap(~month(Date)) + labs(x = 'SD Harrachov [cm]', y = 'SD Labska [cm]') + 
  #scale_color_gradientn(colours = rainbow(5)) +
  labs(x = 'Wind speed Harrachov [m/s]', y = 'Wind speed Labska [m/s]') +
  ggtitle('Wind speed ')

ggsave('d:/CESNET/Data_snih/Laviny_Krkonose/Avalanche_analysis/Results/WindSpeed_Harrachov_Labska_all.png', width = 14, height = 6, units = "in", dpi = 300)

# H
ggplot(Labska_Harr_H, aes(x = Value.x, y = Value.y, colour = month(Date))) + 
  geom_point() + geom_smooth(se = T, method = 'lm') + stat_regline_equation(label.y = 100) + 
  stat_cor(label.y = 110) +
  #facet_wrap(~year(Date)) + labs(x = 'SD Harrachov [cm]', y = 'SD Labska [cm]') + 
  #scale_color_gradientn(colours = rainbow(5)) +
  labs(x = 'Humidity Harrachov [%]', y = 'Humidity Labska [%]') +
  ggtitle('Relative air humidity ')

ggsave('d:/CESNET/Data_snih/Laviny_Krkonose/Avalanche_analysis/Results/Humidity_Harrachov_Labska_all.png', width = 14, height = 6, units = "in", dpi = 300)

##########




# Correlation among stations
library(corrplot)
library(reshape2)
library(PerformanceAnalytics)

## SCE ##
#########

SCE_4Corr <- merge(Labska_Lucni_compare, VITK_old, by = 'Date', all = F)
#SCE_4Corr <- merge(Labska_Lucni_compare, data.Har[variable == 'SCE'], by = 'Date', all = F)
SCE_4Corr <- merge(SCE_4Corr, data.Har[variable == 'SCE'], by = 'Date', all = F)
SCE_4Corr <- merge(SCE_4Corr, data.Pec[variable == 'SCE'], by = 'Date', all = F)

SCE_4Corr <- SCE_4Corr[, c('Value.x', 'SCE_correct', 'SCE_V','Value.y', 'Value')]
names_SCE_statins <- c('H1LUCB01', 'H1LBOU01', 'H1VITK01','P2HARR01', 'H1PEC01') 
names(SCE_4Corr) <- names_SCE_statins

# create a correlation matrix
SCE_cor.mat <- cor(SCE_4Corr, use = 'complete.obs', method = 'spearman')

corrplot(SCE_cor.mat, type = 'upper', order = 'hclust', method = 'number', 
         title = "Snow depth correlation (Spearman) among stations", mar=c(0,0,1,0))

## SRA ##
#########

data.Pec[variable == 'SRA']
data.Har[variable == 'SRA']


SRA_4corr <- merge(data.Luc[variable == 'SRA'],data.Lab[variable == 'SRA'],  by = 'Date', all = T)
SRA_4corr <- merge(SRA_4corr, data.Vit[variable == 'SRA'], by = 'Date', all = T)
SRA_4corr <- merge(SRA_4corr, data.Har[variable == 'SRA'], by = 'Date', all = T)
SRA_4corr <- SRA_4corr[, c('Date', 'Value.x', 'Value.y', 'Value.x', 'Value.y')]
names(SRA_4corr) <- c('Date',names_SCE_statins[1:4])
SRA_4corr <- merge(SRA_4corr, data.Pec[variable == 'SRA'], by = 'Date', all = T)
SRA_4corr <- SRA_4corr[,2:6]
names(SRA_4corr) <- names_SCE_statins

SRA_cor.mat <- cor(SRA_4corr, use = 'complete.obs', method = 'spearman')
corrplot(SRA_cor.mat, type = 'upper', order = 'hclust', method = 'number', 
         title = "Precipitation correlation (Spearman) among stations", mar=c(0,0,1,0))

## SSV ##
#########

SSV_4corr <- merge(data.Luc[variable == 'SSV'],data.Lab[variable == 'SSV'],  by = 'Date', all = T)
SSV_4corr <- merge(SSV_4corr, data.Vit[variable == 'SSV'], by = 'Date', all = T)
SSV_4corr <- merge(SSV_4corr, data.Har[variable == 'SSV'], by = 'Date', all = T)
SSV_4corr <- SSV_4corr[, c('Date', 'Value.x', 'Value.y', 'Value.x', 'Value.y')]
names(SSV_4corr) <- c('Date',names_SCE_statins[1:4])
SSV_4corr <- merge(SSV_4corr, data.Pec[variable == 'SSV'], by = 'Date', all = T)
SSV_4corr <- SSV_4corr[,2:6]
names(SSV_4corr) <- names_SCE_statins

SSV_cor.mat <- cor(SSV_4corr, use = 'complete.obs', method = 'spearman')
corrplot(SSV_cor.mat, type = 'upper', order = 'hclust', method = 'number', 
         title = "Sunshine duration correlation (Spearman) among stations", mar=c(0,0,1,0))

## H ##
#########

H_4corr <- merge(data.Luc[Variable == 'H'],data.Lab[Variable == 'H'],  by = 'Date', all = T)
H_4corr <- merge(H_4corr, data.Vit[Variable == 'H'], by = 'Date', all = T)
H_4corr <- merge(H_4corr, data.Har[Variable == 'H'], by = 'Date', all = T)
H_4corr <- H_4corr[, c('Date', 'Value.x', 'Value.y', 'Value.x', 'Value.y')]
names(H_4corr) <- c('Date',names_SCE_statins[1:4])
H_4corr <- merge(H_4corr, data.Pec[Variable == 'H'], by = 'Date', all = T)
H_4corr <- H_4corr[,2:6]
names(H_4corr) <- names_SCE_statins

H_cor.mat <- cor(H_4corr, use = 'complete.obs', method = 'spearman')
corrplot(H_cor.mat, type = 'upper', order = 'hclust', method = 'number', 
         title = "Relative humidity correlation (Spearman) among stations", mar=c(0,0,1,0))

## T ##
#########

T_4corr <- merge(data.Luc[variable == 'T'],data.Lab[variable == 'T'],  by = 'Date', all = T)
T_4corr <- merge(T_4corr, data.Vit[variable == 'T'], by = 'Date', all = T)
T_4corr <- merge(T_4corr, data.Har[variable == 'T'], by = 'Date', all = T)
T_4corr <- T_4corr[, c('Date', 'Value.x', 'Value.y', 'Value.x', 'Value.y')]
names(T_4corr) <- c('Date',names_SCE_statins[1:4])
T_4corr <- merge(T_4corr, data.Pec[variable == 'T'], by = 'Date', all = T)
T_4corr <- T_4corr[,2:6]
names(T_4corr) <- names_SCE_statins

T_cor.mat <- cor(T_4corr, use = 'complete.obs', method = 'spearman')
T_corr <- corrplot(T_cor.mat, type = 'upper', order = 'hclust', method = 'number', 
         title = "Temperature correlation (Spearman) among stations", mar=c(0,0,1,0))

## F ##
#########

F_4corr <- merge(data.Luc[variable == 'F'],data.Lab[variable == 'F'],  by = 'Date', all = T)
F_4corr <- merge(F_4corr, data.Vit[variable == 'F'], by = 'Date', all = T)
F_4corr <- merge(F_4corr, data.Har[variable == 'F'], by = 'Date', all = T)
F_4corr <- F_4corr[, c('Date', 'Value.x', 'Value.y', 'Value.x', 'Value.y')]
names(F_4corr) <- c('Date',names_SCE_statins[1:4])
F_4corr <- merge(F_4corr, data.Pec[variable == 'F'], by = 'Date', all = T)
F_4corr <- F_4corr[,2:6]
names(F_4corr) <- names_SCE_statins

F_cor.mat <- cor(F_4corr, use = 'complete.obs', method = 'spearman')
Fcorr <- corrplot(F_cor.mat, type = 'upper', order = 'hclust', method = 'number', 
         title = "Wind speed correlation (Spearman) among stations", mar=c(0,0,1,0))


