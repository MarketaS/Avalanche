# analysis of the daily data provided by CHMI
# created by @Roman.Juras

library(data.table)
library(ggplot2)

data.dir = 'd:/CESNET/Data_snih/Laviny_Krkonose/Avalanche_analysis/Data/'

# Reading all variables into one data.table per station

# data PEC p. Snezkou
data.list_Pec <- dir(paste0(data.dir, 'H1PECS01'))
data.list_Pec <- data.list_Pec[-2] # remove Fmax because of different columns


skip.list <- c(25, 24, 25, 30, 29, 30, 23, 25, 25, 25) # how many rows will be skipped for each file (this varies for each Station/variable)
variable.list <- c('F', 'H', 'P', 'SCE', 'SNO', 'SRA', 'SSV', 'T', 'TMA', 'TMI')
length(skip.list)

# reading daily data - downloaded from CHMI web database
#######
DATA.Pec <- list()
for (i in 1:length(data.list_Pec)) {
 print(paste0('i= ', i))
  data.Pec <-  fread(paste0(data.dir, 'H1PECS01/', data.list_Pec[i]), sep = ';', 
                    header = T, skip = skip.list[i], na.strings = '', stringsAsFactors = F, dec = ',') 
 data.Pec[, variable := variable.list[i]]
 data.Pec[,datum := as.Date(do.call(paste, c(.SD, sep = '-')), format = '%Y-%m-%d'), .SDcols = c('Rok','Mìsíc','Den')]
 data.Pec[, c('Rok','Mìsíc','Den') := NULL]
 setcolorder(data.Pec, c('datum', 'Hodnota', 'Pøíznak', 'variable'))
 
 DATA.Pec[[i]] <- data.Pec 

}

data.Pec <- do.call(rbind, DATA.Pec) # merge list structuru to the data.table structure
data.Pec[,  station :=  'H1PECS01']

ggplot(data = data.Pec[!is.na(Hodnota) & !is.na(datum)], aes(x = year(datum), col = Hodnota)) + 
  geom_bar(stat="count", position = 'dodge', fill = 'red') +
  #facet_wrap(~month(date)) +
  facet_wrap('variable') +
  ggtitle("Data availability from Pec p. Snezkou") +
  labs(y = 'Days available')

ggsave('d:/CESNET/Data_snih/Laviny_Krkonose/Avalanche_analysis/Results/data_avalaibility_Pec.png', width = 14, height = 6, units = "in")

# Vitkovice
# data PEC p. Snezkou
data.list_Vit <- dir(paste0(data.dir, 'H1VITK01'))

skip.list <- c(20, 27, 26, 24, 20, 20, 20, 20) # how many rows will be skipped for each file (this varies for each Station/variable)
variable.list <- c('F', 'SCE', 'SNO', 'SRA', 'SSV', 'T', 'TMA', 'TMI')
length(skip.list)

DATA.Vit <- list()
for (i in 1:length(data.list_Vit)) {
  print(paste0('i= ', i))
  data.Vit <-  fread(paste0(data.dir, 'H1VITK01/', data.list_Vit[i]), sep = ';', 
                     header = T, skip = skip.list[i], na.strings = '', stringsAsFactors = F, dec = ',') 
  data.Vit[, variable := variable.list[i]]
  data.Vit[,datum := as.Date(do.call(paste, c(.SD, sep = '-')), format = '%Y-%m-%d'), .SDcols = c('Rok','Mìsíc','Den')]
  data.Vit[, c('Rok','Mìsíc','Den') := NULL]
  setcolorder(data.Vit, c('datum', 'Hodnota', 'Pøíznak', 'variable'))
  
  DATA.Vit[[i]] <- data.Vit 
  
}

data.Vit <- do.call(rbind, DATA.Vit) # merge list structuru to the data.table structure
data.Vit[,  station :=  'H1VITK01']

######

# Test plot - data avalability
ggplot(data = data.Vit[!is.na(Hodnota) & !is.na(datum)], aes(x = year(datum), col = Hodnota)) + 
  geom_bar(stat="count", position = 'dodge', fill = 'blue') +
  #facet_wrap(~month(date)) +
  facet_wrap('variable') +
  ggtitle("Data availability from Vitkovice") +
  labs(y = 'Days available')

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
  data.Har[,datum := as.Date(do.call(paste, c(.SD, sep = '-')), format = '%Y-%m-%d'), .SDcols = c('Rok','Mìsíc','Den')]
  data.Har[, c('Rok','Mìsíc','Den') := NULL]
  setcolorder(data.Har, c('datum', 'Hodnota', 'Pøíznak', 'variable'))
  
  DATA.Har[[i]] <- data.Har 
  
}

data.Har <- do.call(rbind, DATA.Har) # merge list structuru to the data.table structure
data.Har[,  station :=  'P2HARR01']

ggplot(data = data.Har[!is.na(Hodnota) & !is.na(datum)], aes(x = year(datum), col = Hodnota)) + 
  geom_bar(stat="count", position = 'dodge', fill = 'green') +
  #facet_wrap(~month(date)) +
  facet_wrap('variable') +
  ggtitle("Data availability from Harrachov") +
  labs(y = 'Days available')

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
  data.Lab[,datum := as.Date(do.call(paste, c(.SD, sep = '-')), format = '%Y-%m-%d'), .SDcols = c('Rok','Mìsíc','Den')]
  data.Lab[, c('Rok','Mìsíc','Den') := NULL]
  setcolorder(data.Lab, c('datum', 'Hodnota', 'Pøíznak', 'variable'))
  
  DATA.Lab[[i]] <- data.Lab 
  
}

data.Lab <- do.call(rbind, DATA.Lab) # merge list structure to the data.table structure
data.Lab[,  station :=  'H1LBOU01']

##########
# compare if data from CHMI web and data from Laviny project - correct :)
# use variable from the file SnowDepth_AvalIndex.R
source('SnowDepth_AvalIndex.R')

Labska_compare <- merge(data.Lab[variable == 'SCE'],  SCE.Krk_all[nazev == 'H1LBOU01'], by = 'datum')

ggplot(Labska_compare, aes(x = Hodnota, y = SCE)) + geom_point()



LBOU_old <- SCE.Krk_all[nazev == 'H1LBOU01']
LBOU_old <- LBOU_old[!is.na(datum)]

LBOU_old_SNO <- SNO.Krk_all[nazev == 'H1LBOU01']
setnames(LBOU_old_SNO, c('datum.SNO','nazev.SNO'), c('datum', 'nazev'))
LBOU_old_SNO <- LBOU_old_SNO[!is.na(datum)]

VITK_old <- SCE.Krk_all[nazev == 'H1VITK01']
VITK_old <- VITK_old[!is.na(datum)]

# Merging snow data from Labska
LBOU_ALL <- merge(LBOU_old, data.Lab[variable == 'SCE'], by = 'datum', all = T)
LBOU_ALL[, SCE_correct := ifelse(is.na(Hodnota), SCE, Hodnota)]
LBOU_ALL[, Station := ifelse(is.na(nazev), station, nazev)]

LBOU_ALL[, c('SCE', 'nazev', 'Hodnota', 'Pøíznak', 'variable', 'station') := NULL]

names(VITK_old) <- c('datum', 'SCE_V', 'nazev_V')

Labska_Vrbatka_compare <- merge(VITK_old, LBOU_old, by = 'datum', all = T)
Labska_Lucni_compare <- merge(data.Luc[variable == 'SCE'], LBOU_ALL, by = 'datum')

# Merging Labska and Harrachov for the comaparison
Labska_Harr_SCE <- merge(data.Har[variable == 'SCE'], LBOU_ALL, by = 'datum')
Labska_Harr_SNO <- merge(data.Har[variable == 'SNO'], LBOU_old_SNO, by = 'datum')
Labska_Harr_SRA <- merge(data.Har[variable == 'SRA'], data.Lab[variable == 'SRA'], by = 'datum')
Labska_Harr_SSV <- merge(data.Har[variable == 'SSV'], data.Lab[variable == 'SSV'], by = 'datum')
Labska_Harr_T <- merge(data.Har[variable == 'T'], data.Lab[variable == 'T'], by = 'datum')
Labska_Harr_F <- merge(data.Har[variable == 'F'], data.Lab[variable == 'F'], by = 'datum')
Labska_Harr_H <- merge(data.Har[variable == 'H'], data.Lab[variable == 'H'], by = 'datum')

library(ggpubr)
# Labska vs. Vrbatka
ggplot(Labska_Vrbatka_compare[!is.na(SCE_V) & SCE > 0], aes(x = SCE_V, y = SCE, colour = year(datum))) + 
  geom_point() + geom_smooth(se = F, method = lm ) + stat_regline_equation(label.y = 200) + 
  stat_cor(label.y = 150) +
  facet_wrap(~month(datum)) + labs(x = 'SD Vrbatka [cm]', y = 'SD Labska [cm]') + 
  ggtitle('SD comparison by months(boxes) and years(colour) - only values > 0 cm')

ggsave('d:/CESNET/Data_snih/Laviny_Krkonose/Avalanche_analysis/Results/Snow_Labska_Vrbatka.png', width = 14, height = 6, units = "in", dpi = 300)

# Labska vs. Lucni
ggplot(Labska_Lucni_compare[Hodnota > 0 & SCE_correct > 0], aes(x = Hodnota, y = SCE_correct, colour = year(datum))) + 
  geom_point() + geom_smooth(se = F, method = lm ) + stat_regline_equation(label.y = 250) + 
  stat_cor(label.y = 200) +
  facet_wrap(~year(datum)) + labs(x = 'SD Lucni [cm]', y = 'SD Labska [cm]') + 
  ggtitle('SD comparison by years - only values > 0 cm')

ggsave('d:/CESNET/Data_snih/Laviny_Krkonose/Avalanche_analysis/Results/Snow_Labska_Lucni_months.png', width = 14, height = 6, units = "in", dpi = 300)
ggsave('d:/CESNET/Data_snih/Laviny_Krkonose/Avalanche_analysis/Results/Snow_Labska_Lucni_all.png', width = 14, height = 6, units = "in", dpi = 300)
ggsave('d:/CESNET/Data_snih/Laviny_Krkonose/Avalanche_analysis/Results/Snow_Labska_Lucni_years.png', width = 14, height = 6, units = "in", dpi = 300)

# Labska vs. Harrachov
# SCE
ggplot(Labska_Harr_SCE[Hodnota > 0 & SCE_correct > 0], aes(x = Hodnota, y = SCE_correct, colour = year(datum))) + 
  geom_point() + geom_smooth(se = F, method = lm ) + stat_regline_equation(label.y = 350) + 
  stat_cor(label.y = 400) +
  #facet_wrap(~month(datum)) + labs(x = 'SD Harrachov [cm]', y = 'SD Labska [cm]') + 
  #scale_color_gradientn(colours = rainbow(5)) +
  labs(x = 'SD Harrachov [cm]', y = 'SD Labska [cm]') +
  ggtitle('SD comparison by years - only values > 0 cm')

ggsave('d:/CESNET/Data_snih/Laviny_Krkonose/Avalanche_analysis/Results/Snow_Harrachov_Labska_all.png', width = 14, height = 6, units = "in", dpi = 300)

# SNO
ggplot(Labska_Harr_SNO[Hodnota > 0 & SNO > 0], aes(x = Hodnota, y = SNO, colour = year(datum))) + 
  geom_point() + geom_smooth(se = F, method = lm ) + stat_regline_equation(label.y = 60) + 
  stat_cor(label.y = 70) +
  #facet_wrap(~month(datum)) + labs(x = 'SD Harrachov [cm]', y = 'SD Labska [cm]') + 
  #scale_color_gradientn(colours = rainbow(5)) +
  labs(x = 'Fresh snow Harrachov [cm]', y = 'Fresh snow Labska [cm]') +
  ggtitle('Fresh snow comparison by years - only values > 0 cm')

ggsave('d:/CESNET/Data_snih/Laviny_Krkonose/Avalanche_analysis/Results/NewSnow_Harrachov_Labska_all.png', width = 14, height = 6, units = "in", dpi = 300)

# SRA
ggplot(Labska_Harr_SRA[Hodnota.x > 0 & Hodnota.y > 0], aes(x = Hodnota.x, y = Hodnota.y, colour = year(datum))) + 
  geom_point() + geom_smooth(se = F, method = lm ) + stat_regline_equation(label.y = 150) + 
  stat_cor(label.y = 170) +
  #facet_wrap(~month(datum)) + labs(x = 'SD Harrachov [cm]', y = 'SD Labska [cm]') + 
  #scale_color_gradientn(colours = rainbow(5)) +
  labs(x = 'Precipitation Harrachov [mm]', y = 'Precipitation Labska [mm]') +
  ggtitle('Precipitation - only values > 0 mm')

ggsave('d:/CESNET/Data_snih/Laviny_Krkonose/Avalanche_analysis/Results/Precip_Harrachov_Labska_all.png', width = 14, height = 6, units = "in", dpi = 300)

# SSV
ggplot(Labska_Harr_SSV[Hodnota.x > 0 & Hodnota.y > 0], aes(x = Hodnota.x, y = Hodnota.y, colour = year(datum))) + 
  geom_point() + geom_smooth(se = F, method = lm ) + stat_regline_equation(label.y = 20) + 
  stat_cor(label.y = 22) +
  #facet_wrap(~month(datum)) + labs(x = 'SD Harrachov [cm]', y = 'SD Labska [cm]') + 
  #scale_color_gradientn(colours = rainbow(5)) +
  labs(x = 'Sunshine duration Harrachov [hours]', y = 'Sunshine duration Labska [hours]') +
  ggtitle('Sunshine duration - only values > 0 hours')

ggsave('d:/CESNET/Data_snih/Laviny_Krkonose/Avalanche_analysis/Results/Sunshine_Harrachov_Labska_all.png', width = 14, height = 6, units = "in", dpi = 300)

# T
ggplot(Labska_Harr_T, aes(x = Hodnota.x, y = Hodnota.y, colour = year(datum))) + 
  geom_point() + geom_smooth(se = F, method = lm ) + stat_regline_equation(label.y = 30) + 
  stat_cor(label.y = 32) +
  #facet_wrap(~month(datum)) + labs(x = 'SD Harrachov [cm]', y = 'SD Labska [cm]') + 
  #scale_color_gradientn(colours = rainbow(5)) +
  labs(x = 'Temperature Harrachov [°C]', y = 'Temperature Labska [°C]') +
  ggtitle('Air temperature ')

ggsave('d:/CESNET/Data_snih/Laviny_Krkonose/Avalanche_analysis/Results/Temperature_Harrachov_Labska_all.png', width = 14, height = 6, units = "in", dpi = 300)


# F
ggplot(Labska_Harr_F, aes(x = Hodnota.x, y = Hodnota.y, colour = year(datum))) + 
  geom_point() + geom_smooth(se = F, method = lm ) + stat_regline_equation(label.y = 30) + 
  stat_cor(label.y = 32) +
  #facet_wrap(~month(datum)) + labs(x = 'SD Harrachov [cm]', y = 'SD Labska [cm]') + 
  #scale_color_gradientn(colours = rainbow(5)) +
  labs(x = 'Wind speed Harrachov [m/s]', y = 'Wind speed Labska [m/s]') +
  ggtitle('Wind speed ')

ggsave('d:/CESNET/Data_snih/Laviny_Krkonose/Avalanche_analysis/Results/WindSpeed_Harrachov_Labska_all.png', width = 14, height = 6, units = "in", dpi = 300)

# H
ggplot(Labska_Harr_H, aes(x = Hodnota.x, y = Hodnota.y, colour = year(datum))) + 
  geom_point() + geom_smooth(se = F, method = lm ) + stat_regline_equation(label.y = 100) + 
  stat_cor(label.y = 110) +
  #facet_wrap(~month(datum)) + labs(x = 'SD Harrachov [cm]', y = 'SD Labska [cm]') + 
  #scale_color_gradientn(colours = rainbow(5)) +
  labs(x = 'Humidity Harrachov [%]', y = 'Humidity Labska [%]') +
  ggtitle('Relative air humidity ')

ggsave('d:/CESNET/Data_snih/Laviny_Krkonose/Avalanche_analysis/Results/Humidity_Harrachov_Labska_all.png', width = 14, height = 6, units = "in", dpi = 300)

##########

ggplot(data = data.Lab[!is.na(Hodnota) & !is.na(datum)], aes(x = year(datum), col = Hodnota)) + 
  geom_bar(stat="count", position = 'dodge', fill = 'violet') +
  #facet_wrap(~month(date)) +
  facet_wrap('variable') +
  ggtitle("Data availability from Labska bouda") +
  labs(y = 'Days available')

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
  data.Luc[,datum := as.Date(do.call(paste, c(.SD, sep = '-')), format = '%Y-%m-%d'), .SDcols = c('Rok','Mìsíc','Den')]
  data.Luc[, c('Rok','Mìsíc','Den') := NULL]
  setcolorder(data.Luc, c('datum', 'Hodnota', 'Pøíznak', 'variable'))
  
  DATA.Luc[[i]] <- data.Luc 
  
}

data.Luc <- do.call(rbind, DATA.Luc) # merge list structuru to the data.table structure
data.Luc[,  station :=  'H1LUCB01']

ggplot(data = data.Luc[!is.na(Hodnota) & !is.na(datum)], aes(x = year(datum), col = Hodnota)) + 
  geom_bar(stat="count", position = 'dodge', fill = 'orange') +
  #facet_wrap(~month(date)) +
  facet_wrap('variable') +
  ggtitle("Data avaiLucility from Lucni bouda") +
  labs(y = 'Days available')

ggsave('d:/CESNET/Data_snih/Laviny_Krkonose/Avalanche_analysis/Results/data_avalaibility_Luc.png', width = 14, height = 6, units = "in")


# Correlation among stations
library(corrplot)
library(reshape2)
library(PerformanceAnalytics)

## SCE ##
#########

SCE_4Corr <- merge(Labska_Lucni_compare, VITK_old, by = 'datum', all = F)
#SCE_4Corr <- merge(Labska_Lucni_compare, data.Har[variable == 'SCE'], by = 'datum', all = F)
SCE_4Corr <- merge(SCE_4Corr, data.Har[variable == 'SCE'], by = 'datum', all = F)
SCE_4Corr <- merge(SCE_4Corr, data.Pec[variable == 'SCE'], by = 'datum', all = F)

SCE_4Corr <- SCE_4Corr[, c('Hodnota.x', 'SCE_correct', 'SCE_V','Hodnota.y', 'Hodnota')]
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


SRA_4corr <- merge(data.Luc[variable == 'SRA'],data.Lab[variable == 'SRA'],  by = 'datum', all = T)
SRA_4corr <- merge(SRA_4corr, data.Vit[variable == 'SRA'], by = 'datum', all = T)
SRA_4corr <- merge(SRA_4corr, data.Har[variable == 'SRA'], by = 'datum', all = T)
SRA_4corr <- SRA_4corr[, c('datum', 'Hodnota.x', 'Hodnota.y', 'Hodnota.x', 'Hodnota.y')]
names(SRA_4corr) <- c('datum',names_SCE_statins[1:4])
SRA_4corr <- merge(SRA_4corr, data.Pec[variable == 'SRA'], by = 'datum', all = T)
SRA_4corr <- SRA_4corr[,2:6]
names(SRA_4corr) <- names_SCE_statins

SRA_cor.mat <- cor(SRA_4corr, use = 'complete.obs', method = 'spearman')
corrplot(SRA_cor.mat, type = 'upper', order = 'hclust', method = 'number', 
         title = "Precipitation correlation (Spearman) among stations", mar=c(0,0,1,0))

## SSV ##
#########

SSV_4corr <- merge(data.Luc[variable == 'SSV'],data.Lab[variable == 'SSV'],  by = 'datum', all = T)
SSV_4corr <- merge(SSV_4corr, data.Vit[variable == 'SSV'], by = 'datum', all = T)
SSV_4corr <- merge(SSV_4corr, data.Har[variable == 'SSV'], by = 'datum', all = T)
SSV_4corr <- SSV_4corr[, c('datum', 'Hodnota.x', 'Hodnota.y', 'Hodnota.x', 'Hodnota.y')]
names(SSV_4corr) <- c('datum',names_SCE_statins[1:4])
SSV_4corr <- merge(SSV_4corr, data.Pec[variable == 'SSV'], by = 'datum', all = T)
SSV_4corr <- SSV_4corr[,2:6]
names(SSV_4corr) <- names_SCE_statins

SSV_cor.mat <- cor(SSV_4corr, use = 'complete.obs', method = 'spearman')
corrplot(SSV_cor.mat, type = 'upper', order = 'hclust', method = 'number', 
         title = "Sunshine duration correlation (Spearman) among stations", mar=c(0,0,1,0))

## H ##
#########

H_4corr <- merge(data.Luc[variable == 'H'],data.Lab[variable == 'H'],  by = 'datum', all = T)
H_4corr <- merge(H_4corr, data.Vit[variable == 'H'], by = 'datum', all = T)
H_4corr <- merge(H_4corr, data.Har[variable == 'H'], by = 'datum', all = T)
H_4corr <- H_4corr[, c('datum', 'Hodnota.x', 'Hodnota.y', 'Hodnota.x', 'Hodnota.y')]
names(H_4corr) <- c('datum',names_SCE_statins[1:4])
H_4corr <- merge(H_4corr, data.Pec[variable == 'H'], by = 'datum', all = T)
H_4corr <- H_4corr[,2:6]
names(H_4corr) <- names_SCE_statins

H_cor.mat <- cor(H_4corr, use = 'complete.obs', method = 'spearman')
corrplot(H_cor.mat, type = 'upper', order = 'hclust', method = 'number', 
         title = "Relative humidity correlation (Spearman) among stations", mar=c(0,0,1,0))

## T ##
#########

T_4corr <- merge(data.Luc[variable == 'T'],data.Lab[variable == 'T'],  by = 'datum', all = T)
T_4corr <- merge(T_4corr, data.Vit[variable == 'T'], by = 'datum', all = T)
T_4corr <- merge(T_4corr, data.Har[variable == 'T'], by = 'datum', all = T)
T_4corr <- T_4corr[, c('datum', 'Hodnota.x', 'Hodnota.y', 'Hodnota.x', 'Hodnota.y')]
names(T_4corr) <- c('datum',names_SCE_statins[1:4])
T_4corr <- merge(T_4corr, data.Pec[variable == 'T'], by = 'datum', all = T)
T_4corr <- T_4corr[,2:6]
names(T_4corr) <- names_SCE_statins

T_cor.mat <- cor(T_4corr, use = 'complete.obs', method = 'spearman')
T_corr <- corrplot(T_cor.mat, type = 'upper', order = 'hclust', method = 'number', 
         title = "Temperature correlation (Spearman) among stations", mar=c(0,0,1,0))

## F ##
#########

F_4corr <- merge(data.Luc[variable == 'F'],data.Lab[variable == 'F'],  by = 'datum', all = T)
F_4corr <- merge(F_4corr, data.Vit[variable == 'F'], by = 'datum', all = T)
F_4corr <- merge(F_4corr, data.Har[variable == 'F'], by = 'datum', all = T)
F_4corr <- F_4corr[, c('datum', 'Hodnota.x', 'Hodnota.y', 'Hodnota.x', 'Hodnota.y')]
names(F_4corr) <- c('datum',names_SCE_statins[1:4])
F_4corr <- merge(F_4corr, data.Pec[variable == 'F'], by = 'datum', all = T)
F_4corr <- F_4corr[,2:6]
names(F_4corr) <- names_SCE_statins

F_cor.mat <- cor(F_4corr, use = 'complete.obs', method = 'spearman')
Fcorr <- corrplot(F_cor.mat, type = 'upper', order = 'hclust', method = 'number', 
         title = "Wind speed correlation (Spearman) among stations", mar=c(0,0,1,0))


