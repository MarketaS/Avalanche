# Script for creating complete meteo data set involving western territory (Labska, Vitkovice-Vrbatka, Harrachov)
# Created by @Roman.Juras

library(data.table)
library(readr)
library(ggplot2)
library(lubridate)
library(writexl)
library(dplyr)
library(roll)


data.dir <- "D:/CESNET/Data_snih/Laviny_Krkonose/Avalanche_analysis/Data/"
result.dir <- "D:/CESNET/Data_snih/Laviny_Krkonose/Avalanche_analysis/Results/"

# Reading processed meteo data
# created in the script - Meteo_Daily_CHMI.R
data.Luc <- readRDS(file = paste0(data.dir, "/processed_data/data.Luc_1961-2020.rds"))
data.Lab <- readRDS(file = paste0(data.dir, "/processed_data/data.Lab_1961-2020.rds"))
data.Har <- readRDS(file = paste0(data.dir, "/processed_data/data.Har_1961-2020.rds"))
data.Vit <- readRDS(file = paste0(data.dir, "/processed_data/data.Vit_1961-2020.rds"))
data.Pec <- readRDS(file = paste0(data.dir, "/processed_data/data.Pec_1961-2020.rds"))
data.daily_all <- readRDS(file = paste0(data.dir, "/processed_data/data.daily_1961-2020.rds"))

# Separating data for individual Stations

# Harrachov
Har_P <- data.daily_all[Variable == 'SRA' & Station == 'P2HARR01']
Har_T <- data.daily_all[Variable == 'T' & Station == 'P2HARR01']
Har_H <- data.daily_all[Variable == 'H' & Station == 'P2HARR01']

Har_meteo <- merge(Har_P, Har_T, by = 'Date')
Har_meteo <- merge(Har_meteo, Har_H, by = 'Date', all = T)
Har_names <- c('Date', 'P_Har', 'T_Har', 'H_Har')
Har_meteo_short <- Har_meteo[, c('Date', 'Value.x', 'Value.y', 'Value')]
names(Har_meteo_short) <- Har_names

# computing Wet-bulb temperature (Tw) based on Stull formnula (https://doi.org/10.1175/JAMC-D-11-0143.1) 
# the results were randomly validated by online calculator: https://www.omnicalculator.com/physics/wet-bulb
Har_meteo_short[, Tw_Har := T_Har * atan(0.151977*(H_Har + 8.313659)^0.5) + atan(T_Har + H_Har) - atan(H_Har - 1.676331) + 0.00391838*(H_Har)^(3/2)*atan(0.023101*H_Har)-4.686035]

# setting Temperature Threshold (TT)
TT_Tw <- -0.5 # TT based on Tw - a rough guess based on the Principles of Snow Hydrology (Fig. 10.3)
TT_Ta <- 0.8 # TT based on the measured Air temperature and HBV calibration: value for Velka Mumlava 
Cum_TT <- 10 # TT for the 5day cummulative mean daily air temperature. This should indicate some melt and therefore LWC in the snowpack

# distinguish between rain and snow
Har_meteo_short[, Rain_Har_Tw := ifelse(Tw_Har >= TT_Tw, P_Har, 0)]
Har_meteo_short[, Rain_Har_Ta := ifelse(T_Har >= TT_Ta, P_Har, 0)]

# Labska + Vrbatka = WTop (West top)
WTop_P <- data.daily_all[Variable == 'SRA' & (Station == 'H1VITK01' | Station == 'H1LBOU01')]
WTop_T <- data.daily_all[Variable == 'T' & (Station == 'H1VITK01' | Station == 'H1LBOU01')]
WTop_H <- data.daily_all[Variable == 'H' & (Station == 'H1VITK01' | Station == 'H1LBOU01')]

WTop_meteo <- merge(WTop_P, WTop_T, by = 'Date')
WTop_meteo <- merge(WTop_meteo, WTop_H, by = 'Date', all = T)
WTop_names <- c('Date', 'P_WTop', 'T_WTop', 'H_WTop', 'Station_H')
WTop_meteo_short <- WTop_meteo[, c('Date', 'Value.x', 'Value.y', 'Value', 'Station.x')]
names(WTop_meteo_short) <- WTop_names

# Merging Harrachov and WTop stations data into one West territory dta 
West_meteo <- merge(x = Har_meteo_short, y = WTop_meteo_short, by = 'Date')

# Assess missing values Relative Humidity in Labska based on lm (Harrachov~Labska) y = 14 + 0.88y (see Meteo_Daily_CHMI.R - L328-334 and Humidity_Harrachov_Labska_all.png)
# compute also another type of regression, beside linear (exponetntial, logaritmic, etc. - see more options)
West_meteo[, H_Lab_calc := 14 + 0.88 * H_Har] 
West_meteo[, H_WTop_completed := ifelse(!is.na(H_WTop), H_WTop, H_Lab_calc)]
West_meteo[, H_WTop_note := ifelse(!is.na(H_WTop), 'Measured', 'Calculated')]
West_meteo[, Station_P := Station_H]

elev_Harr <- 675
elev_Lab <- 1320
elev_Vit <- 1410
T_grad_const <- -0.55


# computing lapse rate between Harrachov and Labska, just for evaluating the constant value above
West_meteo[, T_grad := (T_WTop-T_Har)/(elev_Vit-elev_Harr)*100]

ggplot(West_meteo, aes(y = T_grad, fill = month(Date) )) + geom_boxplot()

summary(West_meteo[month(Date) < 6][, T_grad])

##### Plotting the monthly lapse rate in western territory
ggplot(West_meteo, aes(x = factor(month(Date)), y = T_grad) )+
  geom_boxplot(position="dodge2")+
  theme_classic()+
  labs(x = "", y ="Temperature gradient [?C/100m]")+
  #scale_fill_manual(values = c( "darkorange","royalblue4"))+
  scale_x_discrete(limits=c("1","2","3","4","5","6","7","8","9","10","11","12"), 
                   labels = c("Jan","Feb", "Mar", "Apr", "May","Jun","Jul","Aug","Sep", "Oct", "Nov","Dec"))+ 
  ggtitle ('Lapse Rate - Western territory')
#theme_generic + theme(plot.title = element_text(size = 14))

###
# computing Wet-bulb temperature (Tw) based on Stull formnula (https://doi.org/10.1175/JAMC-D-11-0143.1) 

# in case of all data available
WTop_meteo_short[, Tw_WTop := T_WTop * atan(0.151977*(H_WTop + 8.313659)^0.5) + atan(T_WTop + H_WTop) - atan(H_WTop - 1.676331) + 0.00391838*(H_WTop)^(3/2)*atan(0.023101*H_WTop)-4.686035]


# Computing missing T values in WTop station (Labska/Vrbatka) based on constant T_grad_const
#West_meteo[, T_Lab_calc := T_Har + (elev_Lab - elev_Harr)/100 * T_grad_const]
West_meteo[, T_Lab_calc := T_Har + (elev_Lab - elev_Harr)/100 * T_grad]
West_meteo[, T_Lab_note := ifelse(!is.na(T_WTop) & Station_H == 'H1LBOU01', 'Measured', 'Calculated')]
West_meteo[, Ta_WTop_completed := ifelse(T_Lab_note == 'Measured', T_WTop, T_Lab_calc)]  # Ta - air temperature 

# computing Wet-bulb temperature (Tw) based on Stull formnula (https://doi.org/10.1175/JAMC-D-11-0143.1) 

# 1. in case of all data available
#West_meteo[, Tw_WTop := T_WTop * atan(0.151977*(H_WTop + 8.313659)^0.5) + atan(T_WTop + H_WTop) - atan(H_WTop - 1.676331) + 0.00391838*(H_WTop)^(3/2)*atan(0.023101*H_WTop)-4.686035]
West_meteo[, Tw_WTop := Ta_WTop_completed * atan(0.151977*(H_WTop + 8.313659)^0.5) + atan(Ta_WTop_completed + H_WTop) - atan(H_WTop - 1.676331) + 0.00391838*(H_WTop)^(3/2)*atan(0.023101*H_WTop)-4.686035]

# 2. in case of measured T_WTop and H_WTop is missing - use T_Lab_calc and H_Lab_calc
# no cases (if we use variable T_grad)
West_meteo[, Tw_WTop_inter := ifelse(is.na(T_WTop) & is.na(H_WTop),T_Lab_calc * atan(0.151977*(H_Lab_calc + 8.313659)^0.5) + atan(T_Lab_calc + H_Lab_calc) - atan(H_Lab_calc - 1.676331) + 0.00391838*(H_Lab_calc)^(3/2)*atan(0.023101*H_Lab_calc)-4.686035, NA)]

# 3. in case of only measured H_WTop is missing - use H_Lab_calc
#West_meteo[, Tw_WTop_inter_II := ifelse(is.na(H_WTop) & !is.na(T_WTop),T_WTop * atan(0.151977*(H_Lab_calc + 8.313659)^0.5) + atan(T_WTop + H_Lab_calc) - atan(H_Lab_calc - 1.676331) + 0.00391838*(H_Lab_calc)^(3/2)*atan(0.023101*H_Lab_calc)-4.686035, NA)]
# this is enough
West_meteo[, Tw_WTop_inter_II := ifelse(is.na(H_WTop) & !is.na(Ta_WTop_completed),T_WTop * atan(0.151977*(H_Lab_calc + 8.313659)^0.5) + atan(Ta_WTop_completed + H_Lab_calc) - atan(H_Lab_calc - 1.676331) + 0.00391838*(H_Lab_calc)^(3/2)*atan(0.023101*H_Lab_calc)-4.686035, NA)]

# 4. in case of only measured T_WTop is missing - use T_Lab_calc
West_meteo[, Tw_WTop_inter_III := ifelse(!is.na(H_WTop) & is.na(T_WTop),T_Lab_calc * atan(0.151977*(H_WTop + 8.313659)^0.5) + atan(T_Lab_calc + H_WTop) - atan(H_WTop - 1.676331) + 0.00391838*(H_WTop)^(3/2)*atan(0.023101*H_WTop)-4.686035, NA)]

# complete Relative humidity (H) data for WTop (Labska/ Vrbatka) mix with measured or interpolated data
West_meteo[, Tw_WTop_note := "NA"]
West_meteo[, Tw_WTop_completed := ifelse(!is.na(Tw_WTop), Tw_WTop, Tw_WTop_inter)]
West_meteo[, Tw_WTop_completed := ifelse(is.na(Tw_WTop) & !is.na(Tw_WTop_inter_II), Tw_WTop_inter_II, Tw_WTop_completed)]
West_meteo[, Tw_WTop_completed := ifelse(is.na(Tw_WTop) & !is.na(Tw_WTop_inter_III), Tw_WTop_inter_III, Tw_WTop_completed)]

# Add an info how we get the Tw Value
West_meteo[, Tw_WTop_note := ifelse(!is.na(Tw_WTop), "Measured", Tw_WTop_note)]
West_meteo[, Tw_WTop_note := ifelse(!is.na(Tw_WTop_inter), "Inter", Tw_WTop_note)]
West_meteo[, Tw_WTop_note := ifelse(!is.na(Tw_WTop_inter_II), "Calculated", Tw_WTop_note)]
West_meteo[, Tw_WTop_note := ifelse(!is.na(Tw_WTop_inter_III), "Inter_III", Tw_WTop_note)]

# distinguish between rain and snow at WTop station
# setting Temperature Threshold (TT) for Labska
TT_Tw <- -0.5 # TT based on Tw - a rough guess based on the Principles of Snow Hydrology (Fig. 10.3)
TT_Ta <- 0.46 # TT based on the measured Air temperature and HBV calibration: value for Labe 

West_meteo[, Rain_WTop_Tw := ifelse(Tw_WTop_completed >= TT_Tw, P_WTop, 0)]
West_meteo[, Rain_WTop_Ta := ifelse(T_WTop >= TT_Ta, P_WTop, 0)]

# Checking the differences in Rain data - different definition
nrow(West_meteo[Rain_WTop_Tw > 0])
nrow(West_meteo[Rain_WTop_Ta > 0])

sum(West_meteo[,Rain_WTop_Ta], na.rm = T)
sum(West_meteo[,Rain_WTop_Tw], na.rm = T)

ggplot(West_meteo, aes(x = Date, y = Rain_WTop_Tw)) + geom_point(col = 'red') +
  geom_point(aes(y = Rain_WTop_Ta), col = 'blue')

#####

# Select only relevant columns in west_meteo

West_meteo_selected <- West_meteo[, c('Date','P_WTop','Station_P', 'Ta_WTop_completed', 'T_Lab_note', 'T_grad', 'H_WTop_completed', 'H_WTop_note', 'Station_H', 'Tw_WTop_completed', 'Tw_WTop_note', 'Rain_WTop_Tw', 'Rain_WTop_Ta')]

# adding other variable only from Vrbatka and Labska bouda stations
# Wind speed
Data_F <- data.daily_all[Variable == 'F' & (Station == 'H1VITK01' | Station == 'H1LBOU01') ]
Data_F[, c('Sign', 'Variable') := NULL]
names(Data_F) <- c('Date', 'F', 'Station_F')
# Total snow depth (prefer LBOU data if available at the same day as Vitkovice, need to separated these two station, because of some duplicated dates)
Data_SCE_LBOU <- data.daily_all[Variable == 'SCE' & (Station == 'H1LBOU01') ]
Data_SCE_LBOU[, c('Sign', 'Variable') := NULL]
names(Data_SCE_LBOU) <- c('Date', 'SCE', 'Station_SCE')

Data_SCE_Vit <- data.daily_all[Variable == 'SCE' & (Station == 'H1VITK01') ]
Data_SCE_Vit[, c('Sign', 'Variable') := NULL]
names(Data_SCE_Vit) <- c('Date', 'SCE', 'Station_SCE')

Data_SCE <- merge(Data_SCE_LBOU, Data_SCE_Vit, by = 'Date', all = T)
Data_SCE[, SCE_completed := ifelse(!is.na(SCE.x), SCE.x, SCE.y)]
Data_SCE[, Station_SCE := ifelse(!is.na(Station_SCE.x), Station_SCE.x, Station_SCE.y)]
Data_SCE[, c('SCE.x', 'Station_SCE.x', 'SCE.y', 'Station_SCE.y') := NULL]
names(Data_SCE) <- c('Date', 'SCE', 'Station_SCE')
#Fresch snow depth (prefer LBOU data if available at the same day as Vitkovice, need to separated these two station, because of some duplicated dates)
Data_SNO_LBOU <- data.daily_all[Variable == 'SNO' & (Station == 'H1LBOU01') ]
Data_SNO_LBOU[, c('Sign', 'Variable') := NULL]
names(Data_SNO_LBOU) <- c('Date', 'SNO', 'Station_SNO')

Data_SNO_Vit <- data.daily_all[Variable == 'SNO' & (Station == 'H1VITK01') ]
Data_SNO_Vit[, c('Sign', 'Variable') := NULL]
names(Data_SNO_Vit) <- c('Date', 'SNO', 'Station_SNO')

Data_SNO <- merge(Data_SNO_LBOU, Data_SNO_Vit, by = 'Date', all = T)
Data_SNO[, SNO_completed := ifelse(!is.na(SNO.x), SNO.x, SNO.y)]
Data_SNO[, Station_SNO := ifelse(!is.na(Station_SNO.x), Station_SNO.x, Station_SNO.y)]
Data_SNO[, c('SNO.x', 'Station_SNO.x', 'SNO.y', 'Station_SNO.y') := NULL]
names(Data_SNO) <- c('Date', 'SNO', 'Station_SNO')
# Sunshine duration
Data_SSV <- data.daily_all[Variable == 'SSV' & (Station == 'H1VITK01' | Station == 'H1LBOU01') ]
Data_SSV[, c('Sign', 'Variable') := NULL]
names(Data_SSV) <- c('Date', 'SSV', 'Station_SSV')
# Total precipitation
Data_SRA <- data.daily_all[Variable == 'SRA' & (Station == 'H1VITK01' | Station == 'H1LBOU01') ]
Data_SRA[, c('Sign', 'Variable') := NULL]
names(Data_SRA) <- c('Date', 'SRA', 'Station_SRA')

# Merging all dta with meteo data in one
meteo.list <- list( Data_F, Data_SCE, Data_SNO, Data_SSV, Data_SRA, West_meteo_selected)
Meteo_4Aval <- Reduce(function(...) merge(..., by = 'Date', all = T), meteo.list)
# Delete some no-needed columns
Meteo_4Aval[, c('P_WTop', 'Station_P', 'Tw_WTop_completed', 'Tw_WTop_note') := NULL]

setnames(Meteo_4Aval, c('Ta_WTop_completed', 'T_Lab_note', 'H_WTop_completed', 'H_WTop_note', 'Rain_WTop_Tw', 'Rain_WTop_Ta'), c('Tair', 'Tair_note', 'H','H_note' , 'Rain_Tw', 'Rain_Ta'))
# saving 
write.table(Meteo_4Aval, file = paste0(data.dir,'processed_data/Meteo_4Aval.csv'), quote = F, row.names = F)
saveRDS(Meteo_4Aval, file = paste0(data.dir,'processed_data/Meteo_4Aval.RDS'))

