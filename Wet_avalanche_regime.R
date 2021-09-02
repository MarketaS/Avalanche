# updated script for Avalanche data analysing and ploting 
# Merging Meteo Data and Avalanche
# created by @Roman.Juras

library(data.table)
library(readr)
library(ggplot2)
library(lubridate)
library(writexl)
library(readxl)
library(dplyr)
library(roll)
library(gridExtra)


data.dir <- "D:/CESNET/Data_snih/Laviny_Krkonose/Avalanche_analysis/Data/"
result.dir <- "D:/CESNET/Data_snih/Laviny_Krkonose/Avalanche_analysis/Results/"


# Read up to date avlanche database
#Aval_61_21 <- data.table(read.table(paste0(data.dir,"Avalanche_event_1961_2021.csv"), sep = ',', header = T))
Aval_61_21 <- data.table(read.delim("D:/CESNET/Data_snih/Laviny_Krkonose/Avalanche_analysis/Scripts/Avalanche_Roman/Aval_utf_8.txt"))
Aval_61_21[, date := as.Date(date)]
Aval_61_21[, C := as.character(C)]
Aval_61_21[, B := as.character(B)]
Aval_61_21[, A := as.character(A)]
Aval_61_21 <- Aval_61_21[, N := as.numeric(N)] # length of avalanche
Aval_61_21 <- Aval_61_21[, M := as.numeric(M)] # width of avalanche
Aval_61_21 <- Aval_61_21[, K := as.numeric(K)] # height of crown face

#Merge Meteo Data with the Avalanche database
#Aval_meteo <- merge(Daily_LBOU,Aval_61_21, by = 'date', all = T)

##########################################################
# New meteo part based on the script Meteo_Daily_CHMI.R #
#########################################################


# separate East and West avalanche path according to the cadastr_number; 1-23, 38-40 East, 24-37 West 
Aval_61_21[, cadastr_number := as.numeric(cadastr_number)] 
Aval_61_21[, position := ifelse(cadastr_number < 24 | cadastr_number > 37, 'East', 'West')]

# Updated database - corrected errors in aval size (due to excel conversion) by Valerian Spusta
# Not continuous time series - only aval event dates
# Exposure column needs to be checked with the real aval path shapefile
# A-A column (added) is dedicated for the second part of A column (y), in case of format such as x_y, for details see email to Valin Spusta from 21.6.2021
# Aval_update <- data.table(read_excel(paste0(data.dir,"Lavinove Zaznamy_Krkonose_SpustaUpdate_2021_RJ.xlsx"), sheet = 'R_style', na = 'x'))
# Aval_update[,Date := as.Date(do.call(paste, c(.SD, sep = '-')), format = '%Y-%m-%d'), .SDcols = c('r','m','d')] # convert columns 'd' 'm' 'r' to date format
# 
# Aval_update <- Aval_update[, N := as.numeric(N)] # length of avalanche
# Aval_update <- Aval_update[, M := as.numeric(M)] # width of avalanche
# Aval_update <- Aval_update[, K := as.numeric(K)] # height of crown face

  Aval_1 <- Aval_61_21

##### AValanche Size estimation #####
#####################################
# 
# Length_max <- Aval_1[,.(Max_Length = max(N, na.rm = T)), by = c("path_number")] # maximum avalanche length per path 
# Width_max <- Aval_1[,.(max_width = max(L, na.rm = T)), by = c("path_number")] # width according to width of crown face
# Trigger_max <- Aval_1[,.(max_crown = max(K, na.rm = T)), by = c("path_number")]

Length_max <- Aval_1[,.(Max_Length = max(N, na.rm = T)), by = c("cadastr_number")] # maximum avalanche length per path 
Width_max <- Aval_1[,.(max_width = max(L, na.rm = T)), by = c("cadastr_number")] # width according to width of crown face
Trigger_max <- Aval_1[,.(max_crown = max(K, na.rm = T)), by = c("cadastr_number")]

# create data.table only with the max dimmensions
Aval_max_dimensions <- data.table(Length_max, Width_max[,max_width], Trigger_max[,max_crown])

#Aval_database <- merge(x = Aval_1, y = Aval_max_dimensions, by = 'Path')
#Aval_database <- merge(x = Aval_1, y = Aval_max_dimensions, by = 'path_number')
Aval_database <- merge(x = Aval_1, y = Aval_max_dimensions, by = 'cadastr_number')

setnames(Aval_database, old = c('V2', 'V3'), new = c('Max_Width', 'Max_crown'))

##
#Aval_size <- data.table(Aval_database[, c('Date', 'path_number', 'path_letter', 'Season', 'K', 'L', 'N', 'C', 'Max_Length', 'Max_Width', 'Max_crown')])
Aval_size <- data.table(Aval_database[, c('date', 'cadastr_number', 'cadastr_letter', 'season', 'A', 'B','K', 'L', 'N', 'C', 'Max_Length', 'Max_Width', 'Max_crown')])
setnames(Aval_size, 'date', 'Date')
##

# Set avalanche size as a portion of maximum size (length/width) appeared in the particular path
Aval_size[, Aval_size_1 := N/Max_Length]
Aval_size[, Aval_size_2 := L/Max_Width]

# Aval size by N #

Aval_size[Aval_size_1 <= 0.2, Size_categ_1 := '1']
Aval_size[Aval_size_1 > 0.2 & Aval_size_1 <= 0.4, Size_categ_1 := '2']
Aval_size[Aval_size_1 > 0.4 & Aval_size_1 <= 0.6, Size_categ_1 := '3']
Aval_size[Aval_size_1 > 0.6 & Aval_size_1 <= 0.8, Size_categ_1 := '4']
Aval_size[Aval_size_1 > 0.8, Size_categ_1 := '5']

# Aval size by L #
Aval_size[Aval_size_2 <= 0.2, Size_categ_2 := '1']
Aval_size[Aval_size_2 > 0.2 & Aval_size_2 <= 0.4, Size_categ_2 := '2']
Aval_size[Aval_size_2 > 0.4 & Aval_size_2 <= 0.6, Size_categ_2 := '3']
Aval_size[Aval_size_2 > 0.6 & Aval_size_2 <= 0.8, Size_categ_2 := '4']
Aval_size[Aval_size_2 > 0.8, Size_categ_2 := '5']

####

Aval_size[Date <= '1971-06-30', Decade := '1961-1971']
Aval_size[Date > '1971-06-30' & Date <= '1981-06-30', Decade := '1971-1981']
Aval_size[Date > '1981-06-30' & Date <= '1991-06-30', Decade := '1981-1991']
Aval_size[Date > '1991-06-30' & Date <= '2001-06-30', Decade := '1991-2001']
Aval_size[Date > '2001-06-30' & Date <= '2011-06-30', Decade := '2001-2011']
Aval_size[Date > '2011-06-30' & Date <= '2021-06-30', Decade := '2011-2021']

Aval_size[Date <= '1991-06-30', Thirty := '1961-1991']
Aval_size[Date > '1991-06-30' & Date <= '2021-06-30', Thirty := '1991-2021']


####################################

#Aval_database <- merge(Aval_update, Aval_size, by = 'Date')

# read merged daily meteo data

Meteo_4Aval <- readRDS(file = paste0(data.dir,'processed_data/Meteo_4Aval.RDS'))
###


# Cummulative rain according to different threshold
Cum_window <- 5  # Nr. of days for rolling sum

Meteo_4Aval[, CumRain5_Tw := frollsum(Rain_Tw, Cum_window)]
Meteo_4Aval[, CumRain5_Ta := frollsum(Rain_Ta, Cum_window)]

Meteo_4Aval[, CumTemp5 := frollsum(Tair, Cum_window)]


#########################################################
# Merging Avalanche and Meteo Data
Aval_meteo <- merge(Aval_size, Meteo_4Aval, by = 'Date', all = T)


# Check the wet avalanche limits 

summary(Aval_meteo[C == 2][, CumRain5_Ta])
summary(Aval_meteo[C == 2][, CumRain5_Tw])
summary(Aval_meteo[C == 2][, CumTemp5])
nrow(Aval_meteo[C == 2])


# Use the Wet avalanche limits for the further definition
RainLimit <- 6.8
TempLimit <- 0.2
# Define wet avalanches based on rolling sums
Aval_meteo[, Wet_aval_RainTw := ifelse((CumRain5_Ta >= RainLimit) & cadastr_number >= 1, "WetRain", NA)]
Aval_meteo[, Wet_aval_Temp := ifelse((CumTemp5 >= TempLimit) & cadastr_number >= 1, "WetTemp", NA)]
Aval_meteo[, Wet_aval_all := ifelse((CumRain5_Ta >= RainLimit | CumTemp5 >= TempLimit) & cadastr_number >= 1, "WetAll", NA)]

summary(Aval_meteo[Wet_aval_all == 'WetAll'][,CumRain5_Ta])
View(Aval_meteo[Wet_aval_all == 'WetAll'])
nrow(Aval_meteo[Wet_aval_all == 'WetAll'])

# Number of wet avalanches - different definitions/views

Nr_wet_aval = nrow(Aval_meteo[C == 2])
Nr_wet_aval2 = nrow(Aval_meteo[Wet_aval_all == 'WetAll'])
Nr_wet_aval3 = nrow(Aval_meteo[Wet_aval_RainTw == 'WetRain'])
Nr_wet_aval4 = nrow(Aval_meteo[Wet_aval_Temp == 'WetTemp'])
Nr_slab_aval = nrow(Aval_meteo[A == 3 | A == 4])
Nr_cornice_aval = nrow(Aval_meteo[A == 5])



# Plotting is in separate script "Plotting_avalanche_daily.R", following code is not necessary needed
# W.Aval_Count_C <- ggplot(data = Aval_meteo[C == 2], aes(x = Decade, fill = Size_categ_1)) +
#   #geom_bar(position="dodge") +
#   geom_bar(stat="count", position = 'stack', fill = "coral") +
#   ggtitle(paste0("Wet Avalanche distribution over the decades \n (after deQuervain) - ", Nr_wet_aval,
#                 " Wet avalanches C = 2"))  +
#   labs(fill = "Avalanche wetness")
# 
# ggsave(paste0(result.dir,"/Wet_avalanches/Wet_Avalanches_C_Count.png"), dpi = 600, width = 9.2, height = 8, units = "in")
# 
# W.Aval_Count_WetAll <- ggplot(data = Aval_meteo[Wet_aval_all == 'WetAll'], aes(x = Decade, fill = Size_categ_2)) +
#   #geom_bar(position="dodge") +
#   geom_bar(stat="count", position = 'stack', fill = "aquamarine4") +
# 
#   ggtitle(paste0("Wet Avalanche distribution over the decades \n (Our definition) - ", Nr_wet_aval2,
#                 " Wet avalanches \n 5day rain >= ", RainLimit, 'mm or 5day sum Temperature >=', TempLimit, '°C'))  +
#   labs(fill = "Avalanche wetness")
# 
# ggsave(paste0(result.dir,"/Wet_avalanches/Wet_Avalanches_OurDefAll_Count.png"), dpi = 600, width = 9.2, height = 8, units = "in")
# 
# ggplot(data = Aval_meteo[Wet_aval_RainTw == 'WetRain'], aes(x = Decade)) +
#   #geom_bar(position="dodge") +
#   geom_bar(stat="count", position = 'stack', fill = "cornflowerblue") +
#   ggtitle(paste0("Wet Avalanche ditribution over the decades \n (Our definition) - ", Nr_wet_aval3,
#                  " Wet avalanches \n 5day rain >= ", RainLimit, 'mm'))  +
#   labs(fill = "Avalanche wetness")
# 
# ggsave(paste0(result.dir,"/Wet_avalanches/Wet_Avalanches_OurDefRain_Count.png"), dpi = 600, width = 9.2, height = 8, units = "in")
# 
# ggplot(data = Aval_meteo[Wet_aval_Temp == 'WetTemp'], aes(x = Decade)) +
#   #geom_bar(position="dodge") +
#   geom_bar(stat="count", position = 'stack', fill = "darkgoldenrod4") +
#   ggtitle(paste0("Wet Avalanche ditribution over the decades (Our definition) - \n", Nr_wet_aval4,
#                  " Wet avalanches \n 5day sum Temperature >=", TempLimit, '°C'))  +
#   labs(fill = "Avalanche wetness")
# 
# ggsave(paste0(result.dir,"/Wet_avalanches/Wet_Avalanches_OurDefTemp_Count.png"), dpi = 600, width = 9.2, height = 8, units = "in")
# 
# ###################
# 
# # Wet avalanches according to C
# W.Aval_CumRain_C_size <- ggplot(Aval_meteo[C == 2], aes(x = Decade, y = CumRain5_Ta, fill = Size_categ_1)) +
#   geom_boxplot(position="dodge2")+
#   theme_classic() + labs(x = '', y = '5day Cummulative Rain (Ta) [mm]', fill = 'Avalanche size') +
#   ggtitle('Rain before avalanche - Wet avalanche (C = 2)')
# 
# ggsave(paste0(result.dir,"/Wet_avalanches/Wet_Avalanches_C_Cum5Rain_Ta_Size.png"), dpi = 600, width = 9.2, height = 8, units = "in")
# 
# W.Aval_CumRain_C <- ggplot(Aval_meteo[C == 2], aes(x = Decade, y = CumRain5_Ta)) +
#   geom_boxplot(position="dodge2", color="red", fill="orange", alpha=0.2) +
#   labs(x = '', y = '5day Cummulative Rain (Ta) [mm]')  +
#   theme_classic() +
#   ggtitle('Rain before avalanche - Wet avalanche (C = 2)')
# 
# ggsave(paste0(result.dir,"/Wet_avalanches/Wet_Avalanches_C_Cum5_Ta.png"), dpi = 600, width = 9.2, height = 8, units = "in")
# 
# ggplot(Aval_meteo[C == 2], aes(x = Decade, y = CumRain5_Tw, fill = Size_categ_1)) +
#   geom_boxplot(position="dodge2")+
#   theme_classic() + labs(x = '', y = '5day Cummulative Rain (Tw) [mm]', fill = 'Avalanche size') +
#   ggtitle('Rain before avalanche - Wet avalanche (C = 2)')
# 
# ggsave(paste0(result.dir,"/Wet_avalanches/Wet_Avalanches_C_Cum5Rain_Tw_Size.png"), dpi = 600, width = 9.2, height = 8, units = "in")
# 
# ggplot(Aval_meteo[C == 2], aes(x = Decade, y = CumRain5_Tw)) +
#   geom_boxplot(position="dodge2", color="red", fill="orange", alpha=0.2) +
#   labs(x = '', y = '5day Cummulative Rain (Tw) [mm]')  +
#   theme_classic() +
#   ggtitle('Rain before avalanche - Wet avalanche (C = 2)')
# 
# ggsave(paste0(result.dir,"/Wet_avalanches/Wet_Avalanches_C_Cum5Rain_Tw.png"), dpi = 600, width = 9.2, height = 8, units = "in")
# 
# 
# W.Aval_CumTemp_C_size <- ggplot(Aval_meteo[C == 2], aes(x = Decade, y = CumTemp5, fill = Size_categ_1)) +
#   geom_boxplot(position="dodge2")+
#   theme_classic() + labs(x = '', y = '5day Cummulative temperature [°C]', fill = 'Avalanche size') +
#   ggtitle('Temperature before avalanche - Wet avalanche (C = 2)')
# 
# ggsave(paste0(result.dir,"/Wet_avalanches/Wet_Avalanches_C_CumTemp5_Size.png"), dpi = 600, width = 9.2, height = 8, units = "in")
# 
# W.Aval_CumTemp_C <- ggplot(Aval_meteo[C == 2], aes(x = Decade, y = CumTemp5)) +
#   geom_boxplot(position="dodge2", color="red", fill="orange", alpha=0.2) +
#   labs(x = '', y = '5day Cummulative temperature [°C]')  +
#   theme_classic() +
#   ggtitle('Temperature before avalanche - Wet avalanche (C = 2)')
# 
# ggsave(paste0(result.dir,"/Wet_avalanches/Wet_Avalanches_C_CumTemp5.png"), dpi = 600, width = 9.2, height = 8, units = "in")
# 
# 
# grid.arrange(W.Aval_Count_C, W.Aval_CumRain_C, W.Aval_CumTemp_C, nrow = 3)
# ggsave(paste0(result.dir,"/Wet_avalanches/Wet_Avalanches_C.png"), dpi = 600, width = 9.2, height = 10, units = "in")
# 
# # Wet avalanches according to WetAll
# W.Aval_CumRain_WetAll_size <- ggplot(Aval_meteo[Wet_aval_all == 'WetAll'], aes(x = Decade, y = CumRain5_Ta, fill = Size_categ_1)) +
#   geom_boxplot(position="dodge2")+
#   theme_classic() + labs(x = '', y = '5day Cummulative Rain (Ta) [mm]', fill = 'Avalanche size') +
#   ggtitle('Rain before avalanche - Wet avalanche (WetAll)')
# 
# ggsave(paste0(result.dir,"/Wet_avalanches/Wet_Avalanches_WetAll_Cum5Rain_Ta_Size.png"), dpi = 600, width = 9.2, height = 8, units = "in")
# 
# 
# W.Aval_CumRain_WetAll <- ggplot(Aval_meteo[Wet_aval_all == 'WetAll'], aes(x = Decade, y = CumRain5_Ta)) +
#   geom_boxplot(position="dodge2", color="red", fill="aquamarine4", alpha=0.2) +
#   labs(x = '', y = '5day Cummulative Rain (Ta) [mm]')  +
#   theme_classic() +
#   ggtitle('Rain before avalanche - Wet avalanche (WetAll)')
# 
# ggsave(paste0(result.dir,"/Wet_avalanches/Wet_Avalanches_WetAll_Cum5Rain_Ta.png"), dpi = 600, width = 9.2, height = 8, units = "in")
# 
# 
# ggplot(Aval_meteo[Wet_aval_all == 'WetAll'], aes(x = Decade, y = CumRain5_Tw, fill = Size_categ_1)) +
#   geom_boxplot(position="dodge2")+
#   theme_classic() + labs(x = '', y = '5day Cummulative Rain (Tw) [mm]', fill = 'Avalanche size') +
#   ggtitle('Rain before avalanche - Wet avalanche (WetAll)')
# 
# ggsave(paste0(result.dir,"/Wet_avalanches/Wet_Avalanches_WetAll_Cum5Rain_Tw_Size.png"), dpi = 600, width = 9.2, height = 8, units = "in")
# 
# 
# ggplot(Aval_meteo[Wet_aval_all == 'WetAll'], aes(x = Decade, y = CumRain5_Tw)) +
#   geom_boxplot(position="dodge2", color="red", fill="orange", alpha=0.2) +
#   labs(x = '', y = '5day Cummulative Rain (Tw) [mm]')  +
#   theme_classic() +
#   ggtitle('Rain before avalanche - Wet avalanche (WetAll)')
# 
# ggsave(paste0(result.dir,"/Wet_avalanches/Wet_Avalanches_WetAll_Cum5Rain_Tw.png"), dpi = 600, width = 9.2, height = 8, units = "in")
# 
# 
# ggplot(Aval_meteo[Wet_aval_all == 'WetAll'], aes(x = Decade, y = CumTemp5, fill = Size_categ_1)) +
#   geom_boxplot(position="dodge2")+
#   theme_classic() + labs(x = '', y = '5day Cummulative temperature [°C]', fill = 'Avalanche size') +
#   ggtitle('Temperature before avalanche - Wet avalanche (Our Definition - WetAll)')
# 
# ggsave(paste0(result.dir,"/Wet_avalanches/Wet_Avalanches_WetAll_Cum5Temp_Size.png"), dpi = 600, width = 9.2, height = 8, units = "in")
# 
# 
# W.Aval_CumTemp_WetAll <- ggplot(Aval_meteo[Wet_aval_all == 'WetAll'], aes(x = Decade, y = CumTemp5)) +
#   geom_boxplot(position="dodge2", color="red", fill="aquamarine4", alpha=0.2) +
#   labs(x = '', y = '5day Cummulative temperature [°C]')  +
#   theme_classic() +
#   ggtitle('Temperature before avalanche - Wet avalanche (Our Definition - WetAll)')
# 
# ggsave(paste0(result.dir,"/Wet_avalanches/Wet_Avalanches_WetAll_Cum5Temp.png"), dpi = 600, width = 9.2, height = 8, units = "in")
# 
# 
# grid.arrange(W.Aval_Count_WetAll, W.Aval_CumRain_WetAll, W.Aval_CumTemp_WetAll, nrow = 3)
# 
# 
# ##################
# 
# 
# ## Next code can be deleted
# 
# 
# # Create data.table only with needed columns
# Wet_Aval_meteo <- Aval_meteo[, c('date','SRA', "SCE", "SNO", "SVH", "Tprum", "cadastr_number",
# "cadastr_letter", "K", "Kmin", "Kmax", "L", "M", "N", "O",  "event_text", "CumTemp5", "rain",
# "CumRain5", "Wet_aval")]
# 
# Wet_Aval_meteo <- Wet_Aval_meteo[Wet_aval == 'wet']
# Nr_wet_aval2 <- nrow(Wet_Aval_meteo)
# 
# Wet_Aval_meteo[date <= '1971-06-30', Decade := '1961-1971']
# Wet_Aval_meteo[date > '1971-06-30' & date <= '1981-06-30', Decade := '1971-1981']
# Wet_Aval_meteo[date > '1981-06-30' & date <= '1991-06-30', Decade := '1981-1991']
# Wet_Aval_meteo[date > '1991-06-30' & date <= '2001-06-30', Decade := '1991-2001']
# Wet_Aval_meteo[date > '2001-06-30' & date <= '2011-06-30', Decade := '2001-2011']
# Wet_Aval_meteo[date > '2011-06-30' & date <= '2021-06-30', Decade := '2011-2021']
# 
# 
# # plot time distribution of wet avalanches - based on our definition
# ggplot(data=Wet_Aval_meteo, aes(x= Decade)) +
#   #geom_bar(position="dodge") +
#   geom_bar(stat="count", position = 'stack', fill = "violet") +
#   facet_wrap(~month(date)) +
#   ggtitle(paste0("Wet Avalanche ditribution over the season - months (Our definition) - ", Nr_wet_aval2, " Wet avalanches \n
#                  5day sum rain >= 5mm or 5day sum Temperature >= 10°C"))  +
#   labs(fill = "Avalanche wetness")
# 
# ggsave(paste0(result.dir,"Wet_Avalanche_Months_ourDefinition.png"), width = 14, height = 6, units = "in")
# 
# ggplot(data=Wet_Aval_meteo, aes(x=year(date))) +
#   #geom_bar(position="dodge") +
#   geom_bar(stat="count", position = 'stack', fill = "coral2") +
#   ggtitle(paste0("Wet Avalanche ditribution over the season -decades (Our definition) - ", Nr_wet_aval2,
#                  " Wet avalanches \n 5day rain >= 5mm or 5day sum Temperature >= 10°C"))  +
#   labs(fill = "Avalanche wetness")
# 
# ggsave(paste0(result.dir,"Wet_Avalanche_Decade_ourDefinition.png"), width = 14, height = 6, units = "in")
# 
# 
# # meteo data availability from Labska Bouda
# ggplot(data = Daily_LBOU[!is.na(SRA) & !is.na(date)], aes(x = year(date), fill = SRA)) +
#   geom_bar(stat="count", position = 'dodge', fill = 'lightblue') +
#   facet_wrap(~month(date)) +
#   ggtitle("Data availability from LBOU - Precipitation") +
#   labs(y = 'Days available')
# 
# ggsave(paste0(result.dir,"Data_Availability_LBOU_Prec.png"), width = 14, height = 6, units = "in")
# 
# ggplot(data = Daily_LBOU[!is.na(Tprum) & !is.na(date)], aes(x = year(date), col = Tprum)) +
#   geom_bar(stat="count", position = 'dodge', fill = 'red') +
#   facet_wrap(~month(date)) +
#   ggtitle("Data availability from LBOU - Average Temperature") +
#   labs(y = 'Days available')
# 
# ggsave(paste0(result.dir,"Data_Availability_LBOU_Temp.png"), width = 14, height = 6, units = "in")
# 
# ggplot(data = Daily_LBOU[!is.na(Hprum) & !is.na(date)], aes(x = year(date), fill = Hprum)) +
#   geom_bar(stat="count", position = 'dodge', fill = 'green') +
#   ggtitle("Data availability from LBOU - Average Humidity") +
#   facet_wrap(~month(date)) +
#   labs(y = 'Days available')
# 
# ggsave(paste0(result.dir,"Data_Availability_LBOU_Humidity.png"), width = 14, height = 6, units = "in")