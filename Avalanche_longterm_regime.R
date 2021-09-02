# Script for analysing long term avalanche regime (seasonal distribution, size distribution)
# Focus on Avalanche size dynamics over different time scales: 30y period, decade, annual
# Created by @Roman.Juras

library(data.table)
library(readr)
library(ggplot2)
library(lubridate)
library(writexl)
library(readxl)
library(dplyr)
library(tidyr)
#library(devtools)
#install_github("easyGgplot2", "kassambara")
#library(easyGgplot2)

data.dir <- "D:/CESNET/Data_snih/Laviny_Krkonose/Avalanche_analysis/Data/"
result.dir <- "D:/CESNET/Data_snih/Laviny_Krkonose/Avalanche_analysis/Results/"

setwd(data.dir)

# Read avalanche data from 1.1.1961 until 15.05.2020, continuous days distinguished aval and non-aval days
######
Aval_1 <- data.table(read_delim(paste0(data.dir,"Aval_1.txt"), 
                             "\t", escape_double = FALSE, col_types = cols(A = col_number(), 
                                                                           B = col_number(), C = col_number(), 
                                                                           D = col_number(), E = col_number(), 
                                                                           F = col_number(), G = col_number(), 
                                                                           H = col_number(), J = col_number(), 
                                                                           K = col_number(), L = col_number(), 
                                                                           M = col_number(), N = col_number(), 
                                                                           O = col_number(), cadastr_letter = col_character(), 
                                                                           cadastr_number = col_number(), Path = col_character(),
                                                                           date = col_character(), 
                                                                           day = col_number(), event = col_number(), 
                                                                           exposure = col_character(), locality = col_character(), 
                                                                           month = col_number(), notes = col_character(), 
                                                                           ranking = col_number(), season = col_character(), 
                                                                           year = col_number()), trim_ws = TRUE, locale = locale(encoding = "windows-1252")))


# same data as previous, but reading from excel
Aval_2 <- data.table(read_excel(paste0(data.dir,"Aval.xlsx")))
Aval_1 <- Aval_1[, N := as.numeric(N)]
Aval_1 <- Aval_1[, M := as.numeric(M)]
Aval_1 <- Aval_1[, C := as.numeric(C)]
Aval_1 <- Aval_1[, Datum := as.Date(date)]
##########

# Aval data 1961 - 2021 - continuous time series including aval and non-aval days
Aval_1 <- data.table(read_excel(paste0(data.dir,"Avalanche_event_1961_2021_edited_RJ.xlsx")))
Aval_1[, Datum := as.Date(date)]
Aval_1 <- Aval_1[, N := as.numeric(N)]
Aval_1 <- Aval_1[, M := as.numeric(M)]

# Updated database - corrected errors in aval size (due to excel conversion) by Valerian Spusta
# Not continuous time series - only aval event dates
# Exposure column needs to be checked with the real aval path shapefile
# A-A column (added) is dedicated for the second part of A column (y), in case of format such as x_y, for details see email to Valin Spusta from 21.6.2021
Aval_update <- data.table(read_excel(paste0(data.dir,"Lavinove Zaznamy_Krkonose_SpustaUpdate_2021_RJ.xlsx"), sheet = 'R_style', na = 'x'))
Aval_update[,date := as.Date(do.call(paste, c(.SD, sep = '-')), format = '%Y-%m-%d'), .SDcols = c('r','m','d')] # convert columns 'd' 'm' 'r' to date format

Aval_update <- Aval_update[, N := as.numeric(N)] # length of avalanche
Aval_update <- Aval_update[, M := as.numeric(M)] # width of avalanche
Aval_update <- Aval_update[, K := as.numeric(K)] # height of crown face

Aval_1 <- Aval_update
#####
# read file with aval path info - cadastre
Paths <- data.table(read_excel(paste0(data.dir,"avalanche_path_overview.xlsx")))

# Calculate max length of the Avalanche (max N)
# Length_max <- Aval_1[,.(Max_Length = max(N, na.rm = T)), by = c("Path")] # maximum avalanche length per path 
# Width_max <- Aval_1[,.(max_width = max(M, na.rm = T)), by = c("Path")]
# Trigger_max <- Aval_1[,.(max_width = max(K, na.rm = T)), by = c("Path")]

Length_max <- Aval_1[,.(Max_Length = max(N, na.rm = T)), by = c("path_number")] # maximum avalanche length per path 
#Width_max <- Aval_1[,.(max_width = max(M, na.rm = T)), by = c("path_number")] # width according to width of avalanche
Width_max <- Aval_1[,.(max_width = max(L, na.rm = T)), by = c("path_number")] # width according to width of crown face
Trigger_max <- Aval_1[,.(max_crown = max(K, na.rm = T)), by = c("path_number")]

# create data.table only with the max dimmensions
Aval_max_dimensions <- data.table(Length_max, Width_max[,max_width], Trigger_max[,max_crown])

#Aval_database <- merge(x = Aval_1, y = Aval_max_dimensions, by = 'Path')
Aval_database <- merge(x = Aval_1, y = Aval_max_dimensions, by = 'path_number')

setnames(Aval_database, old = c('V2', 'V3'), new = c('Max_Width', 'Max_crown'))

##
Aval_size <- data.table(Aval_database[, c('Date', 'path_number', 'path_letter', 'Season', 'K', 'L', 'N', 'C', 'Max_Length', 'Max_Width', 'Max_crown')])
##

# Set avalanche size as a portion of maximum size (length/width) appeared in the particular path
Aval_size[, Aval_size_1 := N/Max_Length]
Aval_size[, Aval_size_2 := L/Max_Width]
#Aval_size[, Aval_size_3 := K/Max_crown]
#Aval_size[, Aval_size_4 := N*L/(Max_Length*Max_Width)]
#Aval_size[, Aval_size_5 := N*L*K/(Max_Length*Max_Width*Max_crown)]

# Create avalanche size categories 1/5

#######
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

# Aval size by K #
# Aval_size[Aval_size_3 <= 0.2, Size_categ_3 := '1']
# Aval_size[Aval_size_3 > 0.2 & Aval_size_3 <= 0.4, Size_categ_3 := '2']
# Aval_size[Aval_size_3 > 0.4 & Aval_size_3 <= 0.6, Size_categ_3 := '3']
# Aval_size[Aval_size_3 > 0.6 & Aval_size_3 <= 0.8, Size_categ_3 := '4']
# Aval_size[Aval_size_3 > 0.8, Size_categ_3 := '5']
# 
# # Aval size by L*N #
# Aval_size[Aval_size_4 <= 0.2, Size_categ_4 := '1']
# Aval_size[Aval_size_4 > 0.2 & Aval_size_4 <= 0.4, Size_categ_4 := '2']
# Aval_size[Aval_size_4 > 0.4 & Aval_size_4 <= 0.6, Size_categ_4 := '3']
# Aval_size[Aval_size_4 > 0.6 & Aval_size_4 <= 0.8, Size_categ_4 := '4']
# Aval_size[Aval_size_4 > 0.8, Size_categ_4 := '5']
# 
# # Aval size by L*N*K #
# Aval_size[Aval_size_5 <= 0.2, Size_categ_5 := '1']
# Aval_size[Aval_size_5 > 0.2 & Aval_size_5 <= 0.4, Size_categ_5 := '2']
# Aval_size[Aval_size_5 > 0.4 & Aval_size_5 <= 0.6, Size_categ_5 := '3']
# Aval_size[Aval_size_5 > 0.6 & Aval_size_5 <= 0.8, Size_categ_5 := '4']
# Aval_size[Aval_size_5 > 0.8, Size_categ_5 := '5']

#################

# Create categories for decades
# Aval_database[Datum <= '1971-06-30', Decade := '1961-1971']
# Aval_database[Datum > '1971-06-30' & Datum <= '1981-06-30', Decade := '1971-1981']
# Aval_database[Datum > '1981-06-30' & Datum <= '1991-06-30', Decade := '1981-1991']
# Aval_database[Datum > '1991-06-30' & Datum <= '2001-06-30', Decade := '1991-2001']
# Aval_database[Datum > '2001-06-30' & Datum <= '2011-06-30', Decade := '2001-2011']
# Aval_database[Datum > '2011-06-30' & Datum <= '2021-06-30', Decade := '2011-2021']

####

Aval_size[Date <= '1971-06-30', Decade := '1961-1971']
Aval_size[Date > '1971-06-30' & Date <= '1981-06-30', Decade := '1971-1981']
Aval_size[Date > '1981-06-30' & Date <= '1991-06-30', Decade := '1981-1991']
Aval_size[Date > '1991-06-30' & Date <= '2001-06-30', Decade := '1991-2001']
Aval_size[Date > '2001-06-30' & Date <= '2011-06-30', Decade := '2001-2011']
Aval_size[Date > '2011-06-30' & Date <= '2021-06-30', Decade := '2011-2021']

Aval_size[Date <= '1991-06-30', Thirty := '1961-1991']
Aval_size[Date > '1991-06-30' & Date <= '2021-06-30', Thirty := '1991-2021']

####

Nr_wet_aval <- nrow(Aval_1[C == 2])


Aval_size[, .(count = .N), by = c('Decade', 'Size_categ_1')][Decade == '2001-2011']
Decade_count_1 <- Aval_size[, .(count = .N), by = c('Decade', 'Size_categ_1')][Size_categ_1 == 1]
Decade_count_2 <- Aval_size[, .(count = .N), by = c('Decade', 'Size_categ_1')][Size_categ_1 == 2]
Decade_count_3 <- Aval_size[, .(count = .N), by = c('Decade', 'Size_categ_1')][Size_categ_1 == 3]
Decade_count_4 <- Aval_size[, .(count = .N), by = c('Decade', 'Size_categ_1')][Size_categ_1 == 4]
Decade_count_5 <- Aval_size[, .(count = .N), by = c('Decade', 'Size_categ_1')][Size_categ_1 == 5]

setnames(x = Decade_count_1, old = 'count', new = 'Size 1')
setnames(x = Decade_count_2, old = 'count', new = 'Size 2')
setnames(x = Decade_count_3, old = 'count', new = 'Size 3')
setnames(x = Decade_count_4, old = 'count', new = 'Size 4')
setnames(x = Decade_count_5, old = 'count', new = 'Size 5')

# ascending order by decade 
Decade_count_1 <- Decade_count_1[order(Decade, na.last = TRUE)]
Decade_count_2 <- Decade_count_2[order(Decade, na.last = TRUE)]
Decade_count_3 <- Decade_count_3[order(Decade, na.last = TRUE)]
Decade_count_4 <- Decade_count_4[order(Decade, na.last = TRUE)]
Decade_count_5 <- Decade_count_5[order(Decade, na.last = TRUE)]
###########
# Playing with summariying etc. - possible candidate to delete
Decade_count <- data.table(Decade_count_1[, 1], Decade_count_1[, 3], Decade_count_2[, 3], Decade_count_3[, 3], Decade_count_4[, 3], Decade_count_5[, 3])

for (i in 2:6) {
Decade_count[i, decade_sum := sum(Decade_count[i,2:6])]
}

Decade_percentage <- Decade_count
for (i in 2:6) {
  print(i)
  Decade_percentage[,i] <- Decade_count[,i]/Decade_count[,7]*100
  
}

Decade_count_melt <- melt(Decade_count, id.vars = 'Decade')
Decade_count_melt[.,(sum.dec = sum(value)), by = 'Decade']

library(dplyr)

Dc_Trans <- ddply(Decade_count, 'Decade', transform)

#####

#####
Decade_count <- merge(Decade_count_1, Decade_count_2, by = 'Decade')
Decade_count <- merge(Decade_count, Decade_count_3, by = 'Decade')
Decade_count <- merge(Decade_count, Decade_count_4, by = 'Decade')
Decade_count <- Decade_count[, c('Decade', 'Size 1', 'Size 2', 'Size 3', 'Size 4')]
Decade_count <- merge(Decade_count, Decade_count_5, by = 'Decade')
Decade_count[, Size_categ_1 := NULL]


Decade_count_percentage <- 

##size category 1 
######### 
# plotting per Decade 
   
  # total number
  ggplot(Aval_size[Size_categ_1 > 0 & !is.na(Decade)], aes(x = Decade, fill = Size_categ_1)) + 
  geom_bar(stat="count", position = position_dodge()) + 
  labs(fill = "Avalanche Size") +
  ggtitle("Avalanches size distribution over the decades, by length  (biggest = 5)")  

ggsave(paste0(result.dir,"Aval_size_decade_total_byN.png"), width = 10, height = 6, units = "in")

#######
# total number of wet avalanches
ggplot(Aval_size[Size_categ_1 > 0 & !is.na(Decade) & C == 2], aes(x = Decade, fill = Size_categ_1)) + 
  geom_bar(stat="count", position = position_dodge()) + 
  labs(fill = "Avalanche Size") +
  ggtitle("Wet avalanches size distribution over the decades, by length  (biggest = 5), C = 2")  

ggsave(paste0(result.dir,"Wet_Aval_size_decade_total_byN.png"), width = 10, height = 6, units = "in")

# per month
ggplot(Aval_size[Size_categ_1 > 0 & !is.na(Decade) & C == 2], aes(x = Decade, fill = Size_categ_1)) + 
  geom_bar(stat="count", position = position_dodge()) + 
  labs(fill = "Avalanche Size") +
  facet_wrap(~month(date)) +
  ggtitle("Wet avalanches size distribution over the decades, by length  (biggest = 5), C = 2")  

ggplot(Aval_size[Size_categ_1 > 0 & !is.na(Decade) & C == 2], aes(x = Decade)) + 
  geom_bar(stat="count", position = position_dodge()) + 
  labs(fill = "Avalanche Size") +
  facet_wrap(~month(date)) +
  ggtitle("Wet avalanches size distribution over the decades, by length  (biggest = 5), C = 2")  


#######



# Percentage
ggplot(Aval_size[Size_categ_1 > 0 & !is.na(Decade) & C == 2], aes(x = Decade, fill = Size_categ_1)) + 
  # geom_bar(position = "fill", stat = "identity") + 
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(fill = "Avalanche Size") +
  ggtitle("Avalanches size distribution over the decades, by length (biggest = 5)") 

ggsave(paste0(result.dir,"Aval_size_decade_percentage_byN.png"), width = 10, height = 6, units = "in")


# plotting per Thirty
# Total
ggplot(Aval_size[Size_categ_1 > 0 & !is.na(Thirty)], aes(x = Thirty, fill = Size_categ_1)) + 
  geom_bar(stat="count", position = position_dodge()) + 
  labs(fill = "Avalanche Size") +
  ggtitle("Avalanches size distribution over the 30y period, by length  (biggest = 5)")  

ggsave(paste0(result.dir,"Aval_size_Thirty_total_byN.png"), width = 10, height = 6, units = "in")

# Percentage
ggplot(Aval_size[Size_categ_1 > 0 & !is.na(Thirty)], aes(x = Thirty, fill = Size_categ_1)) + 
  # geom_bar(position = "fill", stat = "identity") + 
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(fill = "Avalanche Size") +
  ggtitle("Avalanches size distribution over the 30y period, by length (biggest = 5)") 

ggsave(paste0(result.dir,"Aval_size_Thirty_percentage_byN.png"), width = 10, height = 6, units = "in")

# plotting per Year
# Total
ggplot(Aval_size[Size_categ_1 > 0 & !is.na(year(date))], aes(x = year(date), fill = Size_categ_1)) + 
  geom_bar(stat="count", position = position_dodge()) + 
  labs(fill = "Avalanche Size") +
  ggtitle("Avalanches size distribution - Annual, by length  (biggest = 5)")  

ggsave(paste0(result.dir,"Aval_size_Year_total_byN.png"), width = 10, height = 6, units = "in")

######
# Total wet avalanches
ggplot(Aval_size[Size_categ_1 > 0 & !is.na(year(date)) & C == 2], aes(x = year(date), fill = Size_categ_1)) + 
  geom_bar(stat="count", position = position_dodge()) + 
  labs(fill = "Avalanche Size") +
  ggtitle("Wet Avalanches size distribution - Annual, by length  (biggest = 5, C = 2)")  

ggsave(paste0(result.dir,"Wet_Aval_size_Year_total_byN.png"), width = 10, height = 6, units = "in")



#####

# Percentage
ggplot(Aval_size[Size_categ_1 > 0 & !is.na(year(date))], aes(x = year(date), fill = Size_categ_1)) + 
  # geom_bar(position = "fill", stat = "identity") + 
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(fill = "Avalanche Size") +
  ggtitle("Avalanches size distribution - Annual, by length (biggest = 5)") 

ggsave(paste0(result.dir,"Aval_size_Year_percentage_byN.png"), width = 10, height = 6, units = "in")

################
# ##size category 2
# plotting per Decade 

# total number
ggplot(Aval_size[Size_categ_2 > 0 & !is.na(Decade)], aes(x = Decade, fill = Size_categ_2)) + 
  geom_bar(stat="count", position = position_dodge()) + 
  labs(fill = "Avalanche Size") +
  ggtitle("Avalanches size distribution over the decades, by width  (biggest = 5)")  

ggsave(paste0(result.dir,"Aval_size_decade_total_byM.png"), width = 10, height = 6, units = "in")

# Percentage
ggplot(Aval_size[Size_categ_2 > 0 & !is.na(Decade)], aes(x = Decade, fill = Size_categ_2)) + 
  # geom_bar(position = "fill", stat = "identity") + 
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(fill = "Avalanche Size") +
  ggtitle("Avalanches size distribution over the decades, by width (biggest = 5)") 

ggsave(paste0(result.dir,"Aval_size_decade_percentage_byM.png"), width = 10, height = 6, units = "in")


# plotting per Thirty
# Total
ggplot(Aval_size[Size_categ_2 > 0 & !is.na(Thirty)], aes(x = Thirty, fill = Size_categ_2)) + 
  geom_bar(stat="count", position = position_dodge()) + 
  labs(fill = "Avalanche Size") +
  ggtitle("Avalanches size distribution over the 30y period, by width  (biggest = 5)")  

ggsave(paste0(result.dir,"Aval_size_Thirty_total_byM.png"), width = 10, height = 6, units = "in")

# Percentage
ggplot(Aval_size[Size_categ_2 > 0 & !is.na(Thirty)], aes(x = Thirty, fill = Size_categ_2)) + 
  # geom_bar(position = "fill", stat = "identity") + 
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(fill = "Avalanche Size") +
  ggtitle("Avalanches size distribution over the 30y period, by width (biggest = 5)") 

ggsave(paste0(result.dir,"Aval_size_Thirty_percentage_byM.png"), width = 10, height = 6, units = "in")

# plotting per Year
# Total
ggplot(Aval_size[Size_categ_2 > 0 & !is.na(year(date))], aes(x = year(date), fill = Size_categ_2)) + 
  geom_bar(stat="count", position = position_dodge()) + 
  labs(fill = "Avalanche Size") +
  ggtitle("Avalanches size distribution - Annual, by width  (biggest = 5)")  

ggsave(paste0(result.dir,"Aval_size_Year_total_byM.png"), width = 10, height = 6, units = "in")

# Percentage
ggplot(Aval_size[Size_categ_2 > 0 & !is.na(year(date))], aes(x = year(date), fill = Size_categ_2)) + 
  # geom_bar(position = "fill", stat = "identity") + 
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(fill = "Avalanche Size") +
  ggtitle("Avalanches size distribution - Annual, by width (biggest = 5)") 

ggsave(paste0(result.dir,"Aval_size_Year_percentage_byM.png"), width = 10, height = 6, units = "in")


########
# category 3
ggplot(Aval_size[Size_categ_3 > 0 & !is.na(Decade)], aes(x = Decade, fill = Size_categ_3)) + 
  geom_bar(stat="count", position = 'stack') + 
  labs(fill = "Avalanche Size") +
  ggtitle("Occurence of different avalanches sizes over the decades (biggest = 5)") 

############################
# category 4
# plotting per Decade 

# total number
ggplot(Aval_size[Size_categ_4 > 0 & !is.na(Decade)], aes(x = Decade, fill = Size_categ_4)) + 
  geom_bar(stat="count", position = position_dodge()) + 
  labs(fill = "Avalanche Size") +
  ggtitle("Avalanches size distribution over the decades, by width x length  (biggest = 5)")  

ggsave(paste0(result.dir,"Aval_size_decade_total_byLxN.png"), width = 10, height = 6, units = "in")

######
# wet aval - total number
ggplot(Aval_size[Size_categ_4 > 0 & !is.na(Decade) & C == 2], aes(x = Decade, fill = Size_categ_4)) + 
  geom_bar(stat="count", position = position_dodge()) + 
  labs(fill = "Avalanche Size") +
  ggtitle("Wet Avalanches size distribution over the decades, by width x length  (biggest = 5, C = 2)")  

ggsave(paste0(result.dir,"Wet_Aval_size_decade_total_byLxN.png"), width = 10, height = 6, units = "in")

#####


# Percentage
ggplot(Aval_size[Size_categ_4 > 0 & !is.na(Decade)], aes(x = Decade, fill = Size_categ_4)) + 
  # geom_bar(position = "fill", stat = "identity") + 
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(fill = "Avalanche Size") +
  ggtitle("Avalanches size distribution over the decades, by width x length (biggest = 5)") 

ggsave(paste0(result.dir,"Aval_size_decade_percentage_byMxN.png"), width = 10, height = 6, units = "in")


# plotting per Thirty
# Total
ggplot(Aval_size[Size_categ_4 > 0 & !is.na(Thirty)], aes(x = Thirty, fill = Size_categ_4)) + 
  geom_bar(stat="count", position = position_dodge()) + 
  labs(fill = "Avalanche Size") +
  ggtitle("Avalanches size distribution over the 30y period, by width x length  (biggest = 5)")  

ggsave(paste0(result.dir,"Aval_size_Thirty_total_byMxN.png"), width = 10, height = 6, units = "in")

# Percentage
ggplot(Aval_size[Size_categ_4 > 0 & !is.na(Thirty)], aes(x = Thirty, fill = Size_categ_4)) + 
  # geom_bar(position = "fill", stat = "identity") + 
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(fill = "Avalanche Size") +
  ggtitle("Avalanches size distribution over the 30y period, by width x length (biggest = 5)") 

ggsave(paste0(result.dir,"Aval_size_Thirty_percentage_byMxN.png"), width = 10, height = 6, units = "in")

# plotting per Year
# Total
ggplot(Aval_size[Size_categ_4 > 0 & !is.na(year(date))], aes(x = year(date), fill = Size_categ_4)) + 
  geom_bar(stat="count", position = position_dodge()) + 
  labs(fill = "Avalanche Size") +
  ggtitle("Avalanches size distribution - Annual, by width x length  (biggest = 5)")  

ggsave(paste0(result.dir,"Aval_size_Year_total_byMxN.png"), width = 10, height = 6, units = "in")

# Percentage
ggplot(Aval_size[Size_categ_4 > 0 & !is.na(year(date))], aes(x = year(date), fill = Size_categ_4)) + 
  # geom_bar(position = "fill", stat = "identity") + 
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(fill = "Avalanche Size") +
  ggtitle("Avalanches size distribution - Annual, by width x length x length (biggest = 5)") 

ggsave(paste0(result.dir,"Aval_size_Year_percentage_byMxN.png"), width = 10, height = 6, units = "in")

############################


############################

######
# by month / percentage
ggplot(data=Aval_database[Size_categ > 0], aes(x=Decade, fill = Size_categ)) +
  #geom_bar(position="dodge") +
  geom_bar(position = "fill") + 
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~month(Datum)) +
  ggtitle("Avalanche size ditribution over the season - months (biggest = 5)")  + 
  labs(fill = "Avalanche Size")

ggsave(paste0(result.dir,"Aval_size_decade_months_perc.png"), width = 10, height = 6, units = "in")

ggplot(data=Aval_database[Size_categ > 0], aes(x=Decade, fill = Size_categ)) +
  #geom_bar(position="dodge") +
  geom_bar(stat="count", position = 'stack') +
  facet_wrap(~month(Datum)) +
  ggtitle("Avalanche size ditribution over the season - months (biggest = 5)")  + 
  labs(fill = "Avalanche Size")

# Wet avalanches
# plotting per Decade
ggplot(Aval_database[Size_categ > 0], aes(x = Decade, fill = C)) + geom_bar(position="dodge") + 
  labs(fill = "Avalanche wetness") +
  scale_fill_manual(values = c("red", "blue", "green", "orange"), labels=c("dry", "wet", "mixed", "NA")) +
  ggtitle(paste0("Avalanche ditribution over the decades - ", Nr_wet_aval,' Wet Avalanches'))  

ggsave(paste0(result.dir,"Wet_Avalanche_decade.png"), width = 10, height = 6, units = "in")

ggplot(data=Aval_database[Size_categ > 0 & !is.na(C)], aes(x=Decade, fill = C)) +
  #geom_bar(position="dodge") +
  geom_bar(stat="count", position = 'dodge') +
  scale_fill_manual(values = c("red", "blue", "green", "orange"), labels=c("dry", "wet", "mixed", "NA")) +
  facet_wrap(~month(Datum)) +
  ggtitle(paste0("Wet Avalanche ditribution over the season - months - ", Nr_wet_aval," Wet Avalanches \n (According to database C parameter)"))  + 
  labs(fill = "Avalanche wetness")

ggsave(paste0(result.dir,"Wet_Avalanche_decade_months.png"), width = 14, height = 6, units = "in")

ggplot(data=Aval_61_21[!is.na(C)], aes(x=year(date), fill = C)) +
  #geom_bar(position="dodge") +
  geom_bar(stat="count", position = 'dodge') +
  scale_fill_manual(values = c("red", "blue", "green", "orange"), labels=c("dry", "wet", "mixed", "NA")) +
  facet_wrap(~month(date)) +
  ggtitle("Wet Avalanche ditribution over the season - months \n (According to database C parameter)")  + 
  labs(fill = "Avalanche wetness")




######

# Barplot for avalanche pathway aspect and number
ggplot(Aval[!is.na(exposure)], aes(x = exposure)) + geom_bar()
ggplot(Aval[!is.na(cadastr_number)], aes(x = cadastr_number)) + geom_bar()

ggplot(Aval[!is.na(N)], aes(x = N)) + geom_boxplot()
ggplot(Aval[!is.na(M)], aes(x = M)) + geom_boxplot()
ggplot(Aval[!is.na(K) & K < 2], aes(x = K)) + geom_boxplot()

summary(Aval[!is.na(L) & cadastr_number == 37][,L])

Names_Aval <- data.table(unique(Aval_1[,Path]))
Names_Aval <- Names_Aval[order(Names_Aval)]

Names_Path <- data.table(unique(Paths[,Path]))

# Problems :
# 1. unique names in Aval_1[,Path] = 60 x Paths[,Path]) = 52 -> need to unify
extra_names_Aval_1 <- c('11A', '11B', '11C', '16A', '16B', '23A', '32A', '40', '7A') 

# links to plot
# http://www.sthda.com/english/wiki/ggplot2-barplot-easy-bar-graphs-in-r-software-using-ggplot2
