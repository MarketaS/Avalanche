library(data.table)
library(readr)
library(zoo)
library(dplyr)
library(ggplot2)

################### AVAL DATA ######################

setwd ("C:/Users/marketa.souckova/Documents/laviny/")
Aval <- data.table(read_delim("./Aval_utf_8_new.txt", 
                              "\t", escape_double = FALSE, col_types = cols( A = col_character(), 
                                                                             B = col_character(), C = col_character(), 
                                                                             D = col_character(), E = col_character(), 
                                                                             F = col_character(), G = col_character(), 
                                                                             H = col_character(), J = col_character(), 
                                                                             K = col_number(), Kmax = col_number(),
                                                                             L = col_number(), 
                                                                             M = col_number(), Mmax = col_number(),
                                                                             N = col_number(), 
                                                                             O = col_number(), 
                                                                             cadastr_letter = col_character(), 
                                                                             cadastr_number = col_number(), date = col_character(), 
                                                                             day = col_number(), event = col_number(),
                                                                             exposure = col_character(), locality = col_character(), 
                                                                             month = col_number(), notes = col_character(), 
                                                                             ranking = col_number(), season = col_character(), 
                                                                             year = col_number()), trim_ws = TRUE, locale = locale(encoding = "windows-1252")))
problems(Aval)

Aval$DATE2 <- strtrim(x = Aval$date, width = 10)
Aval$DATE3 <- as.POSIXct(paste0(Aval$DATE2, " 07:00"), format = "%Y-%m-%d %H:%M")
Aval$DATE_OFF <- as.POSIXct(Aval$DATE3 + (17*60*60))

W_aval <- c(24:37)
E_aval <- c(1,2,3,4, 5, 6, 7,8,9,10,11,12, 13,14,15,16,17,18,19,20,21,22,23,38,39)
Aval[, locality := ifelse(cadastr_number %in% W_aval, 'W', 'E')]
aval_lbou <- Aval[locality == "W",]
aval_lucb <- Aval[locality == "E",]
aval_lbou[event == 1]
aval_lucb <- aval_lucb[event == 1]

non_aval_lucb <- Aval[year(DATE3) > 2008 & month(DATE3) %in% c(10, 11, 12, 1, 2, 3, 4, 5) & event == 0,]
non_aval_lbou <- Aval[year(DATE3) > 2003 & month(DATE3) %in% c(10, 11, 12, 1, 2, 3, 4, 5) & event == 0,]

non_aval_lbou[, locality:= "W"]
non_aval_lucb[, locality:= "E"]

aval_lbou <- aval_lbou[year(DATE3) > 2003]
aval_lucb <- aval_lucb[year(DATE3) > 2008]

aval_total_lucb <- rbind(aval_lucb, non_aval_lucb)
aval_total_lbou <- rbind(aval_lbou, non_aval_lbou)

aval_total_lucb$ID <- paste0("Aval ", 1:nrow(aval_total_lucb))
aval_total_lbou$ID <- paste0("Aval ", 1:nrow(aval_total_lbou))

aval_total_lucb_pos <- aval_total_lucb[event == 1]
aval_total_lbou_pos <- aval_total_lbou[event == 1]

aval_melt_total <- readRDS(file = "./my_data/lbou_lucb_data_new.rds")

aval_total_lucb_pos <- aval_total_lucb_pos[,.(ID, A, B, C, D, E, F, G, H, J, K, Kmax, L, M, Mmax, N, O)]
aval_total_lbou_pos <- aval_total_lbou_pos[,.(ID, A, B, C, D, E, F, G, H, J, K, Kmax, L, M, Mmax, N, O)]

aval_melt_lbou <- left_join(x = aval_melt_total[stat == "LBOU"], y = aval_total_lbou_pos, by = "ID") 
aval_melt_lucb <- left_join(x = aval_melt_total[stat == "LUCB"], y = aval_total_lucb_pos, by = "ID")

aval_melt_abc <- rbind(aval_melt_lbou, aval_melt_lucb)

saveRDS(aval_melt_abc, "./my_data/aval_melt_abc.rds")
aval_melt_abc <- readRDS("./data/aval_melt_abc.rds")

aval_melt_abc[var == "SCE", var:= "snow depth [cm]"] 
aval_melt_abc[var == "SVH", var:= "snow water equivalent [mm]"] 
aval_melt_abc[var == "SNO", var:= "new snow sum [cm]"] 
aval_melt_abc[var == "D", var:= "wind direction [°]"] 
aval_melt_abc[var == "Fprum", var:= "averadge wind speed [m/s]"]
aval_melt_abc[var == "Fmax", var:= "maximum wind speed [m/s]"]
aval_melt_abc[var == "SRA1H", var:= "precipitation [mm]"]
aval_melt_abc[var == "SSV1H", var:= "sun light duration [0.1/hour]"]
aval_melt_abc[var == "T", var:= "air temperature [°C]"]
aval_melt_abc[var == "Tdiff", var:= "air temperature difference [°C]"]
aval_melt_abc[var == "H", var:= "relative air humidity [%]"]
aval_melt_abc[var == "T05", var:= "soil temperature in 5 cm [%]"]

#add in all [C == 2 & N > 100, ID]
plot_wet <- aval_melt_abc[event == 1 & C == 2 & variable == "value", mean(value, na.rm = T), by = .(PLOT, stat, var, variable)]
plot_slab <- aval_melt_abc[event == 1 & A %in% c(2,3,4) & variable == "value", mean(value, na.rm = T), by = .(PLOT, stat, var, variable)]
#plot_over <- aval_melt_abc[event == 1 & A %in% c(5) & variable == "value", mean(value, na.rm = T), by = .(PLOT, stat, var, variable)]

num_wet <- length(aval_melt_abc[event == 1 & C == 2 & variable == "value", unique(ID)])
num_slab <- length(aval_melt_abc[event == 1 & A %in% c(2, 3, 4) & variable == "value", unique(ID)])
#num_over <- length(aval_melt_abc[event == 1 & A %in% c(5) & variable == "value", unique(ID)])

plot_wet$variable <- NULL
plot_slab$variable <- NULL
#plot_over$variable <- NULL

ggplot(plot_wet)+
  geom_line(aes(x = PLOT, y = V1, color = stat), alpha = 0.5)+
  geom_smooth(aes(x = PLOT, y = V1, color = stat), method = "lm")+
  facet_wrap(~ var, scales = "free_y") +
  scale_x_reverse(breaks = c(0, 24, 48, 72, 96, 120, 144))+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(title = paste0("Wet avalanches (count ", num_wet, ")"), y = "", x = "number of hours", color = "station")

ggplot(plot_slab)+
  geom_line(aes(x = PLOT, y = V1, color = stat), alpha = 0.5)+
  geom_smooth(aes(x = PLOT, y = V1, color = stat), method = "lm")+
  facet_wrap(~ var, scales = "free_y")+
  scale_x_reverse(breaks = c(0, 24, 48, 72, 96, 120, 144))+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(title = paste0("Slab avalanches (count ", num_slab, ")"), y = "", x = "number of hours", color = "station")

ggplot(plot_over)+
  geom_line(aes(x = PLOT, y = V1, color = stat), alpha = 0.3)+
  geom_smooth(aes(x = PLOT, y = V1, color = stat), method = "lm")+
  facet_wrap(~ var, scales = "free_y")+
  scale_x_reverse(breaks = c(0, 24, 48, 72, 96, 120, 144))+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(title = paste0("Cornice avalanches (count ", num_over, ")"), y = "", x = "number of hours", color = "station")

ggplot(plot_over[var == "SRA1H"])+
  geom_line(aes(x = PLOT, y = V1, color = stat), alpha = 0.3)+
  geom_smooth(aes(x = PLOT, y = V1, color = stat), method = "lm")+
  facet_wrap(variable~ var, scales = "free_y")+
  scale_x_reverse()+
  theme_bw()
