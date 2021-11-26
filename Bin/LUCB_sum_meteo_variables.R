library(data.table)
library(readr)
library(ggplot2)
library(lubridate)
install.packages("writexl")
library(writexl)
library (dplyr)

Aval <- data.table(read_delim("C:/Users/marketa.souckova/Documents/laviny/LBOU/Aval.txt", 
                              "\t", escape_double = FALSE, col_types = cols(A = col_character(), 
                                                                            B = col_character(), C = col_character(), 
                                                                            D = col_character(), E = col_character(), 
                                                                            F = col_character(), G = col_character(), 
                                                                            H = col_character(), J = col_character(),
                                                                            K = col_number(), L = col_number(), 
                                                                            M = col_number(), N = col_number(), 
                                                                            O = col_character(), cadastr_num_let = col_character(), 
                                                                            cadastr_number = col_number(), date = col_character(), 
                                                                            day = col_number(), event = col_character(), 
                                                                            exposure = col_character(), locality = col_character(), 
                                                                            month = col_number(), notes = col_character(), 
                                                                            ranking = col_number(), season = col_character(), 
                                                                            year = col_number()), trim_ws = TRUE))
dta_LUCB <- data.table(read_delim("C:/Users/marketa.souckova/Documents/laviny/LBOU/LUCB_dta_hourly_2004_2020.txt", 
                             "\t", escape_double = FALSE, col_types = cols(datum = col_date(format = "%Y-%m-%d"),SCE = col_double(), SNO = col_double(), SVH = col_double(),
                                                                           H = col_double(), D = col_double(),
                                                                           Fprum = col_double(), Fmax = col_double(), 
                                                                           T = col_double(), SRA1H = col_double(), SSV1H = col_double(), 
                                                                           T05 = col_double(), TPM = col_double(), 
                                                                           date = col_character()), 
                                                                          trim_ws = TRUE))
dates <- gsub(pattern = " UTC", replacement = "", x = dta_LUCB$date)
#dta_LUCB$date <- NULL
dta_LUCB$date <- as.POSIXct(x = dates, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

times <- dta_LUCB$time
#dta_LUCB$time <- NULL
dta_LUCB$time <- as.POSIXct(x = times, format = "%Y-%m-%d %H:%M")

dta_LUCB$DATE2 <- strtrim(x = as.character(dta_LUCB$date), width = 10)# d?lka 10 znak? zbaven? se minut a sekund
dta_LUCB$DATE2 <- as.POSIXct(paste(dta_LUCB$date), format = "%Y-%m-%d")
#data_hourly_2004_2020 <- as.POSIXct(paste(data_hourly_2004_2020$date), format = "%d.%m.%Y %H:%M")

Aval$DATE2 <- strtrim(x = Aval$date, width = 10)# d?lka 10 znak? zbaven? se minut a sekund
Aval$DATE3 <- as.POSIXct(paste0(Aval$DATE2, " 07:00"), format = "%d.%m.%Y %H:%M")
Aval$DATE_OFF <- as.POSIXct(Aval$DATE3 + (17*60*60))# srovn?n? na 24 h od 7:00

Aval_short <- Aval[DATE3 %between% c(dta_LUCB[1,DATE2] + 5*24*60*60, dta_LUCB[nrow(dta_LUCB),DATE2])]#dtum prvn? laviny a meteo 5 dn? p?ed
boxplot (Aval_short$N)
summary (Aval_short$N)

Aval_short$ID <- paste0("Aval ", 1:nrow(Aval_short))#vlo?en? sloupce v?dy? AVal_ a s ka?d?m ??dkem se bude p?ipisovat ??slo
#Ameteo<-merge(Aval[,.(date, DATE2, event)], dta_LUCB, by ="date")???


#vytvo?en? seznamu, s t?m ?e pro ka?dou lavinu v data table Aval_short

aval_list <- list()
for (i in 1:nrow(Aval_short)){
  id <- Aval_short[i,30]#30 sloupec Aval 1,2,3...
  start = Aval_short[i,29]#29 sloupec pro ka?dou lavinu, datum
  stop =  Aval_short[i,29] - 5*24*60*60 #okno -5, form?t Posix pracuje se sekundami
  dta_LUCB_aval <- dta_LUCB[DATE2 %between% c(stop[1], start[1]) , ]#vezmi ??dky start, stop a pro ty mi vykresli meteo prom?nn?
  dta_LUCB_aval$PLOT <- 1:nrow(dta_LUCB_aval)# pro ka?dou ud?lost graf? kv?li ?emu? pro lep?? vykreslen? dat # K ?EMU PLOT?
  dta_LUCB_aval$ID <- id # sloupec id asi definov?n? parametru?
  aval_list[[i]] <- dta_LUCB_aval
  print(i)
}

#4159 lavin 2009-2020
aval_dta_LUCBfr <- rbindlist(aval_list)

aval_dta_LUCBfr$ranking <- aval_dta_LUCBfr$date <- aval_dta_LUCBfr$day <- aval_dta_LUCBfr$month <- aval_dta_LUCBfr$year <- aval_dta_LUCBfr$datum <-aval_dta_LUCBfr$time <- NULL #vymaz?n? nepot?ebn?ch sloupc?

aval_melt_LUCB <- melt(data = aval_dta_LUCBfr, id.vars = c("ID", "DATE2", "PLOT"))#
aval_melt_LUCB[ , station:="LUCB"]
saveRDS(object = aval_melt_LUCB, file = "C:/Users/marketa.souckova/Documents/laviny/aval_melt_LUCB.rds")
#write_xlsx(aval_melt,"C:/Users/marketa.souckova/Documents/laviny/LUCB/aval_melt_2004_2020.xlsx")
unique(aval_melt$variable)
ggplot(aval_melt)+
  geom_line(aes(x = PLOT, y = value), col = "red", alpha = 0.5)+
  facet_wrap(~ variable, scales = "free_y")+
  scale_x_reverse()

#wraps a 1d sequence of panels into 2d. This is generally a better use of screen space than facet_grid() because most displays are roughly rectangular.
target<- Aval_short [C == 2 & N > 100, ID]
#target <- Aval_short[C == 2 & N > 300 & cadastr_number %in% c(1,"01A",2,3,4,5,6, "06A",	"06B",	"06C",7,8,9,10,11,"11A","11B",12,13,"13A","13B","13C",14,15,16,17,18,"18A","18B",19,"19A",20,21,22,23,38,39,"19B","07A","16A","17A","16B","16B"), ID] 
target <- Aval_short[C == 2 & cadastr_num_let %in% c("1","01A","2","3","4","5","6","06A","06B","06C","7","8","9","10","11","11A","11B","12","13","13A","13B","13C","14","15","16","17","18","18A","18B","19","19A","20","21","22","23","38","39","19B","07A","16A","17A","16B"), ID] 
str(Aval_short$cadastr_num_let)
#laviny dle vlhkosti sn?hu C2 = mokr? sn?h
#target <- Aval_short[cadastr_number %in% c(24,	25,	26,	27,	28,	29,	30,	31,	32,	33,	34,	35,	36,	37), ID]
str(aval_short$cadastr_number)
#vykreslen? png ka?d? lavinov? situace
for (i in 1:length(target)){
  name <- target[i]
  #png(filename = paste0("C:/Users/marketa.souckova/Documents/laviny/LUCB/humid/", name, ".png"), width = 1500, height = 1000, units = "px")
  
  ggplot(aval_melt[ID %in% name])+    #target mohu libovoln? m?nit 
    geom_line(aes(x = PLOT, y = value))+
    facet_wrap(~ variable, scales = "free_y")+
    theme(legend.position = "top")+
    scale_x_reverse()
  
  geom_text
  
  #ggsave(filename = paste0("C:/Users/marketa.souckova/Documents/laviny/LUCB/humid_N100/", name, ".png"), device = "png", dpi = 200)
  
  ggsave(filename = paste0("C:/Users/marketa.souckova/Documents/laviny/LUCB/humid_east_path_all/", name, ".png"), device = "png", dpi = 200)
  #dev.off()
}

#v?echny mokr? laviny z LUCB
ggplot(aval_melt[ID %in% target])+    #target mohu libovoln? m?nit 
  geom_line(aes(x = PLOT, y = value, color = ID), alpha = 0.5)+
  facet_wrap(~ variable, scales = "free_y")+
  theme(legend.position = "none")+
  scale_x_reverse()# jak bude vypadat plot, bez n?zvu legendy
LUCB_LBOU <-rbind(aval_melt,aval_melt_LUCB)
#sub_dta_LUCB <- aval_melt_LUCB[ID %in% target]# v?b?r pouze mokr?ch lavin, ???ka odtrhu v?t?? ne? 20 m, jedine?n? ID v prom?nn? target
sub_dta_LUCB_LBOU <- LUCB_LBOU[ID %in% target]
sub_dta2_LUCB <- sub_dta_LBOU[,mean(value, na.rm = T), by = .(PLOT, variable)]#pr?m?r v?ech hodnot mokr?ch lavin, jak stejn? vytvo??m pro nelaviny, event = 0? 

sub_dta2_LUCB_LBOU <- sub_dta_LUCB_LBOU[,mean(value, na.rm = T), by = .(PLOT, variable, station)]
##pr?m?r v?ech hodnot vybran?ch lavin, prolo?en? linie 5 dn? p?ed
ggplot(sub_dta2_LUCB_LBOU)+
  geom_line(aes(x = PLOT, y = V1, color = station, col= "red, blue"))+
  #geom_smooth(aes(x = PLOT, y = V1), method = "lm")+
  #scale_color_manual(values = c("red", "blue"))+
  facet_wrap(~ variable, scales = "free_y")+
  scale_x_reverse()
saveRDS(object = sub_dta2_LUCB, file = "C:/Users/marketa.souckova/Documents/laviny/sub_dta2_LUCB.rds")

