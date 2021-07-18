library(data.table)
library(readr)
library(ggplot2)
library(lubridate)
install.packages("writexl")
library(writexl)

Aval <- data.table(read_delim("C:/Users/marketa.souckova/Documents/laviny/LBOU/Aval.txt", 
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
# Avalanche locality dedication#
W_aval <- c(24,25,26,27,28,29,30,31,32,33,34,35,36,37)
W_aval<-c(seq(24:37))
E_aval <- c(1,2,3,4, 5, 6, 7,8,9,10,11,12, 13,14,15,16,17,18,19,20,21,22,23,38,39)
Aval[, locality := ifelse(cadastr_number %in% W_aval, 'W', 'E')]
#data_hourly_2004_2014 <- data.table(read_delim("C:/Users/marketa.souckova/Documents/laviny/LBOU/data_hourly_2004_2014_a.txt", 
                             "\t", escape_double = FALSE, col_types = cols(D = col_number(), 
                                                                           Fmax = col_number(), Fprum = col_number(), 
                                                                           H = col_number(), RGLB1H = col_number(), 
                                                                           SCE = col_number(), SNO = col_number(), 
                                                                           SRA1H = col_number(), SSV1H = col_number(), 
                                                                           SVH = col_number(), T = col_number(), 
                                                                           T05 = col_number(), date = col_character(), 
                                                                           datum = col_character(), day = col_number(), 
                                                                           month = col_number(), ranking = col_number(), 
                                                                           year = col_number()), trim_ws = TRUE))

dta <- data.table(read_delim("C:/Users/marketa.souckova/Documents/laviny/LBOU/data_hourly_2004_2020.txt", 
                                               "\t", escape_double = FALSE, col_types = cols(D = col_double(), 
                                                                                             Fmax = col_double(), Fprum = col_double(), 
                                                                                             H = col_double(), RGLB1H = col_double(), 
                                                                                             SCE = col_double(), SNO = col_double(), 
                                                                                             SRA1H = col_double(), SSV1H = col_double(), 
                                                                                             SVH = col_double(), T = col_double(), 
                                                                                             T05 = col_double(), date = col_character(), 
                                                                                             datum = col_date(format = "%Y-%m-%d"), 
                                                                                             time = col_character()), trim_ws = TRUE))

dates <- gsub(pattern = " UTC", replacement = "", x = dta$date)
#dta$date <- NULL
dta$date <- as.POSIXct(x = dates, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

times <- dta$time
#dta$time <- NULL
dta$time <- as.POSIXct(x = times, format = "%Y-%m-%d %H:%M")

dta$DATE2 <- strtrim(x = as.character(dta$date), width = 10)# délka 10 znakù zbavení se minut a sekund
dta$DATE2 <- as.POSIXct(paste(dta$date), format = "%Y-%m-%d")
#data_hourly_2004_2020 <- as.POSIXct(paste(data_hourly_2004_2020$date), format = "%d.%m.%Y %H:%M")

Aval$DATE2 <- strtrim(x = Aval$date, width = 10)# délka 10 znakù zbavení se minut a sekund
Aval$DATE3 <- as.POSIXct(paste0(Aval$DATE2, " 07:00"), format = "%d.%m.%Y %H:%M")
Aval$DATE_OFF <- as.POSIXct(Aval$DATE3 + (17*60*60))# srovnání na 24 h od 7:00

Aval_short <- Aval[DATE3 %between% c(dta[1,DATE2] + 5*24*60*60, dta[nrow(dta),DATE2])]#dtum první laviny a meteo 5 dní pøed
boxplot (Aval_short$N)

summary (Aval_short$N)

Aval_short$ID <- paste0("Aval ", 1:nrow(Aval_short))#vložení sloupce vždy? AVal_ a s každým øádkem se bude pøipisovat èíslo


#vytvoøení seznamu, s tím že pro každou lavinu v data table Aval_short

aval_list <- list()
for (i in 1:nrow(Aval_short)){
  id <- Aval_short[i,ID]#30 sloupec Aval 1,2,3...
  start = Aval_short[i,29]#29 sloupec pro každou lavinu, datum
  stop =  Aval_short[i,29] - 5*24*60*60 #okno -5, formát Posix pracuje se sekundami
  dta_aval <- dta[DATE2 %between% c(stop[1], start[1]) , ]#vezmi øádky start, stop a pro ty mi vykresli meteo promìnné
  dta_aval$PLOT <- 1:nrow(dta_aval)# pro každou událost graf? kvùli èemu? pro lepší vykreslení dat # K ÈEMU PLOT?
  dta_aval$ID <- id # sloupec id asi definování parametru?
  aval_list[[i]] <- dta_aval
  print(i)
}
#6183 lavin 2004-2020
aval_dtafr <- rbindlist(aval_list)
#write_xlsx(aval_dtafr,"C:/Users/marketa.souckova/Documents/laviny/LBOU/aval_dtafr_2004_2020.xlsx")
aval_dtafr$ranking <- aval_dtafr$date <- aval_dtafr$day <- aval_dtafr$month <- aval_dtafr$year <- aval_dtafr$datum <-aval_dtafr$time <- NULL #vymazání nepotøebných sloupcù

aval_melt <- melt(data = aval_dtafr, id.vars = c("ID", "DATE2", "PLOT"))#
aval_melt[ , station:="LBOU"]
saveRDS(object = aval_melt, file = "C:/Users/marketa.souckova/Documents/laviny/aval_melt.rds")
#write_xlsx(aval_melt,"C:/Users/marketa.souckova/Documents/laviny/LBOU/aval_melt_2004_2020.xlsx")
unique(aval_melt$variable)
ggplot(aval_melt)+
  geom_line(aes(x = PLOT, y = value), col = "red", alpha = 0.5)+
  facet_wrap(~ variable, scales = "free_y")+
  scale_x_reverse()
#co dìlá scales
#wraps a 1d sequence of panels into 2d. This is generally a better use of screen space than facet_grid() because most displays are roughly rectangular.
target<- Aval_short [C == 2 & N > 100, ID]
#target <- Aval_short[C == 2 & N > 300 & cadastr_number %in% c(24,	25,	26,	27,	28,	29,	30,	31,	32,	33,	34,	35,	36,	37), ID]       
#target <- Aval_short[C == 2 & cadastr_number %in% c(24,	25,	26,	27,	28,	29,	30,	31,	32,	33,	34,	35,	36,	37), ID]   
#laviny dle vlhkosti snìhu C2 = mokrý sníh
#target <- Aval_short[cadastr_number %in% c(24,	25,	26,	27,	28,	29,	30,	31,	32,	33,	34,	35,	36,	37), ID]
#target <- Aval_short[L>20, ID]#šíøka odtrhu vìtší než 20 m
target <- Aval_short[A == 3, ID]
#laviny dle tvaru dráhy D1 plošná lavina
#target2 <- Aval_short[A %in% c(3,4) & B == 1, ID] #laviny dle formy odtrhu A3,4 desková (mìkká, tvrdá lavina)
target <- Aval_short[A %in% c(3,4),ID] #laviny dle formy odtrhu A3, A4 - desková lavina, tvrdá, mìkká

#vykreslení png každé lavinové situace
for (i in 1:length(target)){
  name <- target[i]
  #png(filename = paste0("C:/Users/marketa.souckova/Documents/laviny/LBOU/humid/", name, ".png"), width = 1500, height = 1000, units = "px")
  #png(filename = paste0("C:/Users/marketa.souckova/Documents/laviny/LBOU/humid/", name, ".png"), width = 1500, height = 1000, units = "px")
  ggplot(aval_melt[ID %in% name])+    #target mohu libovolnì mìnit 
     geom_line(aes(x = PLOT, y = value))+
    facet_wrap(~ variable, scales = "free_y")+
    theme(legend.position = "top")+
    scale_x_reverse()
  
  ggsave(filename = paste0("C:/Users/marketa.souckova/Documents/laviny/LBOU/A3_slab_soft/", name, ".png"), device = "png", dpi = 200)
  #ggsave(filename = paste0("C:/Users/marketa.souckova/Documents/laviny/LBOU/A5_overhanging/", name, ".png"), device = "png", dpi = 200)
  #ggsave(filename = paste0("C:/Users/marketa.souckova/Documents/laviny/LBOU/humid_all/", name, ".png"), device = "png", dpi = 200)
  #dev.off()
}

#všechny mokré laviny z LBOU
ggplot(aval_melt[ID %in% target])+    #target mohu libovolnì mìnit 
  geom_line(aes(x = PLOT, y = value, color = ID), alpha = 0.5)+
  facet_wrap(~ variable, scales = "free_y")+
  theme(legend.position = "none")+ # jak bude vypadat plot, bez názvu legendy
  scale_x_reverse()

sub_dta <- aval_melt[ID %in% target]# výbìr pouze mokrých lavin, šíøka odtrhu vìtší než 20 m, jedineèný ID v promìnné target

sub_dta2 <- sub_dta[,mean(value, na.rm = T), by = .(PLOT, variable)]#prùmìr všech hodnot mokrých lavin, jak stejnì vytvoøím pro nelaviny, event = 0? 
#LBOU laviny 24	25	26	27	28	29	30	31	32	32A	33	34	35	36	36A	36B	37	37A
#E %in% c(24,	25,	26	27	28	29	30	31	32	32A	33	34	35	36	36A	36B	37	37A)
#stanice 
##prùmìr všech hodnot vybraných lavin, proložená linie 5 dní pøed
ggplot(sub_dta2)+
  geom_line(aes(x = PLOT, y = V1, color = Station))+
  geom_smooth(aes(x = PLOT, y = V1), method = "lm")+
  #scale_color_manual(values = c("red", "blue"))+
  facet_wrap(~ variable, scales = "free_y")+
  scale_x_reverse()
saveRDS(object = sub_dta2, file = "C:/Users/marketa.souckova/Documents/laviny/LBOU_cumulative_variables.rds")

#load("cumulative_variables.RData")

#uložení každého grafu - splnìno
#uložení do jednotlivých složek - splnìno
