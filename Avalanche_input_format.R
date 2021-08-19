dir <- "C:/Users/Marketa/Documents/CZU/R/Krkonose/data/" # odkud se berou data


install.packages('writexl')
library(writexl)
library(readxl)
library(data.table)
library(readr)

library(lubridate)
library(writexl)
library(dplyr)  
library(zoo)

Aval <- data.table(read_delim("./data/Aval_utf_8.txt", 
                              "\t", escape_double = FALSE, 
                              col_types = cols(A = col_character(), 
                                               B = col_character(), C = col_character(), 
                                               D = col_character(), E = col_character(), 
                                               F = col_character(), G = col_character(), 
                                               H = col_character(), J = col_character(),
                                               K = col_number(), L = col_number(), 
                                               M = col_number(), N = col_number(), 
                                               O = col_character(), cadastr_letter = col_character(), 
                                               cadastr_number = col_number(), date = col_character(), 
                                               day = col_number(), event = col_number(), 
                                               exposure = col_character(), locality = col_character(), 
                                               month = col_number(), notes = col_character(), 
                                               ranking = col_number(), season = col_character(), 
                                               year = col_number()), trim_ws = TRUE))
Aval<- read_xlsx("C:/Users/Marketa/Documents/CZU/R/Krkonose/data/Aval_event_2021.xlsx", sheet = 2, skip = 0, na ="", col_types = c(rep('guess',10), rep('numeric',16), rep('guess',1)))
str(Aval)
Aval <- data.table(Aval)
class(Aval)

date <- seq(as.Date("1961-01-01"), as.Date("2021-12-31"), by = "day")

date <-seq(as.POSIXct("1961-01-01"), as.POSIXct("2021-12-31"), by="day")
DT <- data.table(date)

str(DT)

Avalanche_event<-do.call("merge", c(lapply(list(DT, Aval), data.frame, row.names=NULL), 
                   by = date, all = TRUE))

Avalanche_event <- data.table(Avalanche_event)
class (Avalanche_event)
event <- Avalanche_event [, event := ifelse(is.na(year),0,1)]
event <- Avalanche_event [, event_text := ifelse(is.na(day),"NonAv","Aval")]

Avalanche_event$date = as.Date(Avalanche_event$date, format = '%Y-%m-%d')
write.csv(Avalanche_event, "C:/Users/Marketa/Documents/CZU/R/Krkonose/data/Avalanche_event_1961_2021.csv")


saveRDS(object = Avalanche_event, file = "data/Avalanche_event.rds")
Avalanche_event <- readRDS(file = "data/Avalanche_event.rds")



           
           