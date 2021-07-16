dir <- "C:/Users/Marketa/Documents/CZU/R/Krkonose" # odkud se berou data

setwd (dir)

#Laviny <- fread("Aval_event_2021.txt", header = T, sep = ",")

install.packages('writexl')
library(writexl)
library(readxl)
library(data.table)
library(readr)
Aval<- read_xlsx("Aval_event_2021_event1.xlsx", sheet = 1, skip = 0, na ="", col_types = c(rep('guess',10), rep('numeric',19), rep('guess',4)))
str(Aval)
Aval <- data.table(Aval)
class(Aval)

date <- seq(as.Date("1961-01-01"), as.Date("2021-12-31"), by = "day")
#strptime(paste0( year, month , day), format = '%Y%m%d')
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
write.csv(Avalanche_event, "C:/Users/Marketa/Documents/CZU/R/Krkonose/Avalanche_event_1961_2021.csv")
save.image(file = "avalanche.RData")
load("avalanche.RData")
#Avalanche_event<- Avalanche_event[!is.na(date),]

na.rm = TRUE

#Fmax[, code := 2]



           
           