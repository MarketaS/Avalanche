# This scripts deals with the breaking points separating the cold and warm part of the snow season according to daily temperature
# Created by @Roman.Juras


# Testing several approaches
# 1. Source: An overview of change point packages in R (https://lindeloev.github.io/mcp/articles/packages.html)

# testing data set - Air temperature from Labska
library(segmented)
library(ggplot2)
library(plotly)

Meteo_4Aval[, Date_num := as.numeric(Date)] # create a column with numerical Date format (need for further analysis)
Meteo_4Aval[, breakpoint := NA] # create a column where breakpoints will be copied

# check which years are available in the dataset
years <- (unique(Meteo_4Aval[,year(Date)]))

# we are missing years 1961, 2000 and 2001, therefore we split the time series
years1 <- seq(1962, 1999, by = 1)
years2 <- seq(2002, 2020, by = 1)

# first part of dataset
for (i in 1:length(years1)) {
  print(i)

Season_temp <- Meteo_4Aval[(year(Date) == years1[i] & month(Date) > 9) | (year(Date) == years1[i+1] & (month(Date) < 6))][, c("Date","Tair")]

fit_lm = lm(Tair ~ 1 + Date, data = Season_temp)  # intercept-only model
fit_segmented = segmented(fit_lm, seg.Z = ~Date, npsi = 2)  # One change points along x

# what is the line number of the breaking point
Order_breakpoint <- which(Meteo_4Aval[,Date_num == as.numeric(round(fit_segmented$psi[2]))])
Order_breakpoint 

print(Order_breakpoint)

# put the breakpoint into original Meteo_4aval
Meteo_4Aval[Order_breakpoint, breakpoint := 1] 

}

# second part of dataset
for (i in 1:length(years2)) {
  print(i)
  Season_temp <- Meteo_4Aval[(year(Date) == years2[i] & month(Date) > 9) | (year(Date) == years2[i+1] & (month(Date) < 6))][, c("Date","Tair")]
  
  fit_lm = lm(Tair ~ 1 + Date, data = Season_temp)  # intercept-only model
  fit_segmented = segmented(fit_lm, seg.Z = ~Date, npsi = 2)  # One change points along x
  
  # what is the line number of the breaking point
  Order_breakpoint <- which(Meteo_4Aval[,Date_num == as.numeric(round(fit_segmented$psi[2]))])
  Order_breakpoint 
  
  print(Order_breakpoint)
  
  # put the breakpoint into original Meteo_4aval
  Meteo_4Aval[Order_breakpoint, breakpoint := 1] 
  
}

# View only the breakpoints over the time series
Meteo_4Aval[!is.na(breakpoint)][,c("Date","Tair", "breakpoint")]

# check and plot individual seasons
Season_temp <- Meteo_4Aval[(year(Date) == 1973 & month(Date) > 9) | (year(Date) == 1974 & (month(Date) < 6))][, c("Date","Tair")]

Season_plot <- plot_ly(Season_temp, x = ~Date, y = ~Tair, type = 'scatter', mode = 'lines')

Season_plot 

# check the segmented model for the individual season
summary(fit_segmented)

plot(fit_segmented)
points(Season_temp)
lines(Season_temp)
lines.segmented(fit_segmented)
points.segmented(fit_segmented)

