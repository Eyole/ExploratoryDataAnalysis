#Author: Lloney Eyole Monono
#Date: 29th August 2018
#
#File: plot1.R Histogram of Household Power Consumption.
#
#

library(readr)
library(sqldf)
library(lubridate)

'%&%' <<- function(x, y)paste0(x,y)


run_plotone <<- function ()
{
  
   wd <<- getwd()
  
  df.household_power_consumptionn <<- read.table(wd %&% "\\household_power_consumption.txt", sep = ";", header = TRUE ,quote="\"", comment.char = "")
  
  df.DateTime <- paste(as.character(df.household_power_consumptionn$Date), "", as.character(df.household_power_consumptionn$Time)) 
  
  df.household_power_consumptionn[, "DateTime"] <<- dmy_hms(paste(as.character(df.household_power_consumptionn$Date), "", as.character(df.household_power_consumptionn$Time)))
  
  df.hpc.subset <<- subset(df.household_power_consumptionn, DateTime >= as.Date("2007-02-01") & DateTime < as.Date("2007-02-03"))

  png(filename = wd %&% "\\plot1.png")
  
  hist(as.numeric(df.hpc.subset$Global_active_power),col = "red",xlab = "Global Actie Power (Watts)" ,main = "Histogram Global Active Power.")
  
  dev.off()
  
  }