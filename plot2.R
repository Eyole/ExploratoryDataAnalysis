#Author: Lloney Eyole Monono
#Date: 29th August 2018
#
#File: plot2.R Plot of Global Active Power Vs DateTime.
#
#

library(readr)
library(sqldf)
library(lubridate)

'%&%' <<- function(x, y)paste0(x,y)


run_plottwo <<- function ()
{
  
  wd <<- getwd()
  
  df.household_power_consumptionn <<- read.table(wd %&% "\\household_power_consumption.txt", sep = ";", header = TRUE ,quote="\"", comment.char = "")
  
  df.DateTime <- paste(as.character(df.household_power_consumptionn$Date), "", as.character(df.household_power_consumptionn$Time)) 
  
  df.household_power_consumptionn[, "DateTime"] <<- dmy_hms(paste(as.character(df.household_power_consumptionn$Date), "", as.character(df.household_power_consumptionn$Time)))
  
  df.hpc.subset <<- subset(df.household_power_consumptionn, DateTime >= as.Date("2007-02-01") & DateTime < as.Date("2007-02-03"))

  df.hpc.subset[,"Day"] <<- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", 
                              "Friday", "Saturday")[unclass(as.POSIXlt(df.hpc.subset$DateTime))$wday +1]
  df.hpc.subset[,"Hour"] <<- unclass(as.POSIXlt(df.hpc.subset$DateTime))$hour
  
  png(filename = wd %&% "\\plot2.png")
  
  with(df.hpc.subset, plot(rownames(df.hpc.subset), as.numeric(df.hpc.subset$Global_active_power), xaxt = "n", type = "l", xlab ="Date & Time", ylab = "Global Active Power"))
  
  lablist <- as.vector(c("Thursday","Friday","Saturday"))
                       
  axis(1,at=c(66620,68060,69500),labels = c("Thursday","Friday","Saturday"), col.ticks = "red")
  
  mtext("Global Active Power Vs Date & Time", side=3, outer=TRUE, line=-3)
  
  dev.off()
  

  }