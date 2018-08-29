#Author: Lloney Eyole Monono
#Date: 29th August 2018
#
#File: plot4.R Four Plots: Global Active Power, Voltage, Submetering & Global Reactive Power
#
#

library(readr)
library(sqldf)
library(lubridate)

'%&%' <<- function(x, y)paste0(x,y)


run_plotfour <<- function ()
{
  
  wd <<- getwd()
  
  df.household_power_consumptionn <<- read.table(wd %&% "\\household_power_consumption.txt", sep = ";", header = TRUE ,quote="\"", comment.char = "")
  
  df.DateTime <- paste(as.character(df.household_power_consumptionn$Date), "", as.character(df.household_power_consumptionn$Time)) 
  
  df.household_power_consumptionn[, "DateTime"] <<- dmy_hms(paste(as.character(df.household_power_consumptionn$Date), "", as.character(df.household_power_consumptionn$Time)))
  
  df.hpc.subset <<- subset(df.household_power_consumptionn, DateTime >= as.Date("2007-02-01") & DateTime < as.Date("2007-02-03"))

  df.hpc.subset[,"Day"] <<- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", 
                              "Friday", "Saturday")[unclass(as.POSIXlt(df.hpc.subset$DateTime))$wday +1]
  df.hpc.subset[,"Hour"] <<- unclass(as.POSIXlt(df.hpc.subset$DateTime))$hour
  
  png(filename = wd %&% "\\plot4.png")
  
  par(mfrow = c(2,2))
  
  with(df.hpc.subset, plot(rownames(df.hpc.subset), as.numeric(df.hpc.subset$Global_active_power), xaxt = "n", type = "l", xlab ="Date & Time", ylab = "Global Active Power"))
  
  lablist <- as.vector(c("Thursday","Friday","Saturday"))
  
  axis(1,at=c(66620,68060,69500),labels = c("Thursday","Friday","Saturday"), col.ticks = "red")
  
  with(df.hpc.subset, plot(rownames(df.hpc.subset), as.numeric(df.hpc.subset$Voltage), xaxt = "n", type = "l",  xlab ="Date & Time", ylab = "Voltage"))
  
  lablist <- as.vector(c("Thursday","Friday","Saturday"))
  
  axis(1,at=c(66620,68060,69500),labels = c("Thursday","Friday","Saturday"), col.ticks = "red")
  
  plot(range(rownames(df.hpc.subset)), range(as.numeric(df.hpc.subset$Sub_metering_1)), xaxt = "n",type="n", xlab ="Date & Time", ylab = "Energy Sub Metering")
  
  lines(rownames(df.hpc.subset), as.numeric(df.hpc.subset$Sub_metering_1), type = "l", lwd=1.5, col = "black")
  
  lines(rownames(df.hpc.subset), as.numeric(df.hpc.subset$Sub_metering_2), type = "l", lwd=1.5, col = "red")
  
  lines(rownames(df.hpc.subset), as.numeric(df.hpc.subset$Sub_metering_3), type = "l", lwd=1.5, col = "blue")
  
  lablist <- as.vector(c("Thursday","Friday","Saturday"))
  
  axis(1,at=c(66620,68060,69500),labels = c("Thursday","Friday","Saturday"), col.ticks = "red")
  
  with(df.hpc.subset, plot(rownames(df.hpc.subset), as.numeric(df.hpc.subset$Global_reactive_power), xaxt = "n", xlab ="Date & Time", ylab = "Global Reactive Power", type = "l"))
  
  lablist <- as.vector(c("Thursday","Friday","Saturday"))
  
  axis(1,at=c(66620,68060,69500),labels = c("Thursday","Friday","Saturday"), col.ticks = "red")
  
  mtext("Global Active Power, Voltage, Sub Metering & Global Reactive Power, Vs Date & Time", side=3, outer=TRUE, line=-3)
  
  dev.off()

  }