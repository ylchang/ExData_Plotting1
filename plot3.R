## To run this code:
## Step 1. Store plot3.R and the data file "household_power_comsumption.txt" in the same working directory
## Step 2. Source plot3.R
## Step 3. Run plot3()
## "plot3.png" will be generated and stored in the working directory.

plot3 <- function(){
  ##Read the data file. NOTE: It may take a while since the data file is huge!!
  initial <- read.table("household_power_consumption.txt", header = TRUE, sep = ";", na.strings = "?", nrows = 100)
  classes <- sapply(initial, class)
  tabAll <- read.table("household_power_consumption.txt", header = TRUE, sep = ";", na.strings = "?",colClasses = classes)
  
  ##Combine column "Date" and column "Time" into one vector "datetime"
  datetime <-paste(tabAll$Date, tabAll$Time, sep = " ")
  
  ##Convert "datetime" to Date and store it in column "Date"
  tabAll$Date <- strptime(datetime, "%d/%m/%Y %H:%M:%S")
  
  ##Remove the redundent column "Time"
  tabAll <- subset(tabAll, select = -Time )
  
  ##Put 2007-02-01 and 2007-02-02 data in a new dataframe "tabSub". After that, remove the 
  ##original data "tabAll" to free up the memory
  tabSub = subset(tabAll, as.Date(Date) >= '2007-02-01' & as.Date(Date) <= '2007-02-02') 
  rm(tabAll)
  
  ##Create a column "wday" to store the week day associate to the "Date"
  wday <- weekdays(as.Date(tabSub$Date))
  tabSub <- cbind(tabSub,wday)
  
  ##Plot the third plot and store it in "plot3.png"
  png(file = "plot3.png")
  with(tabSub, plot(Date, Sub_metering_1, ylab = "Energy sub metering", type = "n"))
  with(tabSub, lines(Date, Sub_metering_1, col = "black"))
  with(tabSub, lines(Date, Sub_metering_2, col = "red"))
  with(tabSub, lines(Date, Sub_metering_3, col = "blue"))
  legend("topright", lwd=1, lty=1, col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  dev.off()
  
  ##Clear everything
  rm(list = ls())
}