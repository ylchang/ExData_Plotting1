## To run this code:
## Step 1. Store plot1.R and the data file "household_power_comsumption.txt" in the same working directory
## Step 2. Source plot1.R
## Step 3. Run plot1()
## "plot1.png" will be generated and stored in the working directory.

plot1 <- function(){
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
  
  ##Plot the first plot and store it in "plot1.png"
  png(file = "plot1.png")
  hist(tabSub$Global_active_power, main = "Global Active Power",xlab = "Global Active Power (kilowatts)", col = "red")
  dev.off()
  
  ##Clear everything
  rm(list = ls())
}