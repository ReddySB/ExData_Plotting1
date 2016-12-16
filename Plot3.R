
Plot3 <- function()
{
  
  WorkingDir <- getwd()
  Filename <- paste(WorkingDir ,"/household_power_consumption.txt" ,sep ="")
  
  # read the data from the given text file from the current directory to the table, it reads the entire data
  HouseData <- read.table(Filename, header = TRUE , sep = ";", na.strings = "?")
  
  # filter the data for the given dates
  RequiredData <- subset(HouseData , as.Date(Date,"%d/%m/%Y") >= as.Date("1/2/2007","%d/%m/%Y") & as.Date(Date,"%d/%m/%Y") < as.Date("3/2/2007","%d/%m/%Y")  )
  
  # concatinate the data and time into to a single variable
  RequiredData$newtime <- as.POSIXct(paste(RequiredData$Date, RequiredData$Time), format= "%d/%m/%Y %H:%M:%S")
  daterange=c(as.POSIXlt(min(RequiredData$newtime)), as.POSIXlt(max(RequiredData$newtime)))
  daterange[2]$hour <- daterange[2]$hour + 1 
  # save the chart to png file in the current directory 
  png('Plot3.png' , width = 480, height = 480 , units = "px")
  
  # Plot three chats for different sub metering values
  plot(RequiredData$Sub_metering_1 ~ RequiredData$newtime,  xaxt="n" ,type = "l" , xlab ="" , ylab="Energy sub metering")
  lines(RequiredData$Sub_metering_2 ~ RequiredData$newtime , col ="red")
  lines(RequiredData$Sub_metering_3 ~ RequiredData$newtime , col ="blue")
  
  #Set the x - axis with day frequency
  axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="day"), format="%a")
  
  #set the legend
  legend("topright" , c("Sub_metering_1", "Sub_metering_2","Sub_metering_3 ") , lty=c(1,1), col = c("black", "red", "blue") )
  
  #set the screen off
  dev.off() 

  }
