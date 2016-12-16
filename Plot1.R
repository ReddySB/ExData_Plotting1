## week one Project for exploratory analysis

Plot1 <- function()
{
  
  WorkingDir <- getwd()
  Filename <- paste(WorkingDir ,"/household_power_consumption.txt" ,sep ="")
  
  # read the data from the given text file from the current directory to the table, it reads the entire data
  HouseData <- read.table(Filename, header = TRUE , sep = ";", na.strings = "?")
  
  # filter the data for the given dates
  RequiredData <- subset(HouseData , as.Date(Date,"%d/%m/%Y") >= as.Date("1/2/2007","%d/%m/%Y") & as.Date(Date,"%d/%m/%Y") < as.Date("3/2/2007","%d/%m/%Y")  )
  
  png('Plot1.png' , width = 480, height = 480 , units = "px")
  hist(RequiredData$Global_active_power,col="red" , xlab ="Global Active Power (Kilowatts)", main = "Global Active Power" )
  
  dev.off() 
  
}