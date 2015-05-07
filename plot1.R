### Run_Plot1 reads power usage information from a delimited file and analysis two days worth of usage and 
### creates chart.

run_plot1 <- function()
{
        ##Read from file 
        data <- fread("./household_power_consumption.txt", sep=";", skip="1/2/2007", 
                     nrow=2881)
        
        
        columnNames  <- c("Date","Time","Global_active_power",
                          "Global_reactive_power","Voltage","Global_intensity",
                          "Sub_metering_1","Sub_metering_2","Sub_metering_3")
        
       
        ## rename columns to meaningful.
        setnames(data, old=c("V1","V2","V3","V4","V5","V6","V7","V8","V9"), new=columnNames)
        
        ## add additional columns 
        data <- cbind(data, dmy(data$Date),  strptime(stri_join(data$Date," ", data$Time),  format="%d/%m/%Y %H:%M:%S"))
        
        ## rename newly added columns.
        setnames(data, old=c("V2","V3"), new=c("dt","datetime"))
        
        ## set no of charts 
        par(mfrow=c(1,1))
        
        ## generate histogram as per request.
        hist(data$Global_active_power, col="red", main="Global Active Power", xlab="Global Active Power (kilowatts)")
        
        ## transfer to a PNG file.
        dev.copy(png, file="./plot1.png")
        
        ## close the device.
        dev.off()



}
