### Run_Plot4 reads power usage information from a delimited file and analysis two days worth of usage and 
### creates chart.

run_plot4 <- function()
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
        
        ## SET DeVICE TO PNG 
        png(filename = "./plot4.png", width = 480, height = 480,
            units = "px", pointsize = 12, bg = "white", res = NA)
        
        par(mfrow = c(2,2))
        par(mar=c(4,4,2,2))
        
        ### DEFINE AND DRAW TOP LEFT PLOT.
        plot(data$datetime, data$Global_active_power, type="n", 
             ylab="Global Active Power", xlab="") 
        
        lines(data$datetime, data$Global_active_power)
        
        
        
        
        
        ### DEFINE AND DRAW TOP RIGHT PLOT.
        plot(data$datetime, data$Voltage, type="n", 
             ylab="Voltage", xlab="datetime") 
        lines(data$datetime, data$Voltage)
        ###
        
        
        
        
        
        ##set colors for each metering
        plot_colors <- c("black","red","blue")
        ### DEFINE AND DRAW BOTTOM LEFT PLOT.
        plot(data$datetime, data$Sub_metering_1 , type="n", 
             ylab="Energy sub metering", xlab="" ) 
        
        ##generate lines on the plot diagram.
        lines(data$datetime, data$Sub_metering_1)
        lines(data$datetime, data$Sub_metering_2, col="red")
        lines(data$datetime, data$Sub_metering_3, col="blue")
        
        ## generate legend on topright.
        legend("topright", legend=c("Sub_metering_1", "Sub_metering_2","Sub_metering_3"), col=plot_colors, lwd=2, bty="n")
        
        
        
        
        
        ### DEFINE AND DRAW BOTTOM RIGHT PLOT.
        plot(data$datetime, data$Global_reactive_power, type="n", 
             xlab="datetime", ylab="Global_reactive_power") 
        lines(data$datetime, data$Global_reactive_power)
        
        ## transfer to a PNG file.
        ##dev.copy(png, file="./plot4.png")
        
        ## close the device.
        
        dev.off()
        
        
        
}
