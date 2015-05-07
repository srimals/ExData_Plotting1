### Run_Plot3 reads power usage information from a delimited file and analysis two days worth of usage and 
### creates chart.

run_plot3 <- function()
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
        par(mar=c(2,3,1,1))
        
        ##set colors for each metering
        plot_colors <- c("black","red","blue")
        
        
        ## SET DeVICE TO PNG 
        png(filename = "./plot3.png", width = 480, height = 480,
            units = "px", pointsize = 12, bg = "white", res = NA)
        ##    restoreConsole = TRUE)
        
        ## generate empty plot first.
        plot(data$datetime, data$Sub_metering_1 , type="n", 
             ylab="Energy sub metering", xlab="" ) 
        
        ##generate lines on the plot diagram.
        lines(data$datetime, data$Sub_metering_1)
        lines(data$datetime, data$Sub_metering_2, col="red")
        lines(data$datetime, data$Sub_metering_3, col="blue")
        
        
        ##xcoords <- c(0, 10, 30)
        ##secondvector <- (1:3)-1
        ##textwidths <- xcoords/secondvector # this works for all but the first element
        ##textwidths[1] <- 0
        
        ## generate legend on topright.
        legend("topright", legend=c("Sub_metering_1", "Sub_metering_2","Sub_metering_3"), col=plot_colors, lwd=1,
               cex=0.8) ##, pt.cex=1)
        
        ## transfer to a PNG file.
        ##dev.copy(png, file="./plot3.png")
        
        ## close the device.
        
        dev.off()
        
        
        
}
