# Plot 4: Multiple plots panel

# Load data and filter for the specific dates
data <- read.table("household_power_consumption.txt", header = TRUE, sep = ";", 
                   na.strings = "?", stringsAsFactors = FALSE)

# Filter data for dates 2007-02-01 and 2007-02-02
data_subset <- data[data$Date %in% c("1/2/2007", "2/2/2007"), ]

# Convert Date and Time to datetime
data_subset$DateTime <- strptime(paste(data_subset$Date, data_subset$Time, sep = " "),
                                 format = "%d/%m/%Y %H:%M:%S")

# Convert numeric columns
data_subset$Global_active_power <- as.numeric(data_subset$Global_active_power)
data_subset$Global_reactive_power <- as.numeric(data_subset$Global_reactive_power)
data_subset$Voltage <- as.numeric(data_subset$Voltage)
data_subset$Sub_metering_1 <- as.numeric(data_subset$Sub_metering_1)
data_subset$Sub_metering_2 <- as.numeric(data_subset$Sub_metering_2)
data_subset$Sub_metering_3 <- as.numeric(data_subset$Sub_metering_3)

# Create the plot and save as PNG
png("plot4.png", width = 480, height = 480)
par(mfrow = c(2, 2))

# Top left: Global Active Power
plot(data_subset$DateTime, data_subset$Global_active_power, 
     type = "l", 
     xlab = "", 
     ylab = "Global Active Power")

# Top right: Voltage
plot(data_subset$DateTime, data_subset$Voltage, 
     type = "l", 
     xlab = "datetime", 
     ylab = "Voltage")

# Bottom left: Sub metering
plot(data_subset$DateTime, data_subset$Sub_metering_1, 
     type = "l", 
     col = "black",
     xlab = "", 
     ylab = "Energy sub metering")
lines(data_subset$DateTime, data_subset$Sub_metering_2, col = "red")
lines(data_subset$DateTime, data_subset$Sub_metering_3, col = "blue")
legend("topright", 
       legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
       col = c("black", "red", "blue"), 
       lty = 1, 
       cex = 0.5)

# Bottom right: Global Reactive Power
plot(data_subset$DateTime, data_subset$Global_reactive_power, 
     type = "l", 
     xlab = "datetime", 
     ylab = "Global_reactive_power")

dev.off()
