# Plot 3: Time series of the 3 sub-metering

# Load data and filter for the specific dates
data <- read.table("household_power_consumption.txt", header = TRUE, sep = ";", 
                   na.strings = "?", stringsAsFactors = FALSE)

# Filter data for dates 2007-02-01 and 2007-02-02
data_subset <- data[data$Date %in% c("1/2/2007", "2/2/2007"), ]

# Convert Date and Time to datetime
data_subset$DateTime <- strptime(paste(data_subset$Date, data_subset$Time, sep = " "),
                                 format = "%d/%m/%Y %H:%M:%S")

# Convert sub-metering columns to numeric
data_subset$Sub_metering_1 <- as.numeric(data_subset$Sub_metering_1)
data_subset$Sub_metering_2 <- as.numeric(data_subset$Sub_metering_2)
data_subset$Sub_metering_3 <- as.numeric(data_subset$Sub_metering_3)

# Create the plot and save as PNG
png("plot3.png", width = 480, height = 480)
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
       cex = 0.8)
dev.off()
