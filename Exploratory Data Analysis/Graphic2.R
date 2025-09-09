# Plot 2: Time series of Global Active Power

# Load data and filter for the specific dates
data <- read.table("household_power_consumption.txt", header = TRUE, sep = ";", 
                   na.strings = "?", stringsAsFactors = FALSE)

# Filter data for dates 2007-02-01 and 2007-02-02
data_subset <- data[data$Date %in% c("1/2/2007", "2/2/2007"), ]

# Convert Date and Time to datetime
data_subset$DateTime <- strptime(paste(data_subset$Date, data_subset$Time, sep = " "),
                                 format = "%d/%m/%Y %H:%M:%S")

# Convert Global_active_power to numeric
data_subset$Global_active_power <- as.numeric(data_subset$Global_active_power)

# Create the plot and save as PNG
png("plot2.png", width = 480, height = 480)
plot(data_subset$DateTime, data_subset$Global_active_power, 
     type = "l", 
     xlab = "", 
     ylab = "Global Active Power (kilowatts)")
dev.off()
