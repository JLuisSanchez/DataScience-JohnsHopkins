# Cargar librerías necesarias
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(dplyr)) install.packages("dplyr")
if (!require(knitr)) install.packages("knitr")
library(ggplot2)
library(dplyr)
library(knitr)

# -------------------------------------------
# Descargar y cargar datos
# -------------------------------------------
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
zip_file <- "activity.zip"
csv_file <- "activity.csv"

if (!file.exists(zip_file)) {
  download.file(url, destfile = zip_file, mode = "wb")
}
if (!file.exists(csv_file)) {
  unzip(zip_file, files = csv_file)
}

activity <- read.csv(csv_file, stringsAsFactors = FALSE)
activity$date <- as.Date(activity$date)

# Mostrar estructura y resumen
str(activity)
summary(activity)

# -------------------------------------------
# Calcular total de pasos por día (ignorar NAs)
# -------------------------------------------
total_steps_per_day <- activity %>%
  group_by(date) %>%
  summarise(total_steps = sum(steps, na.rm = TRUE)) %>%
  filter(total_steps > 0) # elimina días con todos NA

# Histograma pasos totales por día
ggplot(total_steps_per_day, aes(x = total_steps)) +
  geom_histogram(binwidth = 1000, fill = "steelblue", color = "white", alpha = 0.7) +
  labs(title = "Total Steps Each Day",
       x = "Number of Steps",
       y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Media y mediana pasos por día
mean_steps <- mean(total_steps_per_day$total_steps)
median_steps <- median(total_steps_per_day$total_steps)
cat("Mean total steps per day:", round(mean_steps, 2), "\n")
cat("Median total steps per day:", round(median_steps, 2), "\n")

# -------------------------------------------
# Patrón promedio de actividad diaria
# -------------------------------------------
avg_steps_per_interval <- activity %>%
  group_by(interval) %>%
  summarise(avg_steps = mean(steps, na.rm = TRUE))

# Serie de tiempo del patrón promedio
ggplot(avg_steps_per_interval, aes(x = interval, y = avg_steps)) +
  geom_line(color = "darkblue", size = 1) +
  labs(title = "Average Daily Activity Pattern",
       x = "5-Minute Interval",
       y = "Average Steps") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(0, 2400, 400))

# Intervalo con máximo promedio de pasos
max_interval <- avg_steps_per_interval$interval[which.max(avg_steps_per_interval$avg_steps)]
max_steps <- max(avg_steps_per_interval$avg_steps)
cat("5-minute interval with maximum average steps:", max_interval, "\n")
cat("Maximum average steps:", round(max_steps, 2), "\n")
max_hour <- max_interval %/% 100
max_minute <- max_interval %% 100
cat("Time of maximum activity:", sprintf("%02d:%02d", max_hour, max_minute), "\n")

# -------------------------------------------
# Imputar valores faltantes
# -------------------------------------------
total_missing <- sum(is.na(activity$steps))
missing_percentage <- mean(is.na(activity$steps)) * 100
cat("Total number of missing values:", total_missing, "\n")
cat("Percentage of missing values:", round(missing_percentage, 1), "%\n")

# Estrategia: sustituir NA por la media del intervalo
activity_filled <- activity %>%
  group_by(interval) %>%
  mutate(steps = ifelse(is.na(steps), mean(steps, na.rm = TRUE), steps)) %>%
  ungroup()

cat("Missing values after imputation:", sum(is.na(activity_filled$steps)), "\n")

# Histograma con datos imputados
total_steps_filled <- activity_filled %>%
  group_by(date) %>%
  summarise(total_steps = sum(steps))

ggplot(total_steps_filled, aes(x = total_steps)) +
  geom_histogram(binwidth = 1000, fill = "darkgreen", color = "white", alpha = 0.7) +
  labs(title = "Total Steps Each Day (After Imputation)",
       x = "Number of Steps",
       y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Media y mediana con datos imputados
mean_steps_filled <- mean(total_steps_filled$total_steps)
median_steps_filled <- median(total_steps_filled$total_steps)
cat("Mean total steps per day (with imputation):", round(mean_steps_filled, 2), "\n")
cat("Median total steps per day (with imputation):", round(median_steps_filled, 2), "\n")
cat("Comparison:\n")
cat("Original mean:", round(mean_steps, 2), "\n")
cat("Imputed mean:", round(mean_steps_filled, 2), "\n")
cat("Difference:", round(mean_steps_filled - mean_steps, 2), "\n")
cat("Original median:", round(median_steps, 2), "\n")
cat("Imputed median:", round(median_steps_filled, 2), "\n")
cat("Difference:", round(median_steps_filled - median_steps, 2), "\n")

# -------------------------------------------
# Comparar patrones entre días laborables y fines de semana
# -------------------------------------------
activity_filled <- activity_filled %>%
  mutate(
    weekday = weekdays(date),
    day_type = factor(ifelse(weekdays(date) %in% c("Saturday", "Sunday"), "weekend", "weekday"))
  )
table(activity_filled$day_type)

# Promedio de pasos por intervalo y tipo de día
avg_steps_by_daytype <- activity_filled %>%
  group_by(interval, day_type) %>%
  summarise(avg_steps = mean(steps), .groups = "drop")

# Panel plot comparativo
ggplot(avg_steps_by_daytype, aes(x = interval, y = avg_steps)) +
  geom_line(aes(color = day_type), size = 1) +
  facet_wrap(~ day_type, nrow = 2) +
  labs(title = "Average Steps: Weekdays vs Weekends",
       x = "5-Minute Interval",
       y = "Average Steps") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none") +
  scale_x_continuous(breaks = seq(0, 2400, 400)) +
  scale_color_manual(values = c("weekday" = "blue", "weekend" = "red"))

# Estadísticas resumen por tipo de día
summary_stats <- activity_filled %>%
  group_by(day_type) %>%
  summarise(
    avg_steps_per_interval = mean(steps),
    total_observations = n(),
    .groups = "drop"
  )
print(summary_stats)

# Tiempos pico por día
peak_times <- avg_steps_by_daytype %>%
  group_by(day_type) %>%
  filter(avg_steps == max(avg_steps)) %>%
  mutate(
    hour = interval %/% 100,
    minute = interval %% 100,
    time = sprintf("%02d:%02d", hour, minute)
  )
print(peak_times[, c("day_type", "interval", "time", "avg_steps")])
